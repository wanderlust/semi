;;; mime-mc.el --- Mailcrypt interface for SEMI

;; Copyright (C) 1996,1997,1998,1999 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         Katsumi Yamaoka  <yamaoka@jpl.org>
;; Keywords: PGP, GnuPG, security, MIME, multimedia, mail, news

;; This file is part of SEMI (Secure Emacs MIME Interface).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'semi-def)
(require 'mailcrypt)

(eval-and-compile
  (mapcar
   (function (lambda (elem) (apply 'autoload elem)))
   '(
     (mc-gpg-debug-print	"mc-gpg")
     (mc-gpg-encrypt-region	"mc-gpg")
     (mc-gpg-lookup-key		"mc-gpg")
     (mc-pgp50-encrypt-region	"mc-pgp5")
     (mc-pgp50-lookup-key	"mc-pgp5")
     (mc-pgp-encrypt-region	"mc-pgp")
     (mc-pgp-lookup-key		"mc-pgp")
     (mc-snarf-keys		"mc-toplev")
     )))

(defvar mc-gpg-comment)
(defvar mc-gpg-extra-args)
(defvar mc-gpg-path)
(defvar mc-gpg-user-id)
(defvar mc-pgp50-comment)
(defvar mc-pgp50-pgps-path)
(defvar mc-pgp50-user-id)
(defvar mc-pgp-comment)
(defvar mc-pgp-path)
(defvar mc-pgp-user-id)


;;; @ Generic functions
;;;

(defun mime-mc-insert-public-key (&optional userid scheme)
  (mc-insert-public-key
   userid
   (or scheme (intern (format "mc-scheme-%s" pgp-version)))
   ))

(defun mime-mc-verify ()
  (let ((mc-default-scheme (intern (format "mc-scheme-%s" pgp-version))))
    (mc-verify)
    ))

(defun mime-mc-decrypt ()
  (let ((mc-default-scheme (intern (format "mc-scheme-%s" pgp-version))))
    (mc-decrypt)
    ))

(defun mime-mc-snarf-keys ()
  (let ((mc-default-scheme (intern (format "mc-scheme-%s" pgp-version))))
    (mc-snarf-keys)
    ))


;;; @ GnuPG functions
;;;

(defun mime-mc-gpg-process-region
  (beg end passwd program args parser bufferdummy boundary)
  (let ((obuf (current-buffer))
	(process-connection-type nil)
	(shell-file-name "/bin/sh") ;; ??? force? need sh (not tcsh) for "2>"
	; other local vars
	mybuf
	stderr-tempfilename stderr-buf
	status-tempfilename status-buf
	proc rc status parser-result
	)
    (mc-gpg-debug-print (format
			 "(mc-gpg-process-region beg=%s end=%s passwd=%s program=%s args=%s parser=%s bufferdummy=%s)"
			 beg end passwd program args parser bufferdummy))
    (setq stderr-tempfilename
	  (make-temp-name (expand-file-name "mailcrypt-gpg-stderr-"
					    mc-temp-directory)))
    (setq status-tempfilename
	  (make-temp-name (expand-file-name "mailcrypt-gpg-status-"
					    mc-temp-directory)))
    (unwind-protect
	(progn
	  ;; get output places ready
	  (setq mybuf (get-buffer-create " *mailcrypt stdout temp"))
	  (set-buffer mybuf)
	  (erase-buffer)
	  (set-buffer obuf)
	  (buffer-disable-undo mybuf)
	  (if passwd
	      (setq args (append '("--passphrase-fd" "0") args)))
	  (setq args (append (list (concat "2>" stderr-tempfilename)) args))
	  (setq args (append (list (concat "3>" status-tempfilename)) args))
	  (setq args (append '("--status-fd" "3") args))
	  (if mc-gpg-extra-args
	      (setq args (append mc-gpg-extra-args args)))
	  (mc-gpg-debug-print (format "prog is %s, args are %s"
				      program
				      (mapconcat '(lambda (x)
						    (format "'%s'" x))
						 args " ")))
	  (setq proc
		(apply 'start-process-shell-command "*GPG*" mybuf
		       program args))
	  ;; send in passwd if necessary
	  (if passwd
	      (progn
		(process-send-string proc (concat passwd "\n"))
		(or mc-passwd-timeout (mc-deactivate-passwd t))))
	  ;; send in the region
	  (process-send-region proc beg end)
	  ;; finish it off
	  (process-send-eof proc)
	  ;; wait for it to finish
	  (while (eq 'run (process-status proc))
	    (accept-process-output proc 5))
	  ;; remember result codes
	  (setq status (process-status proc))
	  (setq rc (process-exit-status proc))
	  (mc-gpg-debug-print (format "prog finished, rc=%s" rc))
	  ;; Hack to force a status_notify() in Emacs 19.29
	  (delete-process proc)
	  ;; remove the annoying "yes your process has finished" message
	  (set-buffer mybuf)
	  (goto-char (point-max))
	  (if (re-search-backward "\nProcess \\*GPG.*\n\\'" nil t)
	      (delete-region (match-beginning 0) (match-end 0)))
	  (goto-char (point-min))
	  ;; CRNL -> NL
	  (while (search-forward "\r\n" nil t)
	    (replace-match "\n"))
	  ;; ponder process death: signal, not just rc!=0
	  (if (or (eq 'stop status) (eq 'signal status))
	      ;; process died
	      (error "%s exited abnormally: '%s'" program rc) ;;is rc a string?
	    )
	  (if (= 127 rc)
	      (error "%s could not be found" program) ;; at least on my system
	    )
	  ;; fill stderr buf
	  (setq stderr-buf (get-buffer-create " *mailcrypt stderr temp"))
	  (buffer-disable-undo stderr-buf)
	  (set-buffer stderr-buf)
	  (erase-buffer)
	  (insert-file-contents stderr-tempfilename)
	  ;; fill status buf
	  (setq status-buf (get-buffer-create " *mailcrypt status temp"))
	  (buffer-disable-undo status-buf)
	  (set-buffer status-buf)
	  (erase-buffer)
	  (insert-file-contents status-tempfilename)
	  ;; feed the parser
	  (set-buffer mybuf)
	  (setq parser-result (funcall parser mybuf stderr-buf status-buf rc))
	  (mc-gpg-debug-print (format " parser returned %s" parser-result))
	  ;; what did the parser tell us?
	  (if (car parser-result)
	      ;; yes, replace region
	      (progn
		(set-buffer obuf)
		(if boundary
		    (save-restriction
		      (narrow-to-region beg end)
		      (goto-char beg)
		      (insert (format "--%s\n" boundary))
		      (goto-char (point-max))
		      (insert (format "\n--%s
Content-Type: application/pgp-signature
Content-Transfer-Encoding: 7bit

" boundary))
		      (insert-buffer-substring mybuf)
		      (goto-char (point-max))
		      (insert (format "\n--%s--\n" boundary))
		      )
		  (delete-region beg end)
		  (goto-char beg)
		  (insert-buffer-substring mybuf)
		  )))
	  ;; return result
	  (cdr parser-result)
	  )
      ;; cleanup forms
      (if (and proc (eq 'run (process-status proc)))
	  ;; it is still running. kill it.
	  (interrupt-process proc))
      (set-buffer obuf)
      (delete-file stderr-tempfilename)
      (delete-file status-tempfilename)
      ;; kill off temporary buffers (which would be useful for debugging)
      (if t ;; nil for easier debugging
	  (progn
	    (if (get-buffer " *mailcrypt stdout temp")
		(kill-buffer " *mailcrypt stdout temp"))
	    (if (get-buffer " *mailcrypt stderr temp")
		(kill-buffer " *mailcrypt stderr temp"))
	    (if (get-buffer " *mailcrypt status temp")
		(kill-buffer " *mailcrypt status temp"))
	    ))
      )))

(defun mime-mc-gpg-sign-region (start end &optional id unclear boundary)
  (if (not (fboundp 'mc-gpg-insert-parser))
      (load "mc-gpg")
    )
  (let ((buffer (get-buffer-create mc-buffer-name))
	passwd args key
	(parser (function mc-gpg-insert-parser))
	(pgp-path mc-gpg-path)
	)
    (setq key (mc-gpg-lookup-key (or id mc-gpg-user-id)))
    (setq passwd
	  (mc-activate-passwd
	   (cdr key)
	   (format "GnuPG passphrase for %s (%s): " (car key) (cdr key))))
    (setq args
	  (cons
	   (if boundary
	       "--detach-sign"
	     (if unclear
		 "--sign"
	       "--clearsign"))
	   (list "--passphrase-fd" "0"
		 "--armor" "--batch" "--textmode" "--verbose"
		 "--local-user" (cdr key))))
    (if mc-gpg-comment
	(setq args (nconc args
			  (list "--comment"
				(format "\"%s\"" mc-gpg-comment))))
      )
    (if (and boundary
	     (string-match "^pgp-" boundary))
	(setq boundary
	      (concat "gpg-" (substring boundary (match-end 0))))
      )
    (message "Signing as %s ..." (car key))
    (if (mime-mc-gpg-process-region
	 start end passwd pgp-path args parser buffer boundary)
	(progn
	  (if boundary
	      (progn
		(goto-char (point-min))
		(insert
		 (format "\
--[[multipart/signed; protocol=\"application/pgp-signature\";
 boundary=\"%s\"; micalg=pgp-sha1][7bit]]\n" boundary))
		))
	  (message "Signing as %s ... Done." (car key))
	  t)
      nil)))

(defun mime-mc-gpg-encrypt-region (recipients start end &optional id sign)
  (if (not (fboundp 'mc-gpg-encrypt-region))
      (load "mc-gpg")
    )
  (let ((mc-pgp-always-sign (if (eq sign 'maybe)
				mc-pgp-always-sign
			      'never)))
    (mc-gpg-encrypt-region
     (mc-split "\\([ \t\n]*,[ \t\n]*\\)+" recipients)
     start end id nil)
    ))


;;; @ PGP 5.0i functions
;;;

(defun mime-mc-pgp50-process-region
  (beg end passwd program args parser &optional buffer boundary)
  (let ((obuf (current-buffer))
	(process-connection-type nil)
	(shell-file-name "/bin/sh")
	mybuf result rgn proc results)
    (unwind-protect
	(progn
	  (setq mybuf (or buffer (generate-new-buffer " *mailcrypt temp")))
	  (set-buffer mybuf)
	  (erase-buffer)
	  (set-buffer obuf)
	  (buffer-disable-undo mybuf)
	  (setq proc
		(apply 'start-process-shell-command "*PGP*" mybuf program
		       "2>&1" args))
	  ;; Now hand the process to the parser, which returns the exit
	  ;; status of the dead process and the limits of the region
	  ;; containing the PGP results.
	  (setq results (funcall parser proc obuf beg end mybuf passwd))
	  (setq result  (car results))
	  (setq rgn     (cadr results))
	  ;; Hack to force a status_notify() in Emacs 19.29
	  (set-buffer mybuf)
	  ;; Hurm.  FIXME; must get better result codes.
	  (if (stringp result)
	      (mc-message result))
	  ;; If the parser found something, migrate it to the old
	  ;; buffer.  In particular, the parser's job is to return
	  ;; a cons of the form ( beg . end ) delimited the result
	  ;; of PGP in the new buffer.
	  (if (consp rgn)
	      (progn
		(set-buffer obuf)
		(if boundary
		    (save-restriction
		      (narrow-to-region beg end)
		      (goto-char beg)
		      (insert (format "--%s\n" boundary))
		      (goto-char (point-max))
		      (insert (format "\n--%s
Content-Type: application/pgp-signature
Content-Transfer-Encoding: 7bit

" boundary))
		      (insert-buffer-substring mybuf (car rgn) (cdr rgn))
		      (goto-char (point-max))
		      (insert (format "\n--%s--\n" boundary))
		      )
		  (delete-region beg end)
		  (goto-char beg)
		  (insert-buffer-substring mybuf (car rgn) (cdr rgn))
		  )
		(set-buffer mybuf)
		(delete-region (car rgn) (cdr rgn))))
	  ;; Return nil on failure and exit code on success
	  (if rgn result nil))
      ;; Cleanup even on nonlocal exit
      (if (and proc (eq 'run (process-status proc)))
	  (interrupt-process proc))
      (set-buffer obuf)
      (or buffer (null mybuf) (kill-buffer mybuf))
      rgn)))

(defun mime-mc-pgp50-sign-region (start end &optional id unclear boundary)
  (if (not (fboundp 'mc-pgp50-sign-parser))
      (load "mc-pgp5")
    )
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	passwd args key
	(parser (function mc-pgp50-sign-parser))
	(pgp-path mc-pgp50-pgps-path)
	)
    (setq key (mc-pgp50-lookup-key (or id mc-pgp50-user-id)))
    (setq passwd
	  (mc-activate-passwd
	   (cdr key)
	   (format "PGP passphrase for %s (%s): " (car key) (cdr key))))
    (setenv "PGPPASSFD" "0")
    (setq args
	  (cons
	   (if boundary
	       "-fbat"
	     "-fat")
	   (list "+verbose=1" "+language=us"
		 (format "+clearsig=%s" (if unclear "off" "on"))
		 "+batchmode" "-u" (cdr key))))
    (if mc-pgp50-comment
	(setq args (cons (format "+comment=\"%s\"" mc-pgp50-comment) args))
      )
    (message "Signing as %s ..." (car key))
    (if (mime-mc-pgp50-process-region
	 start end passwd pgp-path args parser buffer boundary)
	(progn
	  (if boundary
	      (progn
		(goto-char (point-min))
		(insert
		 (format "\
--[[multipart/signed; protocol=\"application/pgp-signature\";
 boundary=\"%s\"; micalg=pgp-md5][7bit]]\n" boundary))
		))
	  (message "Signing as %s ... Done." (car key))
	  t)
      nil)))

(defun mime-mc-pgp50-encrypt-region (recipients start end &optional id sign)
  (if (not (fboundp 'mc-pgp50-encrypt-region))
      (load "mc-pgp5")
    )
  (let ((mc-pgp-always-sign (if (eq sign 'maybe)
				mc-pgp-always-sign
			      'never)))
    (mc-pgp50-encrypt-region
     (mc-split "\\([ \t\n]*,[ \t\n]*\\)+" recipients)
     start end id nil)
    ))


;;; @ PGP 2.6 functions
;;;

(defun mime-mc-process-region
  (beg end passwd program args parser &optional buffer boundary)
  (let ((obuf (current-buffer))
	(process-connection-type nil)
	mybuf result rgn proc)
    (unwind-protect
	(progn
	  (setq mybuf (or buffer (generate-new-buffer " *mailcrypt temp")))
	  (set-buffer mybuf)
	  (erase-buffer)
	  (set-buffer obuf)
	  (buffer-disable-undo mybuf)
	  (setq proc
		(apply 'start-process "*PGP*" mybuf program args))
	  (if passwd
	      (progn
		(process-send-string proc (concat passwd "\n"))
		(or mc-passwd-timeout (mc-deactivate-passwd t))))
	  (process-send-region proc beg end)
	  (process-send-eof proc)
	  (while (eq 'run (process-status proc))
	    (accept-process-output proc 5))
	  (setq result (process-exit-status proc))
	  ;; Hack to force a status_notify() in Emacs 19.29
	  (delete-process proc)
	  (set-buffer mybuf)
	  (goto-char (point-max))
	  (if (re-search-backward "\nProcess \\*PGP.*\n\\'" nil t)
	      (delete-region (match-beginning 0) (match-end 0)))
	  (goto-char (point-min))
	  ;; CRNL -> NL
	  (while (search-forward "\r\n" nil t)
	    (replace-match "\n"))
	  ;; Hurm.  FIXME; must get better result codes.
	  (if (stringp result)
	      (error "%s exited abnormally: '%s'" program result)
	    (setq rgn (funcall parser result))
	    ;; If the parser found something, migrate it
	    (if (consp rgn)
		(progn
		  (set-buffer obuf)
		  (if boundary
		      (save-restriction
			(narrow-to-region beg end)
			(goto-char beg)
			(insert (format "--%s\n" boundary))
			(goto-char (point-max))
			(insert (format "\n--%s
Content-Type: application/pgp-signature
Content-Transfer-Encoding: 7bit

" boundary))
			(insert-buffer-substring mybuf (car rgn) (cdr rgn))
			(goto-char (point-max))
			(insert (format "\n--%s--\n" boundary))
			)
		    (delete-region beg end)
		    (goto-char beg)
		    (insert-buffer-substring mybuf (car rgn) (cdr rgn))
		    )
		  (set-buffer mybuf)
		  (delete-region (car rgn) (cdr rgn)))))
	  ;; Return nil on failure and exit code on success
	  (if rgn result))
      ;; Cleanup even on nonlocal exit
      (if (and proc (eq 'run (process-status proc)))
	  (interrupt-process proc))
      (set-buffer obuf)
      (or buffer (null mybuf) (kill-buffer mybuf)))))

(defun mime-mc-pgp-sign-region (start end &optional id unclear boundary)
  (if (not (fboundp 'mc-pgp-generic-parser))
      (load "mc-pgp")
    )
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	passwd args key
	(parser (function mc-pgp-generic-parser))
	(pgp-path mc-pgp-path)
	)
    (setq key (mc-pgp-lookup-key (or id mc-pgp-user-id)))
    (setq passwd
	  (mc-activate-passwd
	   (cdr key)
	   (format "PGP passphrase for %s (%s): " (car key) (cdr key))))
    (setenv "PGPPASSFD" "0")
    (setq args
	  (cons
	   (if boundary
	       "-fbast"
	     "-fast")
	   (list "+verbose=1" "+language=en"
		 (format "+clearsig=%s" (if unclear "off" "on"))
		 "+batchmode" "-u" (cdr key))))
    (if mc-pgp-comment
	(setq args (cons (format "+comment=%s" mc-pgp-comment) args))
      )
    (message "Signing as %s ..." (car key))
    (if (mime-mc-process-region
	 start end passwd pgp-path args parser buffer boundary)
	(progn
	  (if boundary
	      (progn
		(goto-char (point-min))
		(insert
		 (format "\
--[[multipart/signed; protocol=\"application/pgp-signature\";
 boundary=\"%s\"; micalg=pgp-md5][7bit]]\n" boundary))
		))
	  (message "Signing as %s ... Done." (car key))
	  t)
      nil)))

(defun mime-mc-pgp-encrypt-region (recipients start end &optional id sign)
  (if (not (fboundp 'mc-pgp-encrypt-region))
      (load "mc-pgp")
    )
  (let ((mc-pgp-always-sign (if (eq sign 'maybe)
				mc-pgp-always-sign
			      'never)))
    (mc-pgp-encrypt-region
     (mc-split "\\([ \t\n]*,[ \t\n]*\\)+" recipients)
     start end id nil)
    ))


;;; @ end
;;;

(provide 'mime-mc)

;;; mime-mc.el ends here
