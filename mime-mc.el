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

(require 'alist)
(require 'std11)
(require 'semi-def)
(require 'mime-def)
(require 'mailcrypt)

(eval-when-compile
  (load "expect" t)
  )

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

(defcustom mime-mc-shell-file-name "/bin/sh"
  "File name to load inferior shells from.  Bourne shell or its equivalent
\(not tcsh) is needed for \"2>\"."
  :group 'mime
  :type 'file)

(defcustom mime-mc-omit-micalg nil
  "Non-nil value means to omit the micalg parameter for multipart/signed.
See draft-yamamoto-openpgp-mime-00.txt (OpenPGP/MIME) for more information."
  :group 'mime
  :type 'boolean)

(defcustom mime-mc-comment-alist
  (let ((product-name (mime-product-name mime-user-interface-product))
	(version (mapconcat
		  (function number-to-string)
		  (mime-product-version mime-user-interface-product)
		  "."))
	(codename (mime-product-code-name mime-user-interface-product))
	string)
    (setq string (format "Processed by Mailcrypt %s under %s %s%s"
			 mc-version product-name version
			 (if (string-match "^[ -~]+$" codename)
			     (concat " - \"" codename "\"")
			   "")))
    (list (cons 'gpg string)
	  (cons 'pgp50 string)
	  (cons 'pgp string)))
  "Alist of the schemes and strings of the comment field to appear in ASCII
armor output."
  :group 'mime
  :type '(repeat (cons :format "%v"
		       (choice (choice-item :tag "GnuPG" gpg)
			       (choice-item :tag "PGP 5.0i" pgp50)
			       (choice-item :tag "PGP 2.6" pgp))
		       (string :tag "Comment"))))

(defmacro mime-mc-comment ()
  "Return a string of the comment field."
  '(or (cdr (assq pgp-version mime-mc-comment-alist))
       (symbol-value (intern (format "mc-%s-comment" pgp-version)))))


;;; @ Internal variable
;;;

(defvar mime-mc-micalg-alist nil
  "Alist of KeyID and the value of message integrity check algorithm.")


;;; @ External variables (for avoid byte compile warnings)
;;;

(defvar mc-gpg-extra-args)
(defvar mc-gpg-path)
(defvar mc-gpg-user-id)
(defvar mc-pgp50-pgps-path)
(defvar mc-pgp50-user-id)
(defvar mc-pgp-path)
(defvar mc-pgp-user-id)


;;; @ Generic functions
;;;

(defun mime-mc-setversion (&optional version)
  "Select `pgp-version' and `mc-default-scheme' if possible.
VERSION should be a string or a symbol."
  (interactive)
  (let ((oldversion pgp-version)
	(table '(("GnuPG" . gpg) ("PGP 5.0i" . pgp50) ("PGP 2.6" . pgp)
		 ("gnupg" . gpg) ("gpg" . gpg) ("pgp5" . pgp50)
		 ("pgp50" . pgp50) ("pgp2" . pgp) ("pgp" . pgp)
		 ("5.0" . pgp50) ("2.6" . pgp))))
    (if (interactive-p)
	(setq version (completing-read
		       (format "Select PGP version (currently %s): "
			       (car (rassoc oldversion table)))
		       table nil t)
	      pgp-version (or (cdr (assoc version table))
			      oldversion))
      (if (stringp version)
	  (setq pgp-version (or (cdr (assoc version table)) oldversion))
	(if (memq version '(gpg pgp50 pgp))
	    (setq pgp-version version)
	  )))
    (condition-case nil
	(mc-setversion
	 (cdr (assq pgp-version
		    '((gpg . "gpg") (pgp50 . "5.0") (pgp . "2.6"))))
	 )
      (error nil))
    (message "PGP version set to %s." (car (rassq pgp-version table)))
    ))

(defun mime-mc-replace-comment-field (comment &optional start end)
  (let ((regexp (if (eq 'pgp pgp-version)
		    "-----BEGIN PGP.*-----\nVersion:"
		  "^-----BEGIN PGP.*\n")))
    (save-excursion
      (save-restriction
	(narrow-to-region (or start (point-min)) (or end (point-max)))
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
	  (forward-line 1)
	  (save-restriction
	    (narrow-to-region (point)
			      (if (search-forward "\n\n" nil t)
				  (point)
				(point-max)))
	    (goto-char (point-min))
	    (if (re-search-forward "^Comment:.*$" nil t)
		(replace-match (concat "Comment: " comment))
	      )))
	(point-max)))))

(defun mime-mc-insert-public-key (&optional userid)
  (let ((not-loaded (not (fboundp (intern (format "mc-%s-insert-public-key"
						  pgp-version)))))
	(comment (mime-mc-comment))
	(scheme (intern (format "mc-scheme-%s" pgp-version))))
    (cond ((eq 'gpg pgp-version)
	   (if not-loaded
	       (load "mc-gpg")
	     )
	   (let ((mc-gpg-comment (if comment "DUMMY")))
	     (mc-insert-public-key userid scheme))
	   )
	  ((eq 'pgp50 pgp-version)
	   (if not-loaded
	       (load "mc-pgp5")
	     )
	   (let ((mc-pgp50-comment (if comment "DUMMY")))
	     (mc-insert-public-key userid scheme))
	   )
	  (t
	   (if not-loaded
	       (load "mc-pgp")
	     )
	   (let ((mc-pgp-comment (if comment "DUMMY")))
	     (mc-insert-public-key userid scheme))
	   ))
    (if comment
	(mime-mc-replace-comment-field comment)
      )))

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
  (beg end passwd program args parser bufferdummy &optional boundary comment)
  "Similar to `mc-gpg-process-region', however enclose an processed data
with BOUNDARY if it is specified and replace the comment field with the
optional argument COMMENT if it is specified."
  (let ((obuf (current-buffer))
	(process-connection-type nil)
	(shell-file-name mime-mc-shell-file-name)
	; other local vars
	mybuf 
	stderr-tempfilename stderr-buf
	status-tempfilename status-buf
	proc rc status parser-result
	)
    (mc-gpg-debug-print (format 
       "(mime-mc-gpg-process-region beg=%s end=%s passwd=%s program=%s args=%s parser=%s bufferdummy=%s boundary=%s comment=%s)"
       beg end passwd program args parser bufferdummy boundary comment))
    (setq stderr-tempfilename 
	  (make-temp-name (expand-file-name "mailcrypt-gpg-stderr-"
					    mc-temp-directory)))
    (setq status-tempfilename 
	  (make-temp-name (expand-file-name "mailcrypt-gpg-status-"
					    mc-temp-directory)))
    (unwind-protect
	(progn
	  ;; Returns non-nil if success, otherwise nil with error message.
	  (catch 'mime-mc-gpg-process-region-done

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

	  (if comment
	      (setq args (append '("--comment" "DUMMY") args))
	    )

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
	      (progn
		(message
		 "%s exited abnormally: '%s'" program rc) ;; is rc a string?
		(throw 'mime-mc-gpg-process-region-done nil)
		))

	  (if (= 127 rc)
	      (progn
		(message
		 "%s could not be found" program) ;; at least on my system
		(throw 'mime-mc-gpg-process-region-done nil)
		))

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

	  ;; replace comment string
	  (set-buffer mybuf)
	  (if comment
	      (mime-mc-replace-comment-field comment)
	    )

	  ;; feed the parser
	  (condition-case err
	      (setq parser-result
		    (funcall parser mybuf stderr-buf status-buf rc)
		    )
	    (error
	     (message "%s" err)
	     (throw 'mime-mc-gpg-process-region-done nil)
	     ))
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
	  ))
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
	micalg
	(comment (mime-mc-comment))
	)
    (setq key (mc-gpg-lookup-key (or id mc-gpg-user-id)))
    (setq passwd
	  (mc-activate-passwd
	   (cdr key)
	   (format "GnuPG passphrase for %s (%s): " (car key) (cdr key))))
    (setq args (cons
		(if boundary
		    "--detach-sign"
		  (if unclear
		      "--sign"
		    "--clearsign")
		  )
		(list "--armor" "--batch" "--textmode" "--verbose"
		      "--local-user" (cdr key))
		))
    (if boundary
	(progn
	  (if (string-match "^pgp-" boundary)
	      (setq boundary
		    (concat "gpg-" (substring boundary (match-end 0))))
	    )
	  (if (not (or mime-mc-omit-micalg
		       (setq micalg
			     (cdr (assoc (cdr key) mime-mc-micalg-alist)))
		       ))
	      (with-temp-buffer
		(message "Detecting the value of `micalg'...")
		(insert "\n")
		(if (let ((mc-passwd-timeout 60)) ;; Don't deactivate passwd.
		      (mime-mc-gpg-process-region
		       1 2 passwd pgp-path
		       (list "--clearsign" "--armor" "--batch" "--textmode"
			     "--verbose" "--local-user" (cdr key))
		       parser buffer nil)
		      )
		    (progn
		      (std11-narrow-to-header)
		      (setq micalg
			    (downcase (or (std11-fetch-field "Hash") "md5"))
			    )
		      (set-alist 'mime-mc-micalg-alist (cdr key) micalg)
		      )
		  ))
	    )))
    (if (or mime-mc-omit-micalg micalg)
	(progn
	  (message "Signing as %s ..." (car key))
	  (if (mime-mc-gpg-process-region
	       start end passwd pgp-path args parser buffer boundary comment)
	      (progn
		(if boundary
		    (progn
		      (goto-char (point-min))
		      (insert
		       (format "\
--[[multipart/signed; protocol=\"application/pgp-signature\";
 boundary=\"%s\"%s][7bit]]\n"
			       boundary
			       (if mime-mc-omit-micalg
				   ""
				 (concat "; micalg=pgp-" micalg)
				 )
			       ))))
		(message "Signing as %s ... Done." (car key))
		t)
	    nil)
	  )
      nil)))

(defun mime-mc-gpg-encrypt-region (recipients start end &optional id sign)
  (if (not (fboundp 'mc-gpg-encrypt-region))
      (load "mc-gpg")
    )
  (let ((mc-pgp-always-sign (if (eq sign 'maybe)
				mc-pgp-always-sign
			      'never))
	(comment (mime-mc-comment)))
    (prog1
	(mc-gpg-encrypt-region
	 (mc-split "\\([ \t\n]*,[ \t\n]*\\)+" recipients)
	 start end id nil)
      (if comment
	  (mime-mc-replace-comment-field comment)
	))))


;;; @ PGP 5.0i functions
;;;

(defun mime-mc-pgp50-process-region
  (beg end passwd program args parser &optional buffer boundary comment)
  "Similar to `mc-pgp50-process-region', however enclose an processed data
with BOUNDARY if it is specified and replace the comment field with the
optional argument COMMENT if it is specified."
  (let ((obuf (current-buffer))
	(process-connection-type nil)
	(shell-file-name mime-mc-shell-file-name)
	mybuf result rgn proc results)
    (if comment
	(setq args (cons "+comment=DUMMY" args))
      )
    (unwind-protect
	(progn
	  ;; Returns non-nil if success, otherwise nil with error message.
	  (catch 'mime-mc-pgp50-process-region-done

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
	  (condition-case err
	      (setq results (funcall parser proc obuf beg end mybuf passwd))
	    (error
	     (message "%s" err)
	     (throw 'mime-mc-pgp50-process-region-done nil)
	     ))
	  (setq result  (car results))
	  (setq rgn     (cadr results))

	  ;; Hack to force a status_notify() in Emacs 19.29
	  (set-buffer mybuf)

	  ;; replace comment string
	  (if (and comment (consp rgn))
	      (setcdr rgn (mime-mc-replace-comment-field
			   comment (car rgn) (cdr rgn)))
	    )

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
	  (if rgn result nil)))

      ;; Cleanup even on nonlocal exit
      (if (and proc (eq 'run (process-status proc)))
	  (interrupt-process proc))
      (set-buffer obuf)
      (or buffer (null mybuf) (kill-buffer mybuf))
      rgn)))

(defun mime-mc-pgp50-sign-parser (proc oldbuf start end newbuf passwd)
  ;; This function is a copy of `mc-pgp50-sign-parser', however it is
  ;; modified for parsing a detached sign.
  (let (result results rgn)
    ;; (setenv "PGPPASSFD" "0")
    (set-buffer newbuf)
    (goto-char (point-max))
    (progn
      (unwind-protect
	  (with-expect proc
	    (message "Sending passphrase...")
	    (expect-send (concat passwd "\n"))
	    (or mc-passwd-timeout (mc-deactivate-passwd t))
	    (expect "No files specified.  Using stdin."
	      (message "Passphrase sent.  Signing...")
	      (set-buffer oldbuf)
	      (process-send-region proc start end)
	      (set-buffer newbuf)
	      (process-send-eof proc)

	      ;; Test output of the program, looking for
	      ;; errors.
	      (expect-cond

	       ;; OPTION 1:  Great!  The data is now signed!
	       ("-----END PGP SIGNATURE-----"

		;; Catch the exit status.
		(setq result (process-exit-status proc))
		(delete-process proc)
		(message "Signing complete.")

		;; Delete everything preceding the signed data.
		(goto-char (point-max))
		(re-search-backward
		 ;; "-----BEGIN PGP SIGNED MESSAGE-----" nil t)
		 "-----BEGIN PGP SIGNATURE-----" nil t)
		(delete-region (point-min) (match-beginning 0))
		(setq rgn (point-min))

		;; Convert out CR/NL -> NL
		(goto-char (point-min))
		(while (search-forward "\r\n" nil t)
		  (replace-match "\n"))

		;; Delete everything after the signature.
		(goto-char (point-min))
		(re-search-forward
		 "-----END PGP SIGNATURE-----\n" nil t)
		(delete-region (match-end 0) (point-max))
			 
		;; Return the exit status, with the region
		;; limits!
		(setq rgn (cons rgn (point-max)))
		(setq results (list result rgn)))
			

	       ;; OPTION 1.a:  The data is now signed, but is 8bit data.
	       ("-----END PGP MESSAGE-----"

		;; Catch the exit status.
		(setq result (process-exit-status proc))
		(delete-process proc)
		(message "Signing complete.")

		;; Delete everything preceding the signed data.
		(goto-char (point-max))
		(re-search-backward 
		 "-----BEGIN PGP MESSAGE-----" nil t)
		(delete-region (point-min) (match-beginning 0))
		(setq rgn (point-min))

		;; Convert out CR/NL -> NL
		(goto-char (point-min))
		(while (search-forward "\r\n" nil t)
		  (replace-match "\n"))

		;; Delete everything after the signature.
		(goto-char (point-min))
		(re-search-forward
		 "-----END PGP MESSAGE-----\n" nil t)
		(delete-region (match-end 0) (point-max))
			 
		;; Return the exit status, with the region
		;; limits!
		(setq rgn (cons rgn (point-max)))
		(setq results (list result rgn)))
			

	       ;; OPTION 2:  Awww...bad passphrase!
	       ("Enter pass phrase:" 
		(mc-deactivate-passwd t)
		(interrupt-process proc)
		(delete-process proc)

		;; Return the bad news.
		(setq results '("Incorrect passphrase" nil)))

	       ;; OPTION 3:  The program exits.
	       (exit
		(setq results (list 
			       (process-exit-status proc) nil)))))))
      results)))

(defun mime-mc-pgp50-sign-region (start end &optional id unclear boundary)
  (if (not (fboundp 'mc-pgp50-sign-parser))
      (load "mc-pgp5")
    )
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	passwd args key
	(parser (if boundary
		    (function mime-mc-pgp50-sign-parser)
		  (function mc-pgp50-sign-parser)))
	(pgp-path mc-pgp50-pgps-path)
	micalg
	(comment (mime-mc-comment))
	)
    (setq key (mc-pgp50-lookup-key (or id mc-pgp50-user-id)))
    (setq passwd
	  (mc-activate-passwd
	   (cdr key)
	   (format "PGP passphrase for %s (%s): " (car key) (cdr key))))
    (setenv "PGPPASSFD" "0")
    (setq args (if boundary
		   (list "-fbat" "+verbose=1" "+language=us" "+batchmode"
			 "-u" (cdr key))
		 (list "-fat" "+verbose=1" "+language=us"
		       (format "+clearsig=%s" (if unclear "off" "on"))
		       "+batchmode" "-u" (cdr key))
		 ))
    (if (and boundary
	     (not (or mime-mc-omit-micalg
		      (setq micalg
			    (cdr (assoc (cdr key) mime-mc-micalg-alist)))
		      )))
	(with-temp-buffer
	  (message "Detecting the value of `micalg'...")
	  (insert "\n")
	  (if (let ((mc-passwd-timeout 60)) ;; Don't deactivate passwd.
		(mime-mc-pgp50-process-region
		 1 2 passwd pgp-path
		 (list "-fat" "+verbose=1" "+language=us" "+clearsig=on"
		       "+batchmode" "-u" (cdr key))
		 (function mc-pgp50-sign-parser) buffer nil)
		)
	      (progn
		(std11-narrow-to-header)
		(setq micalg (downcase (or (std11-fetch-field "Hash") "md5")))
		(set-alist 'mime-mc-micalg-alist (cdr key) micalg)
		)
	    ))
      )
    (if (or mime-mc-omit-micalg micalg)
	(progn
	  (message "Signing as %s ..." (car key))
	  (if (mime-mc-pgp50-process-region
	       start end passwd pgp-path args parser buffer boundary comment)
	      (progn
		(if boundary
		    (progn
		      (goto-char (point-min))
		      (insert
		       (format "\
--[[multipart/signed; protocol=\"application/pgp-signature\";
 boundary=\"%s\"%s][7bit]]\n"
			       boundary
			       (if mime-mc-omit-micalg
				   ""
				 (concat "; micalg=pgp-" micalg)
				 )
			       ))))
		(message "Signing as %s ... Done." (car key))
		t)
	    nil)
	  )
      nil)))

(defun mime-mc-pgp50-encrypt-region (recipients start end &optional id sign)
  (if (not (fboundp 'mc-pgp50-encrypt-region))
      (load "mc-pgp5")
    )
  (let ((mc-pgp-always-sign (if (eq sign 'maybe)
				mc-pgp-always-sign
			      'never))
	(comment (mime-mc-comment))
	(mc-pgp50-comment "DUMMY"))
    (prog1
	(mc-pgp50-encrypt-region
	 (mc-split "\\([ \t\n]*,[ \t\n]*\\)+" recipients)
	 start end id nil)
      (if comment
	  (mime-mc-replace-comment-field comment)
	))))


;;; @ PGP 2.6 functions
;;;

(defun mime-mc-process-region
  (beg end passwd program args parser &optional buffer boundary comment)
  "Similar to `mc-pgp-process-region', however enclose an processed data
with BOUNDARY if it is specified and replace the comment field with the
optional argument COMMENT if it is specified."
  (let ((obuf (current-buffer))
	(process-connection-type nil)
	mybuf result rgn proc)
    (if comment
	(setq args (cons "+comment=DUMMY" args))
      )
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
	    ;; replace comment string
	    (if comment
		(mime-mc-replace-comment-field comment)
	      )
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
	(comment (mime-mc-comment))
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
    (message "Signing as %s ..." (car key))
    (if (mime-mc-process-region
	 start end passwd pgp-path args parser buffer boundary comment)
	(progn
	  (if boundary
	      (progn
		(goto-char (point-min))
		(insert
		 (format "\
--[[multipart/signed; protocol=\"application/pgp-signature\";
 boundary=\"%s\"%s][7bit]]\n"
			 boundary
			 (if mime-mc-omit-micalg
			     ""
			   "; micalg=pgp-md5"
			   )
			 ))))
	  (message "Signing as %s ... Done." (car key))
	  t)
      nil)))

(defun mime-mc-pgp-encrypt-region (recipients start end &optional id sign)
  (if (not (fboundp 'mc-pgp-encrypt-region))
      (load "mc-pgp")
    )
  (let ((mc-pgp-always-sign (if (eq sign 'maybe)
				mc-pgp-always-sign
			      'never))
	(comment (mime-mc-comment))
	(mc-pgp-comment "DUMMY"))
    (prog1
	(mc-pgp-encrypt-region
	 (mc-split "\\([ \t\n]*,[ \t\n]*\\)+" recipients)
	 start end id nil)
      (if comment
	  (mime-mc-replace-comment-field comment)
	))))


;;; @ end
;;;

(provide 'mime-mc)

;;; mime-mc.el ends here
