;;; mime-pgp.el --- mime-view internal methods for either PGP or GnuPG.

;; Copyright (C) 1995,1996,1997,1998,1999 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         Katsumi Yamaoka  <yamaoka@jpl.org>
;; Created: 1995/12/7
;;	Renamed: 1997/2/27 from tm-pgp.el
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

;;; Commentary:

;;    This module is based on

;;	[security-multipart] RFC 1847: "Security Multiparts for MIME:
;;	    Multipart/Signed and Multipart/Encrypted" by
;;	    Jim Galvin <galvin@tis.com>, Sandy Murphy <sandy@tis.com>,
;;	    Steve Crocker <crocker@cybercash.com> and
;;	    Ned Freed <ned@innosoft.com> (1995/10)

;;	[PGP/MIME] RFC 2015: "MIME Security with Pretty Good Privacy
;;	    (PGP)" by Michael Elkins <elkins@aero.org> (1996/6)

;;	[PGP-kazu] draft-kazu-pgp-mime-00.txt: "PGP MIME Integration"
;;	    by Kazuhiko Yamamoto <kazu@is.aist-nara.ac.jp> (1995/10;
;;	    expired)

;;; Code:

(require 'semi-def)
(require 'mime-play)


;;; @ Internal method for multipart/signed
;;;
;;; It is based on RFC 1847 (security-multipart).

(defun mime-verify-multipart/signed (entity situation)
  "Internal method to verify multipart/signed."
  (mime-play-entity
   (nth 1 (mime-entity-children entity)) ; entity-info of signature
   (list (assq 'mode situation)) ; play-mode
   ))


;;; @ internal method for application/pgp
;;;
;;; It is based on draft-kazu-pgp-mime-00.txt (PGP-kazu).

(defun mime-view-application/pgp (entity situation)
  (let* ((p-win (or (get-buffer-window (current-buffer))
		    (get-largest-window)))
	 (new-name
	  (format "%s-%s" (buffer-name) (mime-entity-number entity)))
	 (mother (current-buffer))
	 representation-type)
    (set-buffer (get-buffer-create new-name))
    (erase-buffer)
    (mime-insert-entity entity)
    (cond ((progn
	     (goto-char (point-min))
	     (re-search-forward "^-+BEGIN PGP SIGNED MESSAGE-+$" nil t))
	   (funcall (pgp-function 'verify))
	   (goto-char (point-min))
	   (delete-region
	    (point-min)
	    (and
	     (re-search-forward "^-+BEGIN PGP SIGNED MESSAGE-+\n\n")
	     (match-end 0)))
	   (delete-region
	    (and (re-search-forward "^-+BEGIN PGP SIGNATURE-+")
		 (match-beginning 0))
	    (point-max))
	   (goto-char (point-min))
	   (while (re-search-forward "^- -" nil t)
	     (replace-match "-")
	     )
	   (setq representation-type (if (mime-entity-cooked-p entity)
					 'cooked))
	   )
	  ((progn
	     (goto-char (point-min))
	     (re-search-forward "^-+BEGIN PGP MESSAGE-+$" nil t))
	   (as-binary-process (funcall (pgp-function 'decrypt)))
	   (goto-char (point-min))
	   (delete-region (point-min)
			  (and
			   (search-forward "\n\n")
			   (match-end 0)))
	   (setq representation-type 'binary)
	   ))
    (setq major-mode 'mime-show-message-mode)
    (save-window-excursion (mime-view-buffer nil nil mother
					     nil representation-type))
    (set-window-buffer p-win mime-preview-buffer)
    ))


;;; @ Internal method for application/pgp-signature
;;;
;;; It is based on RFC 2015 (PGP/MIME).

(defcustom mime-pgp-command-alist '((gpg   . "gpg")
				    (pgp50 . "pgp")
				    (pgp   . "pgp"))
  "Alist of the schemes and the name of the commands.  Valid SCHEMEs are:

   gpg   - GnuPG.
   pgp50 - PGP version 5.0i.
   pgp   - PGP version 2.6.

COMMAND for `pgp50' must *NOT* have a suffix, like neither \"pgpe\", \"pgpk\",
\"pgps\" nor \"pgpv\"."
  :group 'mime
  :type '(repeat (cons :format "%v"
		       (choice (choice-item :tag "GnuPG" gpg)
			       (choice-item :tag "PGP 5.0i" pgp50)
			       (choice-item :tag "PGP 2.6" pgp))
		       (string :tag "Command"))))

(defcustom mime-pgp-default-language-alist '((gpg   . nil)
					     (pgp50 . us)
					     (pgp   . en))
  "Alist of the schemes and the symbol of languages.  It should be ISO 639
2 letter language code such as en, ja, ...  Each element looks like
\(SCHEME . SYMBOL).  See also `mime-pgp-command-alist' for valid SCHEMEs."
  :group 'mime
  :type '(repeat (cons :format "%v"
		       (choice (choice-item :tag "GnuPG" gpg)
			       (choice-item :tag "PGP 5.0i" pgp50)
			       (choice-item :tag "PGP 2.6" pgp))
		       (symbol :tag "Language"))))

(defcustom mime-pgp-good-signature-regexp-alist
  '((gpg
     (nil "Good signature from.*$" nil)
     )
    (pgp50
     (us "Good signature made .* by key:$"
	 mime-pgp-good-signature-post-function-pgp50-us)
     )
    (pgp
     (en "Good signature from user.*$" nil)
     ))
  "Alist of the schemes and alist of the languages and the regexps for
detecting ``Good signature''.  The optional symbol of the post processing
function for arranging the output message can be specified in each element.
It will be called just after re-search is done successfully, and it is
expected that the function returns a string for messaging."
  :group 'mime
  :type '(repeat (cons :format "%v"
		       (choice (choice-item :tag "GnuPG" gpg)
			       (choice-item :tag "PGP 5.0i" pgp50)
			       (choice-item :tag "PGP 2.6" pgp))
		       (repeat (list :format "%v"
				     (symbol :tag "Language")
				     (regexp :tag "Regexp")
				     (function :tag "Post Function"))))))

(defcustom mime-pgp-key-expected-regexp-alist
  '((gpg
     (nil
      .
      "key ID \\(\\S +\\)\ngpg: Can't check signature: public key not found")
     )
    (pgp50
     (us . "Signature by unknown keyid: 0x\\(\\S +\\)")
     )
    (pgp
     (en . "Key matching expected Key ID \\(\\S +\\) not found")
     ))
  "Alist of the schemes and alist of the languages and regexps for detecting
``Key expected''."
  :group 'mime
  :type '(repeat (cons :format "%v"
		       (choice (choice-item :tag "GnuPG" gpg)
			       (choice-item :tag "PGP 5.0i" pgp50)
			       (choice-item :tag "PGP 2.6" pgp))
		       (repeat (cons :format "%v"
				     (symbol :tag "Language")
				     (regexp :tag "Regexp"))))))

(defmacro mime-pgp-command (&optional suffix)
  "Return a suitable command.  SUFFIX should be either \"e\", \"k\", \"s\"
or \"v\" for choosing a command of PGP 5.0i."
  (` (let ((command (cdr (assq pgp-version mime-pgp-command-alist))))
       (if (and command
		(progn
		  (if (eq 'pgp50 pgp-version)
		      (setq command (format "%s%s" command (, suffix))))
		  (exec-installed-p command)))
	   command
	 (error "Please specify the valid command name for `%s'."
		(or pgp-version 'pgp-version))))))

(defmacro mime-pgp-default-language ()
  "Return a symbol of language."
  '(cond ((eq 'gpg pgp-version)
	  nil)
	 ((eq 'pgp50 pgp-version)
	  (or (cdr (assq pgp-version mime-pgp-default-language-alist)) 'us)
	  )
	 (t
	  (or (cdr (assq pgp-version mime-pgp-default-language-alist)) 'en)
	  )))

(defmacro mime-pgp-good-signature-regexp ()
  "Return a regexp to detect ``Good signature''."
  '(nth 1
	(assq
	 (mime-pgp-default-language)
	 (cdr (assq pgp-version mime-pgp-good-signature-regexp-alist))
	 )))

(defmacro mime-pgp-good-signature-post-function ()
  "Return a post processing function for arranging the message for
``Good signature''."
  '(nth 2
	(assq
	 (mime-pgp-default-language)
	 (cdr (assq pgp-version mime-pgp-good-signature-regexp-alist))
	 )))

(defmacro mime-pgp-key-expected-regexp ()
  "Return a regexp to detect ``Key expected''."
  '(cdr (assq (mime-pgp-default-language)
	      (cdr (assq pgp-version mime-pgp-key-expected-regexp-alist))
	      )))

(defun mime-pgp-check-signature (output-buffer orig-file)
  (save-excursion
    (set-buffer output-buffer)
    (erase-buffer)
    (setq truncate-lines t))
  (let* ((lang (mime-pgp-default-language))
	 (command (mime-pgp-command 'v))
	 (args (cond ((eq 'gpg pgp-version)
		      (list "--batch" "--verify"
			    (concat orig-file ".sig"))
		      )
		     ((eq 'pgp50 pgp-version)
		      (list "+batchmode=1"
			    (format "+language=%s" lang)
			    (concat orig-file ".sig"))
		      )
		     ((eq 'pgp pgp-version)
		      (list (format "+language=%s" lang) orig-file))
		     ))
	 (regexp (mime-pgp-good-signature-regexp))
	 (post-function (mime-pgp-good-signature-post-function))
	 pgp-id)
    (if (zerop (apply 'call-process-region
		      (point-min) (point-max) command nil output-buffer nil
		      args))
	(save-excursion
	  (set-buffer output-buffer)
	  (goto-char (point-min))
	  (cond
	   ((not (stringp regexp))
	    (message "Please specify right regexp for specified language")
	    )
	   ((re-search-forward regexp nil t)
	    (message (if post-function
			 (funcall post-function)
		       (buffer-substring (match-beginning 0) (match-end 0))))
	    (goto-char (point-min))
	    )
	   ;; PGP 5.0i always returns 0, so we should attempt to fetch.
	   ((eq 'pgp50 pgp-version)
	    (if (not (stringp (setq regexp (mime-pgp-key-expected-regexp))))
		(message "Please specify right regexp for specified language")
	      (if (re-search-forward regexp nil t)
		  (progn
		    (setq pgp-id
			  (concat "0x" (buffer-substring-no-properties
					(match-beginning 1)
					(match-end 1))))
		    (if (and
			 pgp-id
			 (y-or-n-p
			  (format "Key %s not found; attempt to fetch? "
				  pgp-id)
			  ))
			(progn
			  (funcall (pgp-function 'fetch-key) (cons nil pgp-id))
			  (mime-pgp-check-signature mime-echo-buffer-name
						    orig-file)
			  )
		      (message "Bad signature")
		      ))
		(message "Bad signature")
		))
	    )
	   (t
	    (message "Bad signature")
	    ))
	  )
      (message "Bad signature")
      nil)))

(defun mime-verify-application/pgp-signature (entity situation)
  "Internal method to check PGP/MIME signature."
  (let* ((entity-node-id (mime-entity-node-id entity))
	 (mother (mime-entity-parent entity))
	 (knum (car entity-node-id))
	 (onum (if (> knum 0)
		   (1- knum)
		 (1+ knum)))
	 (orig-entity (nth onum (mime-entity-children mother)))
	 (basename (expand-file-name "tm" temporary-file-directory))
	 (orig-file (make-temp-name basename))
	 (sig-file (concat orig-file ".sig"))
	 )
    (mime-write-entity orig-entity orig-file)
    (save-excursion (mime-show-echo-buffer))
    (mime-write-entity-content entity sig-file)
    (if (mime-pgp-check-signature mime-echo-buffer-name orig-file)
	(let ((other-window-scroll-buffer mime-echo-buffer-name))
	  (scroll-other-window
	   (cdr (assq pgp-version '((gpg . 0) (pgp50 . 1) (pgp . 10))))
	   ))
      (if (eq 'pgp pgp-version)
	  (save-excursion
	    (set-buffer mime-echo-buffer-name)
	    (goto-char (point-min))
	    (if (search-forward "\C-g" nil t)
		(goto-char (match-beginning 0))
	      (forward-line 7))
	    (set-window-start
	     (get-buffer-window mime-echo-buffer-name)
	     (point))
	    )
	(let ((other-window-scroll-buffer mime-echo-buffer-name))
	  (scroll-other-window
	   (cdr (assq pgp-version '((gpg . 0) (pgp50 . 1))))
	   )))
      (let (pgp-id)
	(save-excursion
	  (set-buffer mime-echo-buffer-name)
	  (goto-char (point-min))
	  (let ((regexp (mime-pgp-key-expected-regexp)))
	    (cond
	     ((not (stringp regexp))
	      (message "Please specify right regexp for specified language")
	      )
	     ((re-search-forward regexp nil t)
	      (setq pgp-id (concat "0x" (buffer-substring-no-properties
					 (match-beginning 1)
					 (match-end 1))))
	      ))))
	(if (and pgp-id
		 (prog1
		     (y-or-n-p
		      (format "Key %s not found; attempt to fetch? " pgp-id))
		   (message ""))
		 )
	    (progn
	      (funcall (pgp-function 'fetch-key) (cons nil pgp-id))
	      (mime-pgp-check-signature mime-echo-buffer-name orig-file)
	      )
	  )))
    (delete-file orig-file)
    (delete-file sig-file)
    ))

(defun mime-pgp-good-signature-post-function-pgp50-us ()
  (forward-line 2)
  (looking-at "\\s +\\(.+\\)$")
  (format "Good signature from %s" (match-string 1)))


;;; @ Internal method for application/pgp-encrypted
;;;
;;; It is based on RFC 2015 (PGP/MIME).

(defun mime-decrypt-application/pgp-encrypted (entity situation)
  (let* ((entity-node-id (mime-entity-node-id entity))
	 (mother (mime-entity-parent entity))
	 (knum (car entity-node-id))
	 (onum (if (> knum 0)
		   (1- knum)
		 (1+ knum)))
	 (orig-entity (nth onum (mime-entity-children mother))))
    (mime-view-application/pgp orig-entity situation)
    ))


;;; @ Internal method for application/pgp-keys
;;;
;;; It is based on RFC 2015 (PGP/MIME).

(defun mime-add-application/pgp-keys (entity situation)
  (let* ((start (mime-entity-point-min entity))
	 (end (mime-entity-point-max entity))
	 (entity-number (mime-raw-point-to-entity-number start))
	 (new-name (format "%s-%s" (buffer-name) entity-number))
	 (encoding (cdr (assq 'encoding situation)))
	 str)
    (setq str (buffer-substring start end))
    (switch-to-buffer new-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert str)
    (goto-char (point-min))
    (if (re-search-forward "^\n" nil t)
	(delete-region (point-min) (match-end 0))
      )
    (mime-decode-region (point-min)(point-max) encoding)
    (funcall (pgp-function 'snarf-keys))
    (kill-buffer (current-buffer))
    ))


;;; @ end
;;;

(provide 'mime-pgp)

(run-hooks 'mime-pgp-load-hook)

;;; mime-pgp.el ends here
