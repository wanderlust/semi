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

;;	[OpenPGP/MIME] draft-yamamoto-openpgp-mime-00.txt: "MIME
;;	    Security with OpenPGP (OpenPGP/MIME)" by Kazuhiko YAMAMOTO
;;	    <kazu@iijlab.net> (1998/1)

;;; Code:

(require 'std11)
(require 'semi-def)
(require 'mime-play)

(defgroup mime-pgp nil
  "Internal methods for either PGP or GnuPG."
  :prefix "mime-pgp-"
  :group 'mime)

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
	     (re-search-forward "^-+BEGIN PGP SIGNED MESSAGE-+\n")
	     (search-forward "\n\n")
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
;;; It is based on RFC 2015 (PGP/MIME) and
;;; draft-yamamoto-openpgp-mime-00.txt (OpenPGP/MIME).

(defcustom mime-pgp-command-alist '((gpg   . "gpg")
				    (pgp50 . "pgp")
				    (pgp   . "pgp"))
  "Alist of the schemes and the name of the commands.  Valid SCHEMEs are:

   gpg   - GnuPG.
   pgp50 - PGP version 5.0i.
   pgp   - PGP version 2.6.

COMMAND for `pgp50' must *NOT* have a suffix, like neither \"pgpe\", \"pgpk\",
\"pgps\" nor \"pgpv\"."
  :group 'mime-pgp
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
  :group 'mime-pgp
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
  :group 'mime-pgp
  :type '(repeat (cons :format "%v"
		       (choice (choice-item :tag "GnuPG" gpg)
			       (choice-item :tag "PGP 5.0i" pgp50)
			       (choice-item :tag "PGP 2.6" pgp))
		       (repeat (list :format "%v"
				     (symbol :tag "Language")
				     (regexp :tag "Regexp")
				     (function :tag "Post Function"))))))

(defcustom mime-pgp-bad-signature-regexp-alist
  '((gpg
     (nil "BAD signature from.*$" nil)
     )
    (pgp50
     (us "BAD signature made .* by key:$"
	 mime-pgp-bad-signature-post-function-pgp50-us)
     )
    (pgp
     (en "Bad signature from user.*$" nil)
     ))
  "Alist of the schemes and alist of the languages and the regexps for
detecting ``BAD signature''.  The optional symbol of the post processing
function for arranging the output message can be specified in each element.
It will be called just after re-search is done successfully, and it is
expected that the function returns a string for messaging."
  :group 'mime-pgp
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
  :group 'mime-pgp
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

(defmacro mime-pgp-bad-signature-regexp ()
  "Return a regexp to detect ``BAD signature''."
  '(nth 1
	(assq
	 (mime-pgp-default-language)
	 (cdr (assq pgp-version mime-pgp-bad-signature-regexp-alist))
	 )))

(defmacro mime-pgp-bad-signature-post-function ()
  "Return a post processing function for arranging the message for
``BAD signature''."
  '(nth 2
	(assq
	 (mime-pgp-default-language)
	 (cdr (assq pgp-version mime-pgp-bad-signature-regexp-alist))
	 )))

(defmacro mime-pgp-key-expected-regexp ()
  "Return a regexp to detect ``Key expected''."
  '(cdr (assq (mime-pgp-default-language)
	      (cdr (assq pgp-version mime-pgp-key-expected-regexp-alist))
	      )))

(defun mime-pgp-detect-version ()
  "Detect PGP version in the buffer.  The buffer is expected to be narrowed
to just an ascii armor.  However, a few leading garbage lines are allowed."
  (let ((version (save-restriction
		   (goto-char (point-min))
		   (if (re-search-forward "^-+BEGIN PGP " nil t)
		       (progn
			 (forward-line 1)
			 (narrow-to-region (point) (point-max))
			 (std11-narrow-to-header)
			 (std11-fetch-field "Version")
			 )))))
    (cond ((not version)
	   pgp-version)
	  ((string-match "GnuPG" version)
	   'gpg)
	  ((string-match "5\\.0i" version)
	   'pgp50)
	  ((string-match "2\\.6" version)
	   'pgp)
	  (t
	   pgp-version))))

(defun mime-entity-detect-pgp-version (entity)
  "Detect PGP version from entity content."
  (with-temp-buffer
    (mime-insert-entity-content entity)
    (mime-pgp-detect-version)
    ))

(defun mime-pgp-check-signature (output-buffer sig-file orig-file
					       &optional hide-lines)
  (with-current-buffer output-buffer
    (erase-buffer)
    (setq truncate-lines t))
  (let* ((lang (mime-pgp-default-language))
	 (command (mime-pgp-command 'v))
	 (args (cond ((eq 'gpg pgp-version)
		      (list "--batch" "--verify" sig-file)
		      )
		     ((eq 'pgp50 pgp-version)
		      (list "+batchmode=1"
			    (format "+language=%s" lang)
			    sig-file)
		      )
		     ((eq 'pgp pgp-version)
		      (list (format "+language=%s" lang)
			    sig-file orig-file))
		     ))
	 (good-regexp (mime-pgp-good-signature-regexp))
	 (good-post-function (mime-pgp-good-signature-post-function))
	 (bad-regexp (mime-pgp-bad-signature-regexp))
	 (bad-post-function (mime-pgp-bad-signature-post-function))
	 status start lines
	 )
    (setq status (apply 'call-process-region (point-min) (point-max)
			command nil output-buffer nil args)
	  )
    (with-current-buffer output-buffer
      (goto-char (point-min))
      (forward-line (or hide-lines 0))
      (setq start (point)
	    lines (count-lines start (point-max)))
      (cond ((not (stringp good-regexp))
	     (message "Please specify right regexp for specified language")
	     (cons start lines)
	     )
	    ((and (zerop status)
		  (progn
		    (goto-char (point-min))
		    (re-search-forward good-regexp nil t)
		    ))
	     (message (if good-post-function
			  (funcall good-post-function)
			(buffer-substring (match-beginning 0)
					  (match-end 0))))
	     (cons start lines)
	     )
	    ((not (stringp bad-regexp))
	     (message "Please specify right regexp for specified language")
	     (cons start lines)
	     )
	    ((progn
	       (goto-char (point-min))
	       (re-search-forward bad-regexp nil t)
	       )
	     (message (if bad-post-function
			  (funcall bad-post-function)
			(buffer-substring (match-beginning 0)
					  (match-end 0))))
	     (cons start lines)
	     )
	    (t
	     ;; Returns nil in order for attempt to fetch key.
	     nil
	     )))))

(defmacro mime-pgp-parse-verify-error (output-buffer &rest forms)
  (` (let ((regexp (mime-pgp-key-expected-regexp))
	   keyid)
       (with-current-buffer (, output-buffer)
	 (goto-char (point-min))
	 (if (stringp regexp)
	     (if (re-search-forward regexp nil t)
		 (progn
		   (setq keyid (concat
				"0x"
				(buffer-substring-no-properties
				 (match-beginning 1) (match-end 1))))
		   (goto-char (point-min))
		   ))
	   (message "Please specify right regexp for specified language")
	   )
	 (,@ forms)
	 (list keyid (point) (count-lines (point) (point-max)))
	 ))))

(defun mime-pgp-parse-verify-error-for-gpg (output-buffer)
  "Subroutine used for parsing verify error with GnuPG.  Returns the
list of expected key-ID, start position and lines to be shown a result."
  (mime-pgp-parse-verify-error output-buffer)
  )

(defun mime-pgp-parse-verify-error-for-pgp50 (output-buffer)
  "Subroutine used for parsing verify error with PGP 5.0i.  Returns the
list of expected key-ID, start position and lines to be shown a result."
  (mime-pgp-parse-verify-error output-buffer (forward-line 1))
  )

(defun mime-pgp-parse-verify-error-for-pgp (output-buffer)
  "Subroutine used for parsing verify error with PGP 2.6.  Returns the
list of expected key-ID, start position and lines to be shown a result."
  (mime-pgp-parse-verify-error
   output-buffer
   (if (search-forward "\C-g" nil t)
       (goto-char (match-beginning 0))
     (forward-line 7))
   ))

(defmacro mime-pgp-show-echo-buffer (start lines)
  (` (let ((mime-echo-window-height
	    (min (+ 2 (, lines))
		 (max window-min-height
		      (- (window-height) window-min-height)
		      ))))
       (set-window-start
	(car (save-current-buffer (mime-show-echo-buffer)))
	(, start))
       )))

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
	 (pgp-version (mime-entity-detect-pgp-version entity))
	 (output-buffer (get-buffer-create mime-echo-buffer-name))
	 (hide-lines (cdr (assq pgp-version
				'((gpg . nil) (pgp50 . 1) (pgp . 10)))))
	 (parser (intern (format "mime-pgp-parse-verify-error-for-%s"
				 pgp-version)))
	 done return pgp-id)
    (mime-write-entity orig-entity orig-file)
    (mime-write-entity-content entity sig-file)
    (message "Checking signature...")
    (unwind-protect
	(while (not done)
	  (if (setq return (mime-pgp-check-signature
			    output-buffer sig-file orig-file hide-lines))
	      (progn
		(mime-pgp-show-echo-buffer (car return) (cdr return))
		(setq done t)
		)
	    (if (and
		 (not pgp-id)
		 (progn
		   (setq return (funcall parser output-buffer))
		   (mime-pgp-show-echo-buffer (nth 1 return)
					      (nth 2 return))
		   (setq pgp-id (car return))
		   )
		 (y-or-n-p (format "Key %s not found; attempt to fetch? "
				   pgp-id))
		 )
		(funcall (pgp-function 'fetch-key) (cons nil pgp-id))
	      (setq return (funcall parser output-buffer))
	      (mime-pgp-show-echo-buffer (nth 1 return) (nth 2 return))
	      (setq done t)
	      (message "Can't check signature")
	      )))
      (delete-file orig-file)
      (delete-file sig-file)
      )))

(defun mime-pgp-good-signature-post-function-pgp50-us ()
  (forward-line 2)
  (looking-at "\\s +\\(.+\\)$")
  (format "Good signature from %s" (match-string 1)))

(defun mime-pgp-bad-signature-post-function-pgp50-us ()
  (forward-line 2)
  (looking-at "\\s +\\(.+\\)$")
  (format "BAD signature from %s" (match-string 1)))


;;; @ Internal method for application/pgp-encrypted
;;;
;;; It is based on RFC 2015 (PGP/MIME) and
;;; draft-yamamoto-openpgp-mime-00.txt (OpenPGP/MIME).

(defun mime-decrypt-application/pgp-encrypted (entity situation)
  (let* ((entity-node-id (mime-entity-node-id entity))
	 (mother (mime-entity-parent entity))
	 (knum (car entity-node-id))
	 (onum (if (> knum 0)
		   (1- knum)
		 (1+ knum)))
	 (orig-entity (nth onum (mime-entity-children mother)))
	 (pgp-version (mime-entity-detect-pgp-version orig-entity))
	 )
    (mime-view-application/pgp orig-entity situation)
    ))

(defun mime-edit-decrypt-application/pgp-encrypted ()
  "Decrypt the encrypted part for the function `mime-edit-again'."
  (let ((pgp-version (mime-pgp-detect-version)))
    ;; The following process should returns a pair (SUCCEEDED . VERIFIED)
    ;; where SUCCEEDED is t if the decryption succeeded and VERIFIED is t
    ;; if there was a valid signature.
    (as-binary-process (funcall (pgp-function 'decrypt)))
    ))


;;; @ Internal method for application/pgp-keys
;;;
;;; It is based on RFC 2015 (PGP/MIME) and
;;; draft-yamamoto-openpgp-mime-00.txt (OpenPGP/MIME).

(defun mime-add-application/pgp-keys (entity &optional situation)
  (with-temp-buffer
    (mime-insert-entity-content entity)
    (let ((pgp-version (mime-pgp-detect-version)))
      (funcall (pgp-function 'snarf-keys))
      )))


;;; @ Internal method for fetching a public key
;;;

(defcustom mime-pgp-keyserver-url-template "/pks/lookup?op=get&search=%s"
  "The URL to pass to the keyserver."
  :group 'mime-pgp
  :type 'string)

(defcustom mime-pgp-keyserver-address "pgp.nic.ad.jp"
  "Host name of keyserver."
  :group 'mime-pgp
  :type 'string)

(defcustom mime-pgp-keyserver-port 11371
  "Port on which the keyserver's HKP daemon lives."
  :group 'mime-pgp
  :type 'integer)

(defcustom mime-pgp-http-proxy-url-template
  "/cgi-bin/pgpsearchkey.pl?op=get&search=%s"
  "The URL to pass to the keyserver via HTTP proxy."
  :group 'mime-pgp
  :type 'string)

(defcustom mime-pgp-http-proxy-server-address nil
  "Host name of HTTP proxy server.  If you are behind firewalls, set the
values of this variable and `mime-pgp-http-proxy-server-port' appropriately."
  :group 'mime-pgp
  :type 'string)

(defcustom mime-pgp-http-proxy-server-port 8080
  "Port on which the proxy server's HTTP daemon lives."
  :group 'mime-pgp
  :type 'integer)

(defcustom mime-pgp-fetch-timeout 20
  "Timeout, in seconds, for any particular key fetch operation."
  :group 'mime-pgp
  :type 'integer)

(defmacro mime-pgp-show-fetched-key (key scroll &rest args)
  (` (let ((current-window (selected-window))
	   keys start hide return window)
       (with-temp-buffer
	 (insert (, key))
	 (as-binary-process
	  (call-process-region
	   (point-min) (point-max) (mime-pgp-command 'v) t t (,@ args))
	  )
	 (goto-char (point-min))
	 (forward-line (, scroll))
	 (setq keys (buffer-string)
	       start (point)
	       hide (count-lines (point) (point-max)))
	 )
       (setq return (mime-show-echo-buffer "%s" keys)
	     window (car return)
	     start (1- (+ start (car (cdr return))))
	     hide (- (window-height window) hide 2))
       (if (>= (+ (window-height current-window) hide) window-min-height)
	   (progn
	     (select-window window)
	     (shrink-window hide)
	     (select-window current-window)
	     ))
       (set-window-start window start)
       )))

(defun mime-pgp-show-fetched-key-for-gpg (key)
  "Extract KEY and show."
  (mime-pgp-show-fetched-key key 0)
  )

(defun mime-pgp-show-fetched-key-for-pgp50 (key)
  "Extract KEY and show."
  (let ((current-window (selected-window))
	(process-environment process-environment)
	process-connection-type process keys start hide return window)
    (setenv "PGPPASSFD" nil)
    (with-temp-buffer
      (setq process
	    (start-process "*show fetched keys*"
			   (current-buffer) (mime-pgp-command 'v)
			   "-f" "+batchmode=0" "+language=us")
	    )
      (set-process-coding-system process 'binary 'binary)
      (process-send-string process key)
      (process-send-eof process)
      (while
	  (progn
	    (accept-process-output process 1)
	    (goto-char (point-min))
	    (not
	     (re-search-forward
	      "^Add these keys to your keyring\\? \\[Y/n\\] "
	      nil t))
	    ))
      (setq start (match-beginning 0))
      (delete-process process)
      (delete-region start (point-max))
      (goto-char (point-min))
      (forward-line 10)
      (setq keys (buffer-string)
	    start (point)
	    hide (count-lines (point) start))
      )
    (setq return (mime-show-echo-buffer "%s" keys)
	  window (car return)
	  start (1- (+ start (car (cdr return))))
	  hide (- (window-height window) hide 2))
    (if (>= (+ (window-height current-window) hide) window-min-height)
	(progn
	  (select-window window)
	  (shrink-window hide)
	  (select-window current-window)
	  ))
    (set-window-start window start)
    ))

(defun mime-pgp-show-fetched-key-for-pgp (key)
  "Extract KEY and show."
  (mime-pgp-show-fetched-key key 7 "-f" "+language=en")
  )

(defun mime-pgp-fetch-key (&optional id)
  "Attempt to fetch a key for addition to PGP or GnuPG keyring.
Interactively, prompt for string matching key to fetch.

Non-interactively, ID must be a pair.  The CAR must be a bare Email
address and the CDR a keyID (with \"0x\" prefix).  Either, but not
both, may be nil.

Return t if we think we were successful; nil otherwise.  Note that nil
is not necessarily an error, since we may have merely fired off an Email
request for the key.

If you want to use this function for verifying a message of PGP/MIME,
for example, please put the following lines in your startup file:

  (eval-after-load \"semi-def\"
    '(progn (require 'alist)
	    (set-alist 'pgp-function-alist 'fetch-key
		       '(mime-pgp-fetch-key \"mime-pgp\"))
	    (autoload 'mime-pgp-fetch-key \"mime-pgp\" nil t)
	    ))

In addition, if you are behind firewalls, please set the values of
`mime-pgp-http-proxy-server-address' and `mime-pgp-http-proxy-server-port'
appropriately."
  (interactive)
  (let ((server (or mime-pgp-http-proxy-server-address
		    mime-pgp-keyserver-address))
	(port (or mime-pgp-http-proxy-server-port
		  mime-pgp-keyserver-port))
	(url-template
	 (if mime-pgp-http-proxy-server-address
	     (concat "http://"
		     mime-pgp-keyserver-address
		     mime-pgp-http-proxy-url-template
		     " HTTP/1.0\r\n")
	   mime-pgp-keyserver-url-template))
	(show-function (intern (format "mime-pgp-show-fetched-key-for-%s"
				       pgp-version)))
	(snarf-function (pgp-function 'snarf-keys))
	armor case-fold-search process-connection-type process start)
    (if (cond ((interactive-p)
	       (setq id (read-string "Fetch key for: "))
	       (cond ((string-equal "" id)
		      (message "Aborted")
		      nil)
		     ((string-match "^0[Xx]" id)
		      (setq id (cons nil id)))
		     (t
		      (setq id (cons id nil)))))
	      ((or (null id)
		   (not (or (stringp (car id)) (stringp (cdr id)))))
	       (message "Aborted")
	       nil)
	      (t t))
	(progn
	  (with-temp-buffer
	    (catch 'mime-pgp-fetch-key-done
	      (message "Fetching %s via HTTP to %s..."
		       (or (cdr id) (car id))
		       mime-pgp-keyserver-address)
	      (condition-case err
		  (setq process (open-network-stream-as-binary
				 "*key fetch*" (current-buffer) server port))
		(error
		 (message "%s" err)
		 (throw 'mime-pgp-fetch-key-done nil)
		 ))
	      (if (not process)
		  (progn
		    (message "Can't connect to %s%s."
			     mime-pgp-keyserver-address
			     (if mime-pgp-http-proxy-server-address
				 (concat "via "
					 mime-pgp-http-proxy-server-address)
			       ""))
		    (throw 'mime-pgp-fetch-key-done nil)
		    )
		(process-send-string
		 process
		 (concat "GET " (format url-template
					(or (cdr id) (car id))) "\r\n")
		 )
		(while (and (eq 'open (process-status process))
			    (accept-process-output process
						   mime-pgp-fetch-timeout)
			    ))
		(delete-process process)
		(goto-char (point-min))
		(if (and (re-search-forward
			  "^-----BEGIN PGP PUBLIC KEY BLOCK-----\r?$" nil t)
			 (setq start (match-beginning 0))
			 (re-search-forward
			  "^-----END PGP PUBLIC KEY BLOCK-----\r?$" nil t)
			 )
		    (setq armor (buffer-substring start (1+ (point))))
		  ))))
	  (if armor
	      (save-window-excursion
		(funcall show-function armor)
		(if (y-or-n-p "Add this key to keyring? ")
		    (if (zerop
			 (with-temp-buffer
			   (insert armor)
			   ;; The function should return a number of
			   ;; keys found.
			   (funcall snarf-function)))
			(progn
			  (message "Key not found or discarded.")
			  nil)
		      t)
		  (message "Aborted")
		  nil))
	    (message "Key not found.")
	    nil)))))


;;; @ end
;;;

(provide 'mime-pgp)

(run-hooks 'mime-pgp-load-hook)

;;; mime-pgp.el ends here
