;;; pgg-gpg.el --- GnuPG support for PGG.

;; Copyright (C) 1999 Daiki Ueno

;; Author: Daiki Ueno <ueno@ueda.info.waseda.ac.jp>
;; Created: 1999/10/28
;; Keywords: PGP, OpenPGP, GnuPG

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

(eval-when-compile (require 'pgg))

(defgroup pgg-gpg ()
  "GnuPG interface"
  :group 'pgg)

(defcustom pgg-gpg-program "gpg" 
  "The GnuPG executable."
  :group 'pgg-gpg
  :type 'string)

(defcustom pgg-gpg-shell-file-name "/bin/sh"
  "The GnuPG executable."
  :group 'pgg-gpg
  :type 'string)

(defcustom pgg-gpg-extra-args nil
  "Extra arguments for every GnuPG invocation."
  :group 'pgg-gpg
  :type 'string)

(eval-and-compile
  (luna-define-class pgg-scheme-gpg (pgg-scheme))
  )
  
(defvar pgg-gpg-user-id nil
  "GnuPG ID of your default identity.")

(defvar pgg-scheme-gpg-instance nil)

;;;###autoload
(defun pgg-make-scheme-gpg ()
  (or pgg-scheme-gpg-instance
      (setq pgg-scheme-gpg-instance
	    (luna-make-entity 'pgg-scheme-gpg))))

(defun pgg-gpg-process-region (start end passphrase program args)
  (let* ((errors-file-name
	  (concat temporary-file-directory 
		  (make-temp-name "pgg-errors")))
	 (status-file-name
	  (concat temporary-file-directory 
		  (make-temp-name "pgg-status")))
	 (args 
	  (append
	   `("--status-fd" "3"
	     ,@(if passphrase '("--passphrase-fd" "0"))
	     ,@pgg-gpg-extra-args)
	   args
	   (list (concat "2>" errors-file-name)
		 (concat "3>" status-file-name))))
	 (shell-file-name pgg-gpg-shell-file-name)
	 (output-buffer pgg-output-buffer)
	 (errors-buffer pgg-errors-buffer)
	 (status-buffer pgg-status-buffer)
	 (process-connection-type nil)
	 process status exit-status)
    (with-current-buffer (get-buffer-create output-buffer)
      (buffer-disable-undo)
      (erase-buffer))
    (setq process
	  (apply #'start-process-shell-command "*GnuPG*" output-buffer
		 program args))
    (set-process-sentinel process 'ignore)
    (when passphrase
      (process-send-string process (concat passphrase "\n")))
    (process-send-region process start end)
    (process-send-eof process)
    (while (eq 'run (process-status process))
      (accept-process-output process 5))
    (setq status (process-status process)
	  exit-status (process-exit-status process))
    (delete-process process)
    (with-current-buffer output-buffer
      (goto-char (point-min))
      (while (search-forward "\r$" nil t)
	(replace-match ""))
      (if (memq status '(stop signal))
	  (error "%s exited abnormally: '%s'" program exit-status))
      (if (= 127 exit-status)
	  (error "%s could not be found" program))

      (set-buffer (get-buffer-create errors-buffer))
      (buffer-disable-undo)
      (erase-buffer)
      (insert-file-contents errors-file-name)
      (delete-file errors-file-name)
      
      (set-buffer (get-buffer-create status-buffer))
      (buffer-disable-undo)
      (erase-buffer)
      (insert-file-contents status-file-name)
      (delete-file status-file-name)

      (if (and process (eq 'run (process-status process)))
	  (interrupt-process process))
      )
    ))

(luna-define-method lookup-key-string ((scheme pgg-scheme-gpg)
				       string &optional type)
  (let ((args (list "--with-colons" "--no-greeting" "--batch" 
		    (if type "--list-secret-keys" "--list-keys")
		    string)))
    (pgg-gpg-process-region (point)(point) nil pgg-gpg-program args)
    (with-current-buffer pgg-output-buffer
      (goto-char (point-min))
      (when (re-search-forward "^\\(sec\\|pub\\):"  nil t)
	(substring 
	 (nth 3 (split-string 
		 (buffer-substring (match-end 0)
				   (progn (end-of-line)(point)))
		 ":"))
	 8)))
    ))

(luna-define-method encrypt-region ((scheme pgg-scheme-gpg) 
				    start end recipients)
  (let* ((pgg-gpg-user-id pgg-default-user-id)
	 (passphrase
	  (pgg-read-passphrase 
	   (format "GnuPG passphrase for %s: " pgg-gpg-user-id)
	   (luna-send scheme 'lookup-key-string
		      scheme pgg-gpg-user-id 'encrypt)))
	 (args 
	  `("--batch" "--armor" "--textmode" "--always-trust" "--encrypt"
	    ,@(if recipients
		  (apply #'append 
			 (mapcar (lambda (rcpt) 
				   (list "--remote-user" 
					 (concat "\"" rcpt "\""))) 
				 recipients))))))
    (pgg-gpg-process-region start end passphrase pgg-gpg-program args)
    (with-current-buffer pgg-output-buffer
      (if (zerop (buffer-size))
	  (insert-buffer-substring pgg-errors-buffer)
	(let ((packet 
	       (cdr (assq 1 (pgg-parse-armor-region 
			     (point-min)(point-max))))))
	  (pgg-add-passphrase-cache 
	   (cdr (assq 'key-identifier packet))
	   passphrase))))
    ))

(luna-define-method decrypt-region ((scheme pgg-scheme-gpg) 
				    start end)
  (let* ((pgg-gpg-user-id pgg-default-user-id)
	 (passphrase
	  (pgg-read-passphrase 
	   (format "GnuPG passphrase for %s: " pgg-gpg-user-id)
	   (luna-send scheme 'lookup-key-string 
		      scheme pgg-gpg-user-id 'encrypt)))
	 (args '("--batch" "--decrypt")))
    (pgg-gpg-process-region start end passphrase pgg-gpg-program args)
    (with-current-buffer pgg-output-buffer
      (when (zerop (buffer-size))
	(insert-buffer-substring pgg-errors-buffer)))
    ))

(luna-define-method sign-region ((scheme pgg-scheme-gpg) 
				 start end)
  (let* ((pgg-gpg-user-id pgg-default-user-id)
	 (passphrase
	  (pgg-read-passphrase 
	   (format "GnuPG passphrase for %s: " pgg-gpg-user-id)
	   (luna-send scheme 'lookup-key-string 
		      scheme pgg-gpg-user-id 'sign)))
	 (args 
	  (list "--detach-sign" "--armor" "--batch" "--verbose" 
		"--local-user" pgg-gpg-user-id)))
    (goto-char start)
    (setq end (set-marker (make-marker) (point-max)))
    (while (progn (end-of-line) (> (marker-position end) (point)))
      (insert "\r")
      (forward-line 1))
    (pgg-gpg-process-region start end passphrase pgg-gpg-program args)
    (goto-char start)
    (while (re-search-forward "\r$" end t)
      (replace-match ""))
    (with-current-buffer pgg-output-buffer
      (if (zerop (buffer-size))
	  (insert-buffer-substring pgg-errors-buffer)
	(let ((packet 
	       (cdr (assq 2 (pgg-parse-armor-region 
			     (point-min)(point-max))))))
	  (pgg-add-passphrase-cache 
	   (cdr (assq 'key-identifier packet))
	   passphrase))))
    ))

(luna-define-method verify-region ((scheme pgg-scheme-gpg) 
				   start end &optional signature)
  (let ((args '("--batch" "--verify")))
    (when (stringp signature)
      (setq args (append args (list signature))))
    (pgg-gpg-process-region start end nil pgg-gpg-program args)
    (set-buffer pgg-errors-buffer)
    (goto-char (point-min))
    (while (re-search-forward "^gpg: " nil t)
      (replace-match ""))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^warning: " nil t)
	(delete-region (match-beginning 0)
		       (progn (beginning-of-line 2) (point)))))
    (append-to-buffer pgg-output-buffer
		      (point-min)(point-max))
    ))

(luna-define-method insert-key ((scheme pgg-scheme-gpg))
  (let* ((pgg-gpg-user-id pgg-default-user-id)
	 (args (list "--batch" "--export" "--armor" 
		     (concat "\"" pgg-gpg-user-id "\""))))
    (pgg-gpg-process-region (point)(point) nil pgg-gpg-program args)
    (insert-buffer-substring pgg-output-buffer)
    ))

(luna-define-method snarf-keys-region ((scheme pgg-scheme-gpg)
				       start end)
  (let ((args '("--import" "--batch")) status)
    (pgg-gpg-process-region start end nil pgg-gpg-program args)
    (set-buffer pgg-status-buffer)
    (goto-char (point-min))
    (when (re-search-forward "^\\[GNUPG:] +IMPORT_RES +" nil t)
      (setq status (buffer-substring (match-end 0) 
				     (progn (end-of-line) 
					    (point)))
	    status (vconcat (split-string status)))
      (erase-buffer)
      (insert (aref status 0) "keys seen\n"
	      (format "\t%d bad, %d new, %d old\n"
		      (string-to-int (aref status 1))
		      (+ (string-to-int (aref status 2))
			 (string-to-int (aref status 10)))
		      (+ (string-to-int (aref status 4))
			 (string-to-int (aref status 11))))
	      (if (zerop (aref status 9))
		  ""
		"\tSecret keys are imported\n")))
    (append-to-buffer pgg-output-buffer
		      (point-min)(point-max))
    (with-current-buffer pgg-output-buffer
      (when (zerop (buffer-size))
	(insert-buffer-substring pgg-errors-buffer)))
    ))

(provide 'pgg-gpg)

;;; pgg-gpg.el ends here

