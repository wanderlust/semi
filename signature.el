;;;
;;; signature.el --- signature utility for GNU Emacs
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1994,1995 MORIOKA Tomohiko
;;; Copyright (C) 1994 OKABE Yasuo
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;;         OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp> (1994/08/01)
;;; Version:
;;;	$Id$
;;; Keywords: mail, news, signature
;;;
;;; This file is part of tm (Tools for MIME).
;;;

(require 'tl-822)

(defvar signature-insert-at-eof nil
  "*Insert signature at the end of file if non-nil.")

(defvar signature-file-name "~/.signature"
  "*Name of file containing the user's signature.")

(defvar signature-file-alist nil)

;;;
;;; Example:
;;;
;;; (setq signature-file-alist
;;;	  '((("Newsgroups" . "zxr")   . "~/.signature-sun")
;;;	    (("To" . "uramimi")	      . "~/.signature-sun")
;;;	    (("Newsgroups" . "jokes") . "~/.signature-jokes")
;;;	    (("To" . "tea")	      . "~/.signature-jokes")
;;;	    (("To" . ("sim" "oku"))   . "~/.signature-formal")
;;;	    ))

(defun signature/get-signature-file-name ()
  (catch 'tag
    (let ((r signature-file-alist) cell b f)
      (save-excursion
	(save-restriction
	  (narrow-to-region
	   (point-min)
	   (progn
	     (goto-char (point-min))
	     (if (re-search-forward
		  (concat "^" (regexp-quote mail-header-separator) "$")
		  nil t)
		 (match-beginning 0)
	       (point-max)
	       )))
	  (while r
	    (setq cell (car r))
	    (setq b (car cell))
	    (if (setq f (rfc822/get-field-body (car b)))
		(cond ((listp (cdr b))
		       (let ((r (cdr b)))
			 (while r
			   (if (string-match (car r) f)
			       (throw 'tag (cdr cell))
			     )
			   (setq r (cdr r))
			   ))
		       )
		      ((stringp (cdr b))
		       (if (string-match (cdr b) f)
			   (throw 'tag (cdr cell))
			 ))
		      ))
	    (setq r (cdr r))
	    ))
	signature-file-name))))

(defun signature/insert-signature-at-point (&optional arg)
  "Insert the file named by signature-file-name at the current point."
  (interactive "P")
  (let ((signature
	 (expand-file-name
	  (if arg
	      (read-file-name "Insert your signature: "
			      (concat signature-file-name "-")
			      signature-file-name
			      nil)
	    (signature/get-signature-file-name)))))
    (insert-file-contents signature)
    (set-buffer-modified-p (buffer-modified-p)) ; force mode line update
    signature))

(defun signature/insert-signature-at-eof (&optional arg)
  "Insert the file named by signature-file-name at the end of file."
  (interactive "P")
  (let ((signature
	 (expand-file-name
	  (if arg
	      (read-file-name "Insert your signature: "
			      (concat signature-file-name "-")
			      signature-file-name
			      nil)
	    (signature/get-signature-file-name)))))
    (save-excursion
      (if (file-readable-p signature)
	  (progn
	    (goto-char (point-max))
	    (if (not (bolp))
		(insert "\n"))
	    (delete-blank-lines)
	    (insert-file-contents signature)
	    (set-buffer-modified-p (buffer-modified-p))
					; force mode line update
	    )))
    signature))

(defun insert-signature (&optional arg)
  "Insert the file named by signature-file-name.  It is inserted at the
end of file if signature-insert-at-eof in non-nil, and otherwise at
the current point.  A prefix argument enables user to specify a file
named <signature-file-name>-DISTRIBUTION interactively."
  (interactive "P")
  (if signature-insert-at-eof
	(call-interactively 'signature/insert-signature-at-eof)
    (call-interactively 'signature/insert-signature-at-point)))


;;; @ end
;;;

(provide 'signature)
