;;;
;;; signature.el --- a signature utility for GNU Emacs
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1994 .. 1996 MORIOKA Tomohiko
;;; Copyright (C) 1994 OKABE Yasuo
;;; Copyright (C) 1996 Artur Pioro
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;;         OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp>
;;;         Artur Pioro <artur@flugor.if.uj.edu.pl>
;;; Created: 1994/7/11
;;; Version:
;;;	$Id$
;;; Keywords: mail, news, signature
;;;
;;; This file is part of tm (Tools for MIME).
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with This program.  If not, write to the Free Software
;;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Code:

(require 'tl-822)

(defvar signature-insert-at-eof nil
  "*Insert signature at the end of file if non-nil. [signature.el]")

(defvar signature-delete-blank-lines-at-eof nil
  "*Signature-insert-at-eof deletes blank lines at the end of file
if non-nil. [signature.el]")

(defvar signature-file-name "~/.signature"
  "*Name of file containing the user's signature. [signature.el]")

(defvar signature-file-alist nil)

(defvar signature-file-prefix nil
  "*String containing optional prefix for the signature file names")

;;;
;;; Example:
;;;
;;; (setq signature-file-alist
;;;	  '((("To" . signature-check-in-bbdb) . nil)
;;;	    (("Newsgroups" . "zxr")   . "~/.signature-sun")
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
			       (throw 'tag
				      (concat
				       signature-file-prefix (cdr cell)))
			     )
			   (setq r (cdr r))
			   ))
		       )
		      ((stringp (cdr b))
		       (if (string-match (cdr b) f)
			   (throw 'tag
				  (concat
				   signature-file-prefix (cdr cell)))
			 ))
		      ((functionp (cdr b))
		       (let ((name (apply (cdr b) f (cdr cell))))
			 (if name
			     (throw 'tag
				    (concat signature-file-prefix name))
			   )))
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
    (if (file-readable-p signature)
	(progn
	  (goto-char (point-max))
	  (if (not (bolp))
	      (insert "\n"))
	  (if signature-delete-blank-lines-at-eof (delete-blank-lines))
	  (insert-file-contents signature)
	  (set-buffer-modified-p (buffer-modified-p))
					; force mode line update
	  ))
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

(defun signature-check-in-bbdb (address)
  "Returns 'sigtype field from BBDB for user specified by ADDRESS"
  (require 'bbdb)
  (require 'bbdb-com)
  (let ((addr-comp (mail-extract-address-components address))
	full-name net-name records record sigtype)
    (setq full-name (car addr-comp))
    (setq net-name (mapconcat (lambda (x) x) (cdr addr-comp) "\\|"))
    (setq records
	  (or
	   (and full-name
		(bbdb-search (bbdb-records) full-name))
	   (and net-name
		(bbdb-search (bbdb-records) nil nil net-name))))
    (setq record (car records))
    (setq records (cdr records))
    (setq sigtype (and record (bbdb-record-getprop record 'sigtype)))
    (while (and (not sigtype) records)
      (setq record (car records))
      (setq records (cdr records))
      (setq sigtype (bbdb-record-getprop record 'sigtype)))
    (if sigtype
	(message (concat "Using signature for: "
			 (bbdb-record-firstname record) " "
			 (bbdb-record-lastname record)
			 (and (bbdb-record-aka record)
			      (concat " (AKA: "
				      (car (bbdb-record-aka record))
				      ")"))
			 " <" (car (bbdb-record-net record)) ">")))
    sigtype))


;;; @ end
;;;

(provide 'signature)

;;; signature.el ends here
