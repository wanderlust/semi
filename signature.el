;;;
;;; signature.el --- a signature utility for GNU Emacs
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1994 .. 1996 MORIOKA Tomohiko
;;; Copyright (C) 1994 OKABE Yasuo
;;; Copyright (C) 1996 Artur Pioro
;;; Copyright (C) 1996 KOBAYASHI Shuhei
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;;         OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp>
;;;         Artur Pioro <artur@flugor.if.uj.edu.pl>
;;;         KOBAYASHI Shuhei <shuhei-k@jaist.ac.jp>
;;; Maintainer: KOBAYASHI Shuhei <shuhei-k@jaist.ac.jp>
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

;;; @ valiables
;;;

(defvar signature-insert-at-eof nil
  "*Insert signature at the end of file if non-nil.")

(defvar signature-delete-blank-lines-at-eof nil
  "*If non-nil, signature-insert-at-eof deletes blank lines at the end
of file.")

(defvar signature-load-hook nil
  "*List of functions called after signature.el is loaded.")

(defvar signature-file-name "~/.signature"
  "*Name of file containing the user's signature.")

(defvar signature-file-alist nil)

(defvar signature-file-prefix nil
  "*String containing optional prefix for the signature file names")

(defvar signature-insert-hook nil
  "*List of functions called before inserting a signature.")

(defvar signature-use-bbdb nil
  "*If non-nil, Register sigtype to BBDB.")

;;;
;;; Example:
;;;
;;; (setq signature-file-alist
;;;       '((("Newsgroups" . "zxr")   . "~/.signature-sun")
;;;         (("To" . "uramimi")       . "~/.signature-sun")
;;;         (("Newsgroups" . "jokes") . "~/.signature-jokes")
;;;         (("To" . "tea")           . "~/.signature-jokes")
;;;         (("To" . ("sim" "oku"))   . "~/.signature-formal")
;;;         ))

(autoload 'signature/get-sigtype-from-bbdb "tm-bbdb")

(defun signature/get-sigtype-interactively (&optional default)
  (read-file-name "Insert your signature: "
                  (or default (concat signature-file-name "-"))
                  (or default signature-file-name)
                  nil))

(defun signature/get-signature-file-name ()
  (save-excursion
    (save-restriction
      (narrow-to-region
       (goto-char (point-min))
       (if (re-search-forward
            (concat "^" (regexp-quote mail-header-separator) "$")
            nil t)
           (match-beginning 0)
         (point-max)
         ))
      (catch 'found
        (let ((alist signature-file-alist) cell field value)
          (while alist
            (setq cell  (car alist)
                  field (rfc822/get-field-body (car (car cell)))
                  value (cdr (car cell)))
            (cond ((functionp value)
		   (let ((name (apply value field (cdr cell))))
		     (if name
			 (throw 'found
				(concat signature-file-prefix name))
		       )))
		  ((stringp field)
		   (cond ((consp value)
			  (while value
			    (if (string-match (car value) field)
				(throw 'found
				       (concat
					signature-file-prefix (cdr cell)))
			      (setq value (cdr value))
			      )))
			 ((stringp value)
			  (if (string-match value field)
			      (throw 'found
				     (concat
				      signature-file-prefix (cdr cell)))
			    )))))
            (setq alist (cdr alist))
            ))
        signature-file-name))))

(defun insert-signature (&optional arg)
  "Insert the file named by signature-file-name.
It is inserted at the end of file if signature-insert-at-eof in non-nil,
and otherwise at the current point.  A prefix argument enables user to
specify a file named <signature-file-name>-DISTRIBUTION interactively."
  (interactive "P")
  (let ((signature-file-name
         (expand-file-name
          (or (and signature-use-bbdb
                   (signature/get-sigtype-from-bbdb arg))
              (and arg
                   (signature/get-sigtype-interactively))
              (signature/get-signature-file-name))
          )))
    (or (file-readable-p signature-file-name)
        (error "Cannot open signature file: %s" signature-file-name))
    (if signature-insert-at-eof
        (progn
          (goto-char (point-max))
          (or (bolp) (insert "\n"))
          (or signature-delete-blank-lines-at-eof (delete-blank-lines))
          ))
    (run-hooks 'signature-insert-hook)
    (insert-file-contents signature-file-name)
    (force-mode-line-update)
    signature-file-name))


;;; @ end
;;;

(provide 'signature)

(run-hooks 'signature-load-hook)

;;; signature.el ends here
