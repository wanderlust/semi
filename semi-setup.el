;;; semi-setup.el --- setup file for MIME-View.

;; Copyright (C) 1994,1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: mail, news, MIME, multimedia, multilingual, encoded-word

;; This file is part of SEMI (Setting for Emacs MIME Interfaces).

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

(require 'mime-def)
(require 'path-util)


;; for image/* and X-Face
(defvar mime-setup-enable-inline-image
  (and window-system
       (or running-xemacs
	   (and (featurep 'mule)(module-installed-p 'bitmap))
	   ))
  "*If it is non-nil, semi-setup sets up to use mime-image.")

(if mime-setup-enable-inline-image
    (call-after-loaded 'mime-view
		       (function
			(lambda ()
			  (require 'mime-image)
			  )))
  )


(defvar mime-setup-enable-pgp
  (module-installed-p 'mailcrypt)
  "*If it is non-nil, semi-setup sets uf to use mime-pgp.")

;; for PGP
(if mime-setup-enable-pgp
    (call-after-loaded 'mime-view
		       (function
			(lambda ()
			  (require 'mime-pgp)
			  )))
  )


;;; @ for mime-edit
;;;

(defun mime-setup-decode-message-header ()
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (narrow-to-region
       (point-min)
       (if (re-search-forward
	    (concat "^" (regexp-quote mail-header-separator) "$")
	    nil t)
	   (match-beginning 0)
	 (point-max)
	 ))
      (eword-decode-header)
      (set-buffer-modified-p nil)
      )))

(add-hook 'mime-edit-mode-hook 'mime-setup-decode-message-header)


;;; @@ variables
;;;

(defvar mime-setup-use-signature t
  "If it is not nil, mime-setup sets up to use signature.el.")

(defvar mime-setup-default-signature-key "\C-c\C-s"
  "*Key to insert signature.")

(defvar mime-setup-signature-key-alist '((mail-mode . "\C-c\C-w"))
  "Alist of major-mode vs. key to insert signature.")


;;; @@ for signature
;;;

(defun mime-setup-set-signature-key ()
  (let ((keymap (current-local-map)))
    (if keymap
	(let ((key
	       (or (cdr (assq major-mode mime-setup-signature-key-alist))
		   mime-setup-default-signature-key)))
	  (define-key keymap key (function insert-signature))
	  ))))

(if mime-setup-use-signature
    (progn
      (autoload 'insert-signature "signature" "Insert signature" t)
      (add-hook 'mime-edit-mode-hook 'mime-setup-set-signature-key)
      (setq gnus-signature-file nil)
      (setq mail-signature nil)
      (setq message-signature nil)
      ))


;;; @ for mu-cite
;;;

(add-hook 'mu-cite/pre-cite-hook 'eword-decode-header)


;;; @ end
;;;

(provide 'semi-setup)

;;; semi-setup.el ends here
