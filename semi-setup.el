;;; semi-setup.el --- setup file for MIME-View.

;; Copyright (C) 1994,95,96,97,98,99,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
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

(require 'semi-def)

;; for image/*
(defvar mime-setup-enable-inline-image
  (if (featurep 'xemacs)
      (console-on-window-system-p)
    window-system)
  "*If it is non-nil, semi-setup sets up to use mime-image.")

(eval-after-load "mime-view"
  '(if mime-setup-enable-inline-image
       (require 'mime-image)))

;; for text/html
(defvar mime-setup-enable-inline-html
  (locate-library "w3")
  "*If it is non-nil, semi-setup sets up to use mime-w3.")

(eval-after-load "mime-view"
  '(when mime-setup-enable-inline-html
     (autoload 'mime-preview-text/html "mime-w3")
     (ctree-set-calist-strictly
      'mime-preview-condition
      '((type . text)(subtype . html)
	(body . visible)
	(body-presentation-method . mime-preview-text/html)))
	 
     (set-alist 'mime-view-type-subtype-score-alist
		'(text . html) 3)))

;; for text/x-vcard
(defvar mime-setup-enable-vcard
  (locate-library "vcard")
  "*If it is non-nil, semi-setup sets uf to use mime-vcard.")

(eval-after-load "mime-view"
  '(when mime-setup-enable-vcard
     (autoload 'mime-display-text/x-vcard "mime-vcard")

     (mime-add-condition
      'preview 
      '((type . text)(subtype . x-vcard)
	(body . visible)
	(body-presentation-method . mime-display-text/x-vcard))
      'strict)

     (set-alist 'mime-view-type-subtype-score-alist
		'(text . x-vcard) 3)))

;; for PGP
(defvar mime-setup-enable-pgp t
  "*If it is non-nil, semi-setup sets uf to use mime-pgp.")

(eval-after-load "mime-view"
  '(when mime-setup-enable-pgp
     (mime-add-condition
      'preview '((type . application)(subtype . pgp)
		 (message-button . visible)))
     (mime-add-condition
      'action '((type . application)(subtype . pgp)
		(method . mime-view-application/pgp))
      'strict "mime-pgp")
     (mime-add-condition
      'action '((type . text)(subtype . x-pgp)
		(method . mime-view-application/pgp)))
	 
     (mime-add-condition
      'action '((type . multipart)(subtype . signed)
		(method . mime-verify-multipart/signed))
      'strict "mime-pgp")
	 
     (mime-add-condition
      'action
      '((type . application)(subtype . pgp-signature)
	(method . mime-verify-application/pgp-signature))
      'strict "mime-pgp")
	 
     (mime-add-condition
      'action
      '((type . application)(subtype . pgp-encrypted)
	(method . mime-decrypt-application/pgp-encrypted))
      'strict "mime-pgp")
	 
     (mime-add-condition
      'action
      '((type . application)(subtype . pgp-keys)
	(method . mime-add-application/pgp-keys))
      'strict "mime-pgp")

     (mime-add-condition
      'action
      '((type . application)(subtype . pkcs7-signature)
	(method . mime-verify-application/pkcs7-signature))
      'strict "mime-pgp")

     (mime-add-condition
      'action
      '((type . application)(subtype . x-pkcs7-signature)
	(method . mime-verify-application/pkcs7-signature))
      'strict "mime-pgp")

     (mime-add-condition
      'action
      '((type . application)(subtype . pkcs7-mime)
	(method . mime-view-application/pkcs7-mime))
      'strict "mime-pgp")

     (mime-add-condition
      'action
      '((type . application)(subtype . x-pkcs7-mime)
	(method . mime-view-application/pkcs7-mime))
      'strict "mime-pgp")))


;;; @ for mime-edit
;;;

;; (defun mime-setup-decode-message-header ()
;;   (save-excursion
;;     (save-restriction
;;       (goto-char (point-min))
;;       (narrow-to-region
;;        (point-min)
;;        (if (re-search-forward
;;             (concat "^" (regexp-quote mail-header-separator) "$")
;;             nil t)
;;            (match-beginning 0)
;;          (point-max)
;;          ))
;;       (mime-decode-header-in-buffer)
;;       (set-buffer-modified-p nil)
;;       )))

;; (add-hook 'mime-edit-mode-hook 'mime-setup-decode-message-header)


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
	  (define-key keymap key (function insert-signature))))))

(when mime-setup-use-signature
  (autoload 'insert-signature "signature" "Insert signature" t)
  (add-hook 'mime-edit-mode-hook 'mime-setup-set-signature-key)
  ;; (setq message-signature nil)
  )


;;; @ for mu-cite
;;;

;; (add-hook 'mu-cite/pre-cite-hook 'eword-decode-header)


;;; @ end
;;;

(provide 'semi-setup)

;;; semi-setup.el ends here
