;;; mime-setup.el --- setup file for MIME viewer and composer.

;; Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version:
;;	$Id$
;; Keywords: MIME, multimedia, multilingual, mail, news

;; This file is part of SEMI (SEMI is Emacs MIME Interfaces).

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

(require 'semi-setup)
(require 'alist)

(autoload 'turn-on-mime-edit "mime-edit"
  "Unconditionally turn on MIME-Edit minor mode." t)

(autoload 'eword-decode-header "eword-decode"
  "Decode MIME encoded-words in header fields." t)

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


;;; @ variables
;;;

(defvar mime-setup-use-signature t
  "If it is not nil, mime-setup sets up to use signature.el.
\[mime-setup.el]")

(defvar mime-setup-default-signature-key "\C-c\C-s"
  "*Key to insert signature. [mime-setup.el]")

(defvar mime-setup-signature-key-alist '((mail-mode . "\C-c\C-w"))
  "Alist of major-mode vs. key to insert signature. [mime-setup.el]")


;;; @ for signature
;;;

(defun mime-setup-set-signature-key ()
  (let ((key (or (cdr (assq major-mode mime-setup-signature-key-alist))
		 mime-setup-default-signature-key)))
    (define-key (current-local-map) key (function insert-signature))
    ))

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


;;; @ for mail-mode, RMAIL and VM
;;;

(add-hook 'mail-setup-hook 'eword-decode-header)
(add-hook 'mail-setup-hook 'turn-on-mime-edit 'append)
(add-hook 'mail-send-hook  'mime-edit-maybe-translate)
(set-alist 'mime-edit-split-message-sender-alist
           'mail-mode (function
                       (lambda ()
                         (interactive)
                         (funcall send-mail-function)
                         )))

;;; @ for mh-e
;;;

(defun mime-setup-mh-draft-setting ()
  (turn-on-mime-edit)
  (make-local-variable 'mail-header-separator)
  (setq mail-header-separator "--------")
  (save-excursion
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (if (re-search-forward "^-*$" nil t)
	(progn
	  (replace-match mail-header-separator)
	  (set-buffer-modified-p (buffer-modified-p))
	  ))
    ))

(add-hook 'mh-letter-mode-hook 'mime-setup-mh-draft-setting t)
(add-hook 'mh-before-send-letter-hook 'mime-edit-maybe-translate)


;;; @ for message (September Gnus 0.58 or later)
;;;

(defun message-maybe-setup-default-charset ()
  (let ((charset
	 (and (boundp 'gnus-summary-buffer)
              (buffer-live-p gnus-summary-buffer)
	      (save-excursion
		(set-buffer gnus-summary-buffer)
		default-mime-charset))))
    (if charset
	(progn
	  (make-local-variable 'default-mime-charset)
	  (setq default-mime-charset charset)
	  ))))

(add-hook 'message-setup-hook  'turn-on-mime-edit)
(add-hook 'message-setup-hook  'message-maybe-setup-default-charset)
(add-hook 'message-send-hook   'mime-edit-maybe-translate)
(add-hook 'message-header-hook 'eword-encode-header)

(call-after-loaded
 'message
 (function
  (lambda ()
    (require 'message-mime)
    )))


;;; @ end
;;;

(provide 'mime-setup)

(run-hooks 'mime-setup-load-hook)

;;; mime-setup.el ends here
