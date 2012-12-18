;;; mime-pgp.el --- mime-view internal methods for EasyPG.

;; Copyright (C) 1995,1996,1997,1998,1999,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;;	Daiki Ueno <ueno@unixuser.org>
;;	Kazuhiro Ito <kzhr@d1.dion.ne.jp>
;; Created: 1995/12/7
;;	Renamed: 1997/2/27 from tm-pgp.el
;;	Renamed: 2010/11/27 from mime-pgp.el in emiko-epg
;;	Renamed: 2012/10/05 from mime-epg.el
;; Keywords: PGP, security, MIME, multimedia, mail, news

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'mime-play)
(require 'epg)
(require 'epa)

(defcustom mime-pgp-decrypt-when-preview nil
  "When non-nil, decrypt encrypted part while viewing."
  :group 'mime-view
  :type 'boolean)

;;; @ Internal method for multipart/signed

(defun mime-verify-multipart/signed (entity situation)
  "Internal method to verify multipart/signed."
  (mime-play-entity
   (nth 1 (mime-entity-children entity)) ; entity-info of signature
   (list (assq 'mode situation)) ; play-mode
   ))


;;; @ Internal method for multipart/encrypted

(defun mime-display-multipart/pgp-encrypted (entity situation)
  (let ((original-major-mode-cell (assq 'major-mode situation))
	(default-situation
	  (cdr (assq 'childrens-situation situation)))
	child-situation)
    (unless mime-pgp-decrypt-when-preview
      (insert "This part is encrypted.\n"))
    (when original-major-mode-cell
      (setq default-situation
	    (cons original-major-mode-cell default-situation)))
    (mapc
     (lambda (child)
       (setq child-situation
	     (mime-find-entity-preview-situation child default-situation))
       (if (and (eq (cdr (assq 'type child-situation)) 'application)
		(eq (cdr (assq 'subtype child-situation)) 'pgp-encrypted))
	   (mime-display-entity
	    child (put-alist 'body (if mime-pgp-decrypt-when-preview
				       'visible
				     'invisible)
			     (copy-alist child-situation)))))
     (mime-entity-children entity))))

;;; @ Internal method for application/*-signature

(defun mime-verify-application/*-signature-internal (entity situation)
  (let* ((mother (mime-entity-parent entity))
	 (orig-entity (car (mime-entity-children mother)))
	 (protocol (cdr (assoc "protocol" (mime-entity-parameters mother))))
	 (context (epg-make-context
		   (if (equal protocol "application/pgp-signature")
		       'OpenPGP
		     (if (string-match
			  "\\`application/\\(x-\\)?pkcs7-signature\\'"
			  protocol)
			 'CMS
		       (error "Unknown protocol: %s" protocol))))))
    (epg-verify-string context
		       (mime-entity-content entity)
		       (with-temp-buffer
			 (if (fboundp 'set-buffer-multibyte)
			     (set-buffer-multibyte nil))
			 (mime-insert-entity orig-entity)
			 (goto-char (point-min))
			 (while (search-forward "\n" nil t)
			   (replace-match "\r\n"))
			 (buffer-substring (point-min) (point-max))))
    (epg-context-result-for context 'verify)))

(defun mime-verify-application/*-signature (entity situation)
  (let ((verify-result
	 (mime-verify-application/*-signature-internal entity situation)))
    (if (> (length verify-result) 1)
	(mime-show-echo-buffer (epg-verify-result-to-string verify-result))
      (if verify-result
	  (epa-display-info verify-result)))))

(defun mime-preview-application/*-signature (entity situation)
  (let ((string
	 (epg-verify-result-to-string
	  (mime-verify-application/*-signature-internal entity situation))))
    (when (> (length string) 0)
      (unless (string-equal (substring string -1) "\n")
	(setq string (concat string "\n")))
      (insert string))))


;;; @ Internal method for application/pgp-encrypted

(defun mime-decrypt-application/pgp-encrypted (entity situation)
  (let* ((mother (mime-entity-parent entity))
	 (encrypted-entity (nth 1 (mime-entity-children mother)))
	 (p-win (or (get-buffer-window (current-buffer))
		    (get-largest-window)))
	 (new-name
	  (format "%s-%s" (buffer-name) (mime-entity-number entity)))
	 (mother (current-buffer))
	 (preview-buffer (concat "*Preview-" (buffer-name) "*"))
	 representation-type message-buf context plain verify-result)
    (set-buffer (setq message-buf (get-buffer-create new-name)))
    (erase-buffer)
    (mime-insert-entity encrypted-entity)
    (goto-char (point-min))
    (setq context (epg-make-context)
	  plain (decode-coding-string
		 (epg-decrypt-string
		  context
		  (buffer-substring (point-min)(point-max)))
		 'raw-text))
    (delete-region (point-min)(point-max))
    (insert plain)
    (setq representation-type 'binary
	  major-mode 'mime-show-message-mode)
    (save-window-excursion
      (mime-view-buffer nil preview-buffer mother
			nil representation-type)
      (make-local-variable 'mime-view-temp-message-buffer)
      (setq mime-view-temp-message-buffer message-buf))
    (set-window-buffer p-win preview-buffer)
    (setq verify-result (epg-context-result-for context 'verify))
    (if (> (length verify-result) 1)
	(mime-show-echo-buffer (epg-verify-result-to-string verify-result))
      (if verify-result
	  (epa-display-info verify-result)))))


;;; @ Internal method for application/pgp-keys

(defun mime-add-application/pgp-keys (entity situation)
  (epg-import-keys-from-string (epg-make-context)
			       (mime-entity-content entity)))


;;; @ Internal method for application/pkcs7-mime

(defun mime-view-application/pkcs7-mime (entity situation)
  (let* ((p-win (or (get-buffer-window (current-buffer))
		    (get-largest-window)))
	 (new-name
	  (format "%s-%s" (buffer-name) (mime-entity-number entity)))
	 (mother (current-buffer))
	 (preview-buffer (concat "*Preview-" (buffer-name) "*"))
	 (context (epg-make-context 'CMS))
	 message-buf)
    (when (memq (or (cdr (assq 'smime-type situation)) 'enveloped-data)
		'(enveloped-data signed-data))
      (set-buffer (setq message-buf (get-buffer-create new-name)))
      (let ((inhibit-read-only t)
	    buffer-read-only)
	(erase-buffer)
	(insert (epg-decrypt-string context (mime-entity-content entity))))
      (setq major-mode 'mime-show-message-mode)
      (save-window-excursion
	(mime-view-buffer nil preview-buffer mother
			  nil 'binary)
	(make-local-variable 'mime-view-temp-message-buffer)
	(setq mime-view-temp-message-buffer message-buf))
      (set-window-buffer p-win preview-buffer))))


;;; @ end
;;;

(provide 'mime-pgp)

(run-hooks 'mime-pgp-load-hook)

;;; mime-pgp.el ends here
