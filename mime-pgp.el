;;; mime-pgp.el --- mime-view internal methods for PGP.

;; Copyright (C) 1995,1996,1997,1998 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1995/12/7
;;	Renamed: 1997/2/27 from tm-pgp.el
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;    This module is based on

;;	[security-multipart] RFC 1847: "Security Multiparts for MIME:
;;	    Multipart/Signed and Multipart/Encrypted" by
;;          Jim Galvin <galvin@tis.com>, Sandy Murphy <sandy@tis.com>,
;;	    Steve Crocker <crocker@cybercash.com> and
;;          Ned Freed <ned@innosoft.com> (1995/10)

;;	[PGP/MIME] RFC 2015: "MIME Security with Pretty Good Privacy
;;	    (PGP)" by Michael Elkins <elkins@aero.org> (1996/6)

;;	[PGP-kazu] draft-kazu-pgp-mime-00.txt: "PGP MIME Integration"
;;	    by Kazuhiko Yamamoto <kazu@is.aist-nara.ac.jp> (1995/10;
;;	    expired)

;;  PGP/MIME and PGP-kazu may be contrary to each other.  You should
;;  decide which you support (Maybe you should not use PGP-kazu).

;;; Code:

(require 'mime-play)


;;; @ internal method for application/pgp
;;;
;;; It is based on draft-kazu-pgp-mime-00.txt (PGP-kazu).

(defun mime-method-for-application/pgp (start end cal)
  (let* ((entity-number (mime-raw-point-to-entity-number start))
	 (p-win (or (get-buffer-window mime-preview-buffer)
		    (get-largest-window)))
	 (new-name (format "%s-%s" (buffer-name) entity-number))
	 (the-buf (current-buffer))
	 (mother mime-preview-buffer)
	 (mode major-mode)
	 text-decoder)
    (set-buffer (get-buffer-create new-name))
    (erase-buffer)
    (insert-buffer-substring the-buf start end)
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
	   (setq text-decoder
		 (cdr (or (assq mode mime-text-decoder-alist)
			  (assq t    mime-text-decoder-alist))))
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
	   (setq text-decoder (function mime-text-decode-buffer))
	   ))
    (setq major-mode 'mime-show-message-mode)
    (setq mime-text-decoder text-decoder)
    (save-window-excursion (mime-view-mode mother))
    (set-window-buffer p-win mime-preview-buffer)
    ))

;; (ctree-set-calist-strictly
;;  'mime-preview-condition '((type . application)(subtype . pgp)
;;                            (message-button . visible)))

;; (ctree-set-calist-strictly
;;  'mime-acting-condition '((type . application)(subtype . pgp)
;;                           (method . mime-method-for-application/pgp)))

;; (ctree-set-calist-strictly
;;  'mime-acting-condition '((type . text)(subtype . x-pgp)
;;                           (method . mime-method-for-application/pgp)))


;;; @ Internal method for multipart/signed
;;;
;;; It is based on RFC 1847 (security-multipart).

(defun mime-method-to-verify-multipart/signed (start end cal)
  "Internal method to verify multipart/signed."
  (mime-raw-play-entity
   ;; entity-info of signature
   (mime-raw-find-entity-from-node-id
    ;; entity-node-id of signature
    (cons 1 (mime-raw-point-to-entity-node-id start)))
   (cdr (assq 'mode cal)) ; play-mode
   ))

;; (ctree-set-calist-strictly
;;  'mime-acting-condition
;;  '((type . multipart)(subtype . signed)
;;    (method . mime-method-to-verify-multipart/signed)))


;;; @ Internal method for application/pgp-signature
;;;
;;; It is based on RFC 2015 (PGP/MIME).

(defvar mime-pgp-command "pgp"
  "*Name of the PGP command.")

(defvar mime-pgp-default-language 'en
  "*Symbol of language for pgp.
It should be ISO 639 2 letter language code such as en, ja, ...")

(defvar mime-pgp-good-signature-regexp-alist
  '((en . "Good signature from user.*$"))
  "Alist of language vs regexp to detect ``Good signature''.")

(defvar mime-pgp-key-expected-regexp-alist
  '((en . "Key matching expected Key ID \\(\\S +\\) not found"))
  "Alist of language vs regexp to detect ``Key expected''.")

(defun mime-pgp-check-signature (output-buffer orig-file)
  (save-excursion
    (set-buffer output-buffer)
    (erase-buffer))
  (let* ((lang (or mime-pgp-default-language 'en))
	 (status (call-process-region (point-min)(point-max)
				      mime-pgp-command
				      nil output-buffer nil
				      orig-file (format "+language=%s" lang)))
	 (regexp (cdr (assq lang mime-pgp-good-signature-regexp-alist))))
    (if (= status 0)
	(save-excursion
	  (set-buffer output-buffer)
	  (goto-char (point-min))
	  (message
	   (cond ((not (stringp regexp))
		  "Please specify right regexp for specified language")
		 ((re-search-forward regexp nil t)
		  (buffer-substring (match-beginning 0) (match-end 0)))
		 (t "Bad signature")))
	  ))))

(defun mime-method-to-verify-application/pgp-signature (start end cal)
  "Internal method to check PGP/MIME signature."
  (let* ((encoding (cdr (assq 'encoding cal)))
	 (entity-node-id (mime-raw-point-to-entity-node-id start))
	 (mother-node-id (cdr entity-node-id))
	 (knum (car entity-node-id))
	 (onum (if (> knum 0)
		   (1- knum)
		 (1+ knum)))
	 (oinfo (mime-raw-find-entity-from-node-id
		 (cons onum mother-node-id) mime-raw-message-info))
	 (basename (expand-file-name "tm" mime-temp-directory))
	 (orig-file (make-temp-name basename))
	 (sig-file (concat orig-file ".sig"))
	 )
    (mime-raw-write-region (mime-entity-point-min oinfo)
			   (mime-entity-point-max oinfo)
			   orig-file)
    (save-excursion (mime-show-echo-buffer))
    (mime-write-decoded-region (save-excursion
				 (goto-char start)
				 (and (search-forward "\n\n")
				      (match-end 0))
				 ) end sig-file encoding)
    (or (mime-pgp-check-signature mime-echo-buffer-name orig-file)
	(let (pgp-id)
	  (save-excursion
	    (set-buffer mime-echo-buffer-name)
	    (goto-char (point-min))
	    (let ((regexp (cdr (assq (or mime-pgp-default-language 'en)
				     mime-pgp-key-expected-regexp-alist))))
	      (cond ((not (stringp regexp))
		     (message
		      "Please specify right regexp for specified language")
		     )
		    ((re-search-forward regexp nil t)
		     (setq pgp-id
			   (concat "0x" (buffer-substring-no-properties
					 (match-beginning 1)
					 (match-end 1))))
		     ))))
	  (if (and pgp-id
		   (y-or-n-p
		    (format "Key %s not found; attempt to fetch? " pgp-id))
		   )
	      (progn
		(funcall (pgp-function 'fetch-key) (cons nil pgp-id))
		(mime-pgp-check-signature mime-echo-buffer-name orig-file)
		))
	  ))
    (let ((other-window-scroll-buffer mime-echo-buffer-name))
      (scroll-other-window 8)
      )
    (delete-file orig-file)
    (delete-file sig-file)
    ))

;; (ctree-set-calist-strictly
;;  'mime-acting-condition
;;  '((type . application)(subtype . pgp-signature)
;;    (method . mime-method-to-verify-application/pgp-signature)))


;;; @ Internal method for application/pgp-encrypted
;;;
;;; It is based on RFC 2015 (PGP/MIME).

(defun mime-method-to-decrypt-application/pgp-encrypted (start end cal)
  (let* ((entity-node-id (mime-raw-point-to-entity-node-id start))
	 (mother-node-id (cdr entity-node-id))
	 (knum (car entity-node-id))
	 (onum (if (> knum 0)
		   (1- knum)
		 (1+ knum)))
	 (oinfo (mime-raw-find-entity-from-node-id
		 (cons onum mother-node-id) mime-raw-message-info))
	 (obeg (mime-entity-point-min oinfo))
	 (oend (mime-entity-point-max oinfo))
	 )
    (mime-method-for-application/pgp obeg oend cal)
    ))

;; (ctree-set-calist-strictly
;;  'mime-acting-condition
;;  '((type . application)(subtype . pgp-encrypted)
;;    (method . mime-method-to-decrypt-application/pgp-encrypted)))


;;; @ Internal method for application/pgp-keys
;;;
;;; It is based on RFC 2015 (PGP/MIME).

(defun mime-method-to-add-application/pgp-keys (start end cal)
  (let* ((entity-number (mime-raw-point-to-entity-number start))
	 (new-name (format "%s-%s" (buffer-name) entity-number))
	 (encoding (cdr (assq 'encoding cal)))
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

;; (ctree-set-calist-strictly
;;  'mime-acting-condition
;;  '((type . application)(subtype . pgp-keys)
;;    (method . mime-method-to-add-application/pgp-keys)))

	 
;;; @ end
;;;

(provide 'mime-pgp)

(run-hooks 'mime-pgp-load-hook)

;;; mime-pgp.el ends here
