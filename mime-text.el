;;; mime-text.el --- mime-view content filter for text

;; Copyright (C) 1994,1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: text, MIME, multimedia, mail, news

;; This file is part of SEMI (Suite of Emacs MIME Interfaces).

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

(require 'mime-view)


;;; @ code conversion
;;;

(defun mime-text-insert-decoded-body (entity)
  "Insert text body of ENTITY in SITUATION.
It decodes MIME-encoding then code-converts as MIME-charset.
MIME-encoding is value of field 'encoding of SITUATION.  It must be
'nil or string.  MIME-charset is value of field \"charset\" of
SITUATION.  It must be symbol."
  (let* ((buffer (mime-entity-buffer entity))
	 (presentation-type
	 (save-excursion
	   (set-buffer buffer)
	   (or mime-raw-representation-type
	       (cdr (or (assq major-mode mime-raw-representation-type-alist)
			(assq t mime-raw-representation-type-alist)))
	       ))))
    (save-restriction
      (insert-buffer-substring buffer
			       (mime-entity-body-start entity)
			       (mime-entity-body-end entity))
      (let ((encoding (mime-entity-encoding entity)))
	(mime-decode-region (point-min) (point-max) encoding)
	(if (or (eq presentation-type 'binary)
		(not (member encoding '(nil "7bit" "8bit" "binary"))))
	    (decode-mime-charset-region (point-min)(point-max)
					(or (mime-content-type-parameter
					     (mime-entity-content-type entity)
					     "charset")
					    default-mime-charset))
	  ))))
  (run-hooks 'mime-text-decode-hook)
  )


;;; @ for URL
;;;

(require 'browse-url)

(defvar mime-text-url-regexp
  "\\(http\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]*[-a-zA-Z0-9_=#$@~`%&*+|\\/]"
  "*Regexp to match URL in text/plain body.")

(defun mime-text-browse-url (&optional url)
  (if (fboundp browse-url-browser-function)
      (if url 
        (funcall browse-url-browser-function url)
      (call-interactively browse-url-browser-function))
    (if (fboundp mime-button-mother-dispatcher)
	(call-interactively mime-button-mother-dispatcher)
      )
    ))

(defsubst mime-text-add-url-buttons ()
  "Add URL-buttons for text body."
  (goto-char (point-min))
  (while (re-search-forward mime-text-url-regexp nil t)
    (let ((beg (match-beginning 0))
	  (end (match-end 0)))
      (mime-add-button beg end #'mime-text-browse-url
		       (list (buffer-substring beg end)))
      )))

(defun mime-text-add-url-buttons-maybe ()
  "Add URL-buttons if 'browse-url-browser-function is not 'nil."
  (if browse-url-browser-function
      (mime-text-add-url-buttons)
    ))


;;; @ content filters for mime-text
;;;

(defun mime-preview-text/plain (entity situation)
  (save-restriction
    (narrow-to-region (point-max)(point-max))
    (mime-text-insert-decoded-body entity)
    (goto-char (point-max))
    (if (not (eq (char-after (1- (point))) ?\n))
	(insert "\n")
      )
    (mime-text-add-url-buttons)
    (run-hooks 'mime-preview-text/plain-hook)
    ))

(defun mime-preview-text/richtext (entity situation)
  (save-restriction
    (narrow-to-region (point-max)(point-max))
    (mime-text-insert-decoded-body entity)
    (let ((beg (point-min)))
      (remove-text-properties beg (point-max) '(face nil))
      (richtext-decode beg (point-max))
      )))

(defun mime-preview-text/enriched (entity situation)
  (save-restriction
    (narrow-to-region (point-max)(point-max))
    (mime-text-insert-decoded-body entity)
    (let ((beg (point-min)))
      (remove-text-properties beg (point-max) '(face nil))
      (enriched-decode beg (point-max))
      )))


;;; @ end
;;;

(provide 'mime-text)

;;; mime-text.el ends here
