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
  (let ((str (mime-entity-content entity)))
    (insert
     (if (and (mime-entity-cooked-p entity)
	      (member (mime-entity-encoding entity)
		      '(nil "7bit" "8bit" "binary")))
	 str
       (decode-mime-charset-string str
				   (or (mime-content-type-parameter
					(mime-entity-content-type entity)
					"charset")
				       default-mime-charset))
       )))
  (run-hooks 'mime-text-decode-hook)
  )


;;; @ content filters for mime-text
;;;

(defun mime-display-text/plain (entity situation)
  (save-restriction
    (narrow-to-region (point-max)(point-max))
    (mime-text-insert-decoded-body entity)
    (goto-char (point-max))
    (if (not (eq (char-after (1- (point))) ?\n))
	(insert "\n")
      )
    (mime-add-url-buttons)
    (run-hooks 'mime-display-text/plain-hook)
    ))

(defun mime-display-text/richtext (entity situation)
  (save-restriction
    (narrow-to-region (point-max)(point-max))
    (mime-text-insert-decoded-body entity)
    (let ((beg (point-min)))
      (remove-text-properties beg (point-max) '(face nil))
      (richtext-decode beg (point-max))
      )))

(defun mime-display-text/enriched (entity situation)
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
