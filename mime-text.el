;;; mime-text.el --- mime-view content filter for text

;; Copyright (C) 1994,1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: text, MIME, multimedia, mail, news

;; This file is part of WEMI (Widget based Emacs MIME Interfaces).

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
(autoload 'widget-convert-text "wid-edit")


;;; @ content filters for mime-text
;;;

(defun mime-display-text/plain (entity situation)
  (save-restriction
    (narrow-to-region (point-max)(point-max))
    (mime-insert-text-content entity)
    (run-hooks 'mime-text-decode-hook)
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
    (mime-insert-text-content entity)
    (run-hooks 'mime-text-decode-hook)
    (let ((beg (point-min)))
      (remove-text-properties beg (point-max) '(face nil))
      (richtext-decode beg (point-max))
      )))

(defun mime-display-text/enriched (entity situation)
  (save-restriction
    (narrow-to-region (point-max)(point-max))
    (mime-insert-text-content entity)
    (run-hooks 'mime-text-decode-hook)
    (let ((beg (point-min)))
      (remove-text-properties beg (point-max) '(face nil))
      (enriched-decode beg (point-max))
      )))


;;; @ end
;;;

(provide 'mime-text)

;;; mime-text.el ends here
