;;; mime-text.el --- mime-view content filter for text

;; Copyright (C) 1994,1995,1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version:
;;	$Id$
;; Keywords: text, MIME, multimedia, mail, news

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

;;; @ code conversion
;;;

(defvar mime-text-decoder-alist
  '((mime/show-message-mode	. mime-charset/decode-buffer)
    (mime-temp-message-mode	. mime-charset/decode-buffer)
    (t				. mime-charset/maybe-decode-buffer)
    ))

(defun mime-charset/decode-buffer (charset &optional encoding)
  (decode-mime-charset-region (point-min)(point-max)
			      (or charset default-mime-charset))
  )

(defun mime-charset/maybe-decode-buffer (charset &optional encoding)
  (or (member encoding '(nil "7bit" "8bit" "binary"))
      (mime-charset/decode-buffer charset)
      ))

(defun mime-preview/decode-text-buffer (charset encoding)
  (mime-decode-region (point-min) (point-max) encoding)
  (let* ((mode mime::preview/original-major-mode)
	 (m (or (save-excursion
		  (set-buffer mime::preview/article-buffer)
		  mime::article/code-converter)
		(cdr (or (assq mode mime-text-decoder-alist)
			 (assq t mime-text-decoder-alist)))
		))
	 )
    (and (functionp m)
	 (funcall m charset encoding)
	 )))


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


;;; @ content filters for mime-text
;;;

(defun mime-preview/filter-for-text/plain (ctype params encoding)
  (mime-preview/decode-text-buffer (cdr (assoc "charset" params)) encoding)
  (goto-char (point-max))
  (if (not (eq (char-after (1- (point))) ?\n))
      (insert "\n")
    )
  (if browse-url-browser-function
      (progn
	(goto-char (point-min))
	(while (re-search-forward mime-text-url-regexp nil t)
	  (let ((beg (match-beginning 0))
		(end (match-end 0)))
	    (mime-add-button beg end
			     (function mime-text-browse-url)
			     (list (buffer-substring beg end))))
	  )))
  (run-hooks 'mime-view-plain-text-preview-hook)
  )

(defun mime-preview/filter-for-text/richtext (ctype params encoding)
  (let* ((mode mime::preview/original-major-mode)
	 (m (assq mode mime-text-decoder-alist))
	 (charset (cdr (assoc "charset" params)))
	 (beg (point-min))
	 )
    (remove-text-properties beg (point-max) '(face nil))
    (mime-preview/decode-text-buffer charset encoding)
    (richtext-decode beg (point-max))
    ))

(defun mime-preview/filter-for-text/enriched (ctype params encoding)
  (let* ((mode mime::preview/original-major-mode)
	 (m (assq mode mime-text-decoder-alist))
	 (charset (cdr (assoc "charset" params)))
	 (beg (point-min))
	 )
    (remove-text-properties beg (point-max) '(face nil))
    (mime-preview/decode-text-buffer charset encoding)
    (enriched-decode beg (point-max))
    ))


;;; @ end
;;;

(provide 'mime-text)

;;; mime-text.el ends here
