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

;;; @ buffer local variables in raw-buffer
;;;

(defvar mime-text-decoder nil
  "Function to decode text in current buffer.
Interface of the function is (CHARSET &optional ENCODING).
CHARSET is symbol of MIME charset and ENCODING is value of
Content-Transfer-Encoding.

Notice that this variable is usually used as buffer local variable in
raw-buffer.")

(make-variable-buffer-local 'mime-text-decoder)


;;; @ code conversion
;;;

(defvar mime-text-decoder-alist
  '((mime-show-message-mode	. mime-text-decode-buffer)
    (mime-temp-message-mode	. mime-text-decode-buffer)
    (t				. mime-text-decode-buffer-maybe)
    )
  "Alist of major-mode vs. mime-text-decoder.
Each element looks like (SYMBOL . FUNCTION).  SYMBOL is major-mode or
t.  t means default.

Specification of FUNCTION is described in DOC-string of variable
`mime-text-decoder'.

This value is overridden by buffer local variable `mime-text-decoder'
if it is not nil.")

(defun mime-text-decode-buffer (charset &optional encoding)
  "Decode text of current buffer as CHARSET.
It code-converts current buffer from network representation specified
by MIME CHARSET to internal code.  CHARSET is symbol of MIME charset.
See also variable `mime-charset-coding-system-alist'."
  (decode-mime-charset-region (point-min)(point-max)
			      (or charset default-mime-charset))
  )

(defun mime-text-decode-buffer-maybe (charset &optional encoding)
  "Decode text of current buffer as CHARSET if ENCODING is actual encoding.
It code-converts current buffer from network representation specified
by MIME CHARSET to internal code if ENCODING is not nil, \"7bit\",
\"8bit\" or \"binary\".  CHARSET is symbol of MIME charset.
See also variable `mime-charset-coding-system-alist'."
  (or (member encoding '(nil "7bit" "8bit" "binary"))
      (mime-text-decode-buffer charset)
      ))

(defun mime-decode-text-body (charset encoding)
  "Decode current buffer as text body.
It decodes MIME-encoding as ENCODING then code-converts as MIME
CHARSET.  CHARSET is SYMBOL and ENCODING is nil or STRING.

It calls text decoder for MIME charset specified by buffer local
variable `mime-text-decoder' and variable `mime-text-decoder-alist'."
  (mime-decode-region (point-min) (point-max) encoding)
  (let ((text-decoder
	 (save-excursion
	   (set-buffer mime-raw-buffer)
	   (or mime-text-decoder
	       (cdr (or (assq major-mode mime-text-decoder-alist)
			(assq t mime-text-decoder-alist)))
	       ))))
    (and (functionp text-decoder)
	 (funcall text-decoder charset encoding)
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

(defun mime-view-filter-for-text/plain (ctype params encoding)
  (mime-decode-text-body (cdr (assoc "charset" params)) encoding)
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

(defun mime-view-filter-for-text/richtext (ctype params encoding)
  (let* ((charset (cdr (assoc "charset" params)))
	 (beg (point-min))
	 )
    (remove-text-properties beg (point-max) '(face nil))
    (mime-decode-text-body charset encoding)
    (richtext-decode beg (point-max))
    ))

(defun mime-view-filter-for-text/enriched (ctype params encoding)
  (let* ((charset (cdr (assoc "charset" params)))
	 (beg (point-min))
	 )
    (remove-text-properties beg (point-max) '(face nil))
    (mime-decode-text-body charset encoding)
    (enriched-decode beg (point-max))
    ))


;;; @ end
;;;

(provide 'mime-text)

;;; mime-text.el ends here
