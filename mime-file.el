;;; mime-file.el --- mime-view internal method for file extraction

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; modified by Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;; Keywords: file, extract, MIME, multimedia, mail, news

;; This file is part of SEMI (Saver for Emacs MIME Interfaces).

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

(defun mime-extract-current-entity (beg end cal)
  (goto-char beg)
  (let* ((name
	  (save-restriction
	    (narrow-to-region beg end)
	    (mime-article/get-filename cal)
	    ))
	 (encoding (cdr (assq 'encoding cal)))
	 (filename
          (if (and name (not (string-equal name "")))
	      (expand-file-name name
				(call-interactively
				 (function
				  (lambda (dir)
				    (interactive "DDirectory: ")
				    dir))))
	    (call-interactively
	     (function
	      (lambda (file)
		(interactive "FFilename: ")
		(expand-file-name file))))))
	 )
    (if (file-exists-p filename)
        (or (yes-or-no-p (format "File %s exists. Save anyway? " filename))
            (error "")))
    (re-search-forward "\n\n")
    (mime-write-decoded-region (match-end 0)(point-max) filename encoding)
    ))


;;; @ setup
;;;

(set-atype 'mime-acting-condition
	   '((type . "application/octet-stream")
	     (method . mime-extract-current-entity)
	     )
	   'ignore '(method)
	   'replacement)

(set-atype 'mime-acting-condition
	   '((mode . "extract")
	     (method . mime-extract-current-entity)
	     )
	   'remove
	   '((method "mime-file"  nil 'file 'type 'encoding 'mode 'name)
	     (mode . "extract"))
	   'replacement)


;;; @ end
;;;

(provide 'mime-file)

;;; end of mime-file.el
