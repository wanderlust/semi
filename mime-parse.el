;;; mime-parse.el --- MIME message parser

;; Copyright (C) 1994,1995,1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id$
;; Keywords: parse, MIME, multimedia, mail, news

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

(require 'emu)
(require 'std11)
(require 'mime-def)

(defsubst symbol-concat (&rest args)
  "Return a symbol whose name is concatenation of arguments ARGS
which are string or symbol."
  (intern (mapconcat (function
		      (lambda (s)
			(cond ((symbolp s) (symbol-name s))
			      ((stringp s) s)
			      )))
		     args "")))

(defmacro define-structure (name &rest slots)
  (let ((pred (symbol-concat name '-p)))
    (cons 'progn
	  (nconc
	   (list
	    (` (defun (, pred) (obj)
		 (and (vectorp obj)
		      (eq (elt obj 0) '(, name))
		      ))
	       )
	    (` (defun (, (symbol-concat name '/create)) (, slots)
		 (, (cons 'vector (cons (list 'quote name) slots)))
		 )
	       ))
	   (let ((i 1))
	     (mapcar (function
		      (lambda (slot)
			(prog1
			    (` (defun (, (symbol-concat name '/ slot)) (obj)
				 (if ((, pred) obj)
				     (elt obj (, i))
				   ))
			       )
			  (setq i (+ i 1))
			  )
			)) slots)
	     )
	   (list (list 'quote name))
	   ))))


;;; @ field parser
;;;

(defsubst regexp-* (regexp)
  (concat regexp "*"))

(defconst rfc822/quoted-pair-regexp "\\\\.")
(defconst rfc822/qtext-regexp
  (concat "[^" (char-list-to-string std11-non-qtext-char-list) "]"))
(defconst rfc822/quoted-string-regexp
  (concat "\""
	  (regexp-*
	   (regexp-or rfc822/qtext-regexp rfc822/quoted-pair-regexp)
	   )
	  "\""))

(defconst mime/content-parameter-value-regexp
  (concat "\\("
	  rfc822/quoted-string-regexp
	  "\\|[^; \t\n]*\\)"))

(defconst mime::parameter-regexp
  (concat "^[ \t]*\;[ \t]*\\(" mime-token-regexp "\\)"
	  "[ \t]*=[ \t]*\\(" mime/content-parameter-value-regexp "\\)"))

(defun mime-parse-parameter (str)
  (if (string-match mime::parameter-regexp str)
      (let ((e (match-end 2)))
	(cons
	 (cons (downcase (substring str (match-beginning 1) (match-end 1)))
	       (std11-strip-quoted-string
		(substring str (match-beginning 2) e))
	       )
	 (substring str e)
	 ))))

(defun mime-parse-Content-Type (string)
  "Parse STRING as field-body of Content-Type field.
Return value is
    (PRIMARY-TYPE SUBTYPE (NAME1 . VALUE1)(NAME2 . VALUE2) ...)
or nil.  PRIMARY-TYPE and SUBTYPE are symbol and NAME_n and VALUE_n
are string."
  (setq string (std11-unfold-string string))
  (if (string-match `,(concat "^\\(" mime-token-regexp
			      "\\)/\\(" mime-token-regexp "\\)") string)
      (let* ((type (downcase
		    (substring string (match-beginning 1) (match-end 1))))
	     (subtype (downcase
		       (substring string (match-beginning 2) (match-end 2))))
	     ret dest)
	(setq string (substring string (match-end 0)))
	(while (setq ret (mime-parse-parameter string))
	  (setq dest (cons (car ret) dest)
		string (cdr ret))
	  )
	(cons (intern type) (cons (intern subtype) (nreverse dest)))
	)))


(defconst mime-disposition-type-regexp mime-token-regexp)

(defun mime-parse-Content-Disposition (string)
  "Parse STRING as field-body of Content-Disposition field."
  (setq string (std11-unfold-string string))
  (if (string-match `,(concat "^" mime-disposition-type-regexp) string)
      (let* ((e (match-end 0))
	     (ctype (downcase (substring string 0 e)))
	     ret dest)
	(setq string (substring string e))
	(while (setq ret (mime-parse-parameter string))
	  (setq dest (cons (car ret) dest)
		string (cdr ret))
	  )
	(cons ctype (nreverse dest))
	)))


;;; @ field reader
;;;

(defun mime-read-Content-Type ()
  "Read field-body of Content-Type field from current-buffer,
and return parsed it.  Format of return value is as same as
`mime-parse-Content-Type'."
  (let ((str (std11-field-body "Content-Type")))
    (if str
	(mime-parse-Content-Type str)
      )))

(defun mime/Content-Transfer-Encoding (&optional default-encoding)
  "Read field-body of Content-Transfer-Encoding field from
current-buffer, and return it.
If is is not found, return DEFAULT-ENCODING. [mime-parse.el]"
  (let ((str (std11-field-body "Content-Transfer-Encoding")))
    (if str
	(progn
	  (if (string-match "[ \t\n\r]+$" str)
	      (setq str (substring str 0 (match-beginning 0)))
	    )
	  (downcase str)
	  )
      default-encoding)
    ))

(defun mime/Content-Disposition ()
  "Read field-body of Content-Disposition field from current-buffer,
and return parsed it. [mime-parse.el]"
  (let ((str (std11-field-body "Content-Disposition")))
    (if str
	(mime-parse-Content-Disposition str)
      )))


;;; @ message parser
;;;

(define-structure mime::content-info
  rcnum point-min point-max type parameters encoding children)

(defsubst make-mime-entity-info (rcnum
				 point-min point-max
				 media-type media-subtype
				 parameters encoding children)
  (let ((ctype (if media-type
		   (if media-subtype
		       (format "%s/%s" media-type media-subtype)
		     (symbol-name media-type)))))
    (mime::content-info/create rcnum point-min point-max
			       ctype params encoding
			       children)
    ))

(defsubst mime-entity-info-point-min (entity-info)
  (mime::content-info/point-min entity-info))

(defsubst mime-entity-info-point-max (entity-info)
  (mime::content-info/point-max entity-info))

(defun mime-parse-multipart (boundary primtype subtype params encoding rcnum)
  (goto-char (point-min))
  (let* ((dash-boundary   (concat "--" boundary))
	 (delimiter       (concat "\n" (regexp-quote dash-boundary)))
	 (close-delimiter (concat delimiter "--[ \t]*$"))
	 (beg (point-min))
	 (end (progn
		(goto-char (point-max))
		(if (re-search-backward close-delimiter nil t)
		    (match-beginning 0)
		  (point-max)
		  )))
	 (rsep (concat delimiter "[ \t]*\n"))
	 (dc-ctl
	  (if (eq subtype 'digest)
	      '(message rfc822)
	    '(text plain)
	    ))
	 cb ce ret ncb children (i 0))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (re-search-forward rsep nil t)
      (setq cb (match-end 0))
      (while (re-search-forward rsep nil t)
	(setq ce (match-beginning 0))
	(setq ncb (match-end 0))
	(save-restriction
	  (narrow-to-region cb ce)
	  (setq ret (mime-parse-message dc-ctl "7bit" (cons i rcnum)))
	  )
	(setq children (cons ret children))
	(goto-char (mime-entity-info-point-max ret))
	(goto-char (setq cb ncb))
	(setq i (1+ i))
	)
      (setq ce (point-max))
      (save-restriction
	(narrow-to-region cb ce)
	(setq ret (mime-parse-message dc-ctl "7bit" (cons i rcnum)))
	)
      (setq children (cons ret children))
      )
    (make-mime-entity-info rcnum beg (point-max)
			   primtype subtype params encoding
			   (nreverse children))
    ))

(defun mime-parse-message (&optional default-ctl default-encoding rcnum)
  "Parse current-buffer as a MIME message.
DEFAULT-CTL is used when an entity does not have valid Content-Type
field.  Its format must be as same as return value of
mime-{parse|read}-Content-Type."
  (setq default-ctl (or (mime-read-Content-Type) default-ctl))
  (let ((primtype (car default-ctl))
	(subtype (car (cdr default-ctl)))
	(params (cdr (cdr default-ctl)))
	(encoding (or (mime/Content-Transfer-Encoding) default-encoding))
	)
    (let ((boundary (assoc "boundary" params)))
      (cond (boundary
	     (setq boundary (std11-strip-quoted-string (cdr boundary)))
	     (mime-parse-multipart
	      boundary
	      primtype subtype params encoding rcnum)
	     )
	    ((and (eq primtype 'message)
		  (memq subtype '(rfc822 news))
		  )
	     (goto-char (point-min))
	     (make-mime-entity-info rcnum
				    (point-min) (point-max)
				    primtype subtype params encoding
				    (save-restriction
				      (narrow-to-region
				       (if (re-search-forward "^$" nil t)
					   (1+ (match-end 0))
					 (point-min)
					 )
				       (point-max))
				      (list (mime-parse-message
					     nil nil (cons 0 rcnum)))
				      ))
	     )
	    (t 
	     (make-mime-entity-info rcnum (point-min) (point-max)
				    primtype subtype params encoding
				    nil)
	     ))
      )))


;;; @ end
;;;

(provide 'mime-parse)

;;; mime-parse.el ends here
