;;; mailcap.el --- mailcap parser

;; Copyright (C) 1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1997/6/27
;; Keywords: mailcap, setting, configuration, MIME, multimedia

;; This file is part of SEMI (Spadework for Emacs MIME Interfaces).

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

;;; @ comment
;;;

(defsubst mailcap-skip-comment ()
  (let ((chr (char-after (point))))
    (when (and chr
	       (or (= chr ?\n)
		   (= chr ?#)))
      (forward-line)
      t)))


;;; @ token
;;;

(defsubst mailcap-look-at-token ()
  (if (looking-at mime-token-regexp)
      (let ((beg (match-beginning 0))
	    (end (match-end 0)))
	(goto-char end)
	(buffer-substring beg end)
	)))


;;; @ typefield
;;;

(defsubst mailcap-look-at-type-field ()
  (let ((type (mailcap-look-at-token)))
    (if type
	(if (eq (char-after (point)) ?/)
	    (progn
	      (forward-char)
	      (let ((subtype (mailcap-look-at-token)))
		(if subtype
		    (cons (cons 'type (intern type))
			  (unless (string= subtype "*")
			    (list (cons 'subtype (intern subtype)))
			    )))))
	  (list (cons 'type (intern type)))
	  ))))


;;; @ field separator
;;;

(defsubst mailcap-skip-field-separator ()
  (let ((ret (looking-at "\\([ \t]\\|\\\\\n\\)*;\\([ \t]\\|\\\\\n\\)*")))
    (when ret
      (goto-char (match-end 0))
      t)))


;;; @ mtext
;;;

(defsubst mailcap-look-at-schar ()
  (let ((chr (char-after (point))))
    (if (and (>= chr ? )
	     (/= chr ?\;)
	     (/= chr ?\\)
	     )
	(prog1
	    chr
	  (forward-char)))))

(defsubst mailcap-look-at-qchar ()
  (let ((chr (char-after (point))))
    (when (eq chr ?\\)
      (forward-char 2)
      (char-before (point))
      )))

(defsubst mailcap-look-at-mtext ()
  (let ((beg (point)))
    (while (or (mailcap-look-at-schar)
	       (mailcap-look-at-schar)))
    (buffer-substring beg (point))
    ))


;;; @ field
;;;

(defsubst mailcap-look-at-field ()
  (let ((token (mailcap-look-at-token)))
    (if token
	(if (looking-at "[ \t]*=[ \t]*")
	    (let ((value (progn
			   (goto-char (match-end 0))
			   (mailcap-look-at-mtext))))
	      (if value
		  (cons token value)
		))
	  (list token)
	  ))))


;;; @ mailcap entry
;;;

(defun mailcap-look-at-entry ()
  (let ((type (mailcap-look-at-type-field)))
    (if (and type (mailcap-skip-field-separator))
	(let ((view (mailcap-look-at-mtext))
	      fields field)
	  (when view
	    (while (and (mailcap-skip-field-separator)
			(setq field (mailcap-look-at-field))
			)
	      (setq fields (cons field fields))
	      )
	    (nconc type
		   (list (cons 'view view))
		   fields))))))


;;; @ main
;;;

(defun mailcap-parse-buffer (&optional buffer order)
  "Parse BUFFER as a mailcap, and return the result.
If optional argument ORDER is a function, result is sorted by it.
If optional argument ORDER is not specified, result is sorted original
order.  Otherwise result is not sorted."
  (save-excursion
    (if buffer
	(set-buffer buffer))
    (goto-char (point-min))
    (let (entries entry)
      (while (progn
	       (while (mailcap-skip-comment))
	       (setq entry (mailcap-look-at-entry))
	       )
	(setq entries (cons entry entries))
	(forward-line)
	)
      (cond ((functionp order) (sort entries order))
	    ((null order) (nreverse entries))
	    (t entries)
	    ))))

(defvar mailcap-file "~/.mailcap"
  "*File name of user's mailcap file.")

(defun mailcap-parse-file (&optional filename order)
  "Parse FILENAME as a mailcap, and return the result.
If optional argument ORDER is a function, result is sorted by it.
If optional argument ORDER is not specified, result is sorted original
order.  Otherwise result is not sorted."
  (or filename
      (setq filename mailcap-file))
  (with-temp-buffer
    (insert-file-contents filename)
    (mailcap-parse-buffer (current-buffer) order)
    ))


;;; @ end
;;;

(provide 'mailcap)

;;; mailcap.el ends here
