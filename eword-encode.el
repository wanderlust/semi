;;; eword-encode.el --- RFC 2047 based encoded-word encoder for GNU Emacs

;; Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Revision$
;; Keywords: encoded-word, MIME, multilingual, header, mail, news

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

(require 'mel)
(require 'std11)
(require 'mime-def)
(require 'cl)

(defsubst find-non-ascii-charset-string (string)
  "Return a list of charsets in the STRING except ascii."
  (delq 'ascii (find-charset-string string))
  )


;;; @ version
;;;

(defconst eword-encode-RCS-ID
  "$Id$")
(defconst eword-encode-version (get-version-string eword-encode-RCS-ID))


;;; @ variables
;;;

(defvar eword-field-encoding-method-alist
  '(("X-Nsubject" . iso-2022-jp-2)
    ("Newsgroups" . nil)
    (t            . mime)
    )
  "*Alist to specify field encoding method.
Its key is field-name, value is encoding method.

If method is `mime', this field will be encoded into MIME format.

If method is a MIME-charset, this field will be encoded as the charset
when it must be convert into network-code.

If method is `default-mime-charset', this field will be encoded as
variable `default-mime-charset' when it must be convert into
network-code.

If method is nil, this field will not be encoded.")

(defvar eword-generate-X-Nsubject nil
  "*If it is not nil, X-Nsubject field is generated
when Subject field is encoded by `eword-encode-header'.")

(defvar eword-charset-encoding-alist
  '((us-ascii		. nil)
    (iso-8859-1		. "Q")
    (iso-8859-2		. "Q")
    (iso-8859-3		. "Q")
    (iso-8859-4		. "Q")
    (iso-8859-5		. "Q")
    (koi8-r		. "Q")
    (iso-8859-7		. "Q")
    (iso-8859-8		. "Q")
    (iso-8859-9		. "Q")
    (iso-2022-jp	. "B")
    (iso-2022-kr	. "B")
    (gb2312		. "B")
    (cn-gb		. "B")
    (cn-gb-2312		. "B")
    (euc-kr		. "B")
    (iso-2022-jp-2	. "B")
    (iso-2022-int-1	. "B")
    ))


;;; @ encoded-text encoder
;;;

(defun tm-eword::encode-encoded-text (charset encoding string &optional mode)
  (let ((text
	 (cond ((string= encoding "B")
		(base64-encode-string string))
	       ((string= encoding "Q")
		(q-encoding-encode-string string mode))
	       )
	 ))
    (if text
	(concat "=?" (upcase (symbol-name charset)) "?"
		encoding "?" text "?=")
      )))


;;; @ leading char
;;;

(defun tm-eword::char-type (chr)
  (if (or (= chr 32)(= chr ?\t))
      nil
    (char-charset chr)
    ))

(defun tm-eword::parse-lc-word (str)
  (let* ((chr (sref str 0))
	 (lc (tm-eword::char-type chr))
	 (i (char-length chr))
	 (len (length str))
	 )
    (while (and (< i len)
		(setq chr (sref str i))
		(eq lc (tm-eword::char-type chr))
		)
      (setq i (+ i (char-length chr)))
      )
    (cons (cons lc (substring str 0 i)) (substring str i))
    ))

(defun tm-eword::split-to-lc-words (str)
  (let (ret dest)
    (while (and (not (string= str ""))
		(setq ret (tm-eword::parse-lc-word str))
		)
      (setq dest (cons (car ret) dest))
      (setq str (cdr ret))
      )
    (reverse dest)
    ))


;;; @ word
;;;

(defun tm-eword::parse-word (lcwl)
  (let* ((lcw (car lcwl))
	 (lc (car lcw))
	 )
    (if (null lc)
	lcwl
      (let ((lcl (list lc))
	    (str (cdr lcw))
	    )
	(catch 'tag
	  (while (setq lcwl (cdr lcwl))
	    (setq lcw (car lcwl))
	    (setq lc (car lcw))
	    (if (null lc)
		(throw 'tag nil)
	      )
	    (if (not (memq lc lcl))
		(setq lcl (cons lc lcl))
	      )
	    (setq str (concat str (cdr lcw)))
	    ))
	(cons (cons lcl str) lcwl)
	))))

(defun tm-eword::lc-words-to-words (lcwl)
  (let (ret dest)
    (while (setq ret (tm-eword::parse-word lcwl))
      (setq dest (cons (car ret) dest))
      (setq lcwl (cdr ret))
      )
    (reverse dest)
    ))


;;; @ rule
;;;

(defmacro tm-eword::make-rword (text charset encoding type)
  (` (list (, text)(, charset)(, encoding)(, type))))
(defmacro tm-eword::rword-text (rword)
  (` (car (, rword))))
(defmacro tm-eword::rword-charset (rword)
  (` (car (cdr (, rword)))))
(defmacro tm-eword::rword-encoding (rword)
  (` (car (cdr (cdr (, rword))))))
(defmacro tm-eword::rword-type (rword)
  (` (car (cdr (cdr (cdr (, rword)))))))

(defun tm-eword::find-charset-rule (charsets)
  (if charsets
      (let* ((charset (charsets-to-mime-charset charsets))
	     (encoding (cdr (assq charset eword-charset-encoding-alist)))
	     )
	(list charset encoding)
	)))

(defun tm-eword::words-to-ruled-words (wl &optional mode)
  (mapcar (function
	   (lambda (word)
	     (let ((ret (tm-eword::find-charset-rule (car word))))
	       (tm-eword::make-rword (cdr word) (car ret)(nth 1 ret) mode)
	       )))
	  wl))

(defun tm-eword::space-process (seq)
  (let (prev a ac b c cc)
    (while seq
      (setq b (car seq))
      (setq seq (cdr seq))
      (setq c (car seq))
      (setq cc (tm-eword::rword-charset c))
      (if (null (tm-eword::rword-charset b))
	  (progn
	    (setq a (car prev))
	    (setq ac (tm-eword::rword-charset a))
	    (if (and (tm-eword::rword-encoding a)
		     (tm-eword::rword-encoding c))
		(cond ((eq ac cc)
		       (setq prev (cons
				   (cons (concat (car a)(car b)(car c))
					 (cdr a))
				   (cdr prev)
				   ))
		       (setq seq (cdr seq))
		       )
		      (t
		       (setq prev (cons
				   (cons (concat (car a)(car b))
					 (cdr a))
				   (cdr prev)
				   ))
		       ))
	      (setq prev (cons b prev))
	      ))
	(setq prev (cons b prev))
	))
    (reverse prev)
    ))

(defun tm-eword::split-string (str &optional mode)
  (tm-eword::space-process
   (tm-eword::words-to-ruled-words (tm-eword::lc-words-to-words
				    (tm-eword::split-to-lc-words str))
				   mode)))


;;; @ length
;;;

(defun tm-eword::encoded-word-length (rword)
  (let ((string   (tm-eword::rword-text     rword))
	(charset  (tm-eword::rword-charset  rword))
	(encoding (tm-eword::rword-encoding rword))
	ret)
    (setq ret
	  (cond ((string-equal encoding "B")
		 (setq string (encode-mime-charset-string string charset))
		 (base64-encoded-length string)
		 )
		((string-equal encoding "Q")
		 (setq string (encode-mime-charset-string string charset))
		 (q-encoding-encoded-length string
					    (tm-eword::rword-type rword))
		 )))
    (if ret
	(cons (+ 7 (length (symbol-name charset)) ret) string)
      )))


;;; @ encode-string
;;;

(defun tm-eword::encode-string-1 (column rwl)
  (let* ((rword (car rwl))
	 (ret (tm-eword::encoded-word-length rword))
	 string len)
    (if (null ret)
	(cond ((and (setq string (car rword))
		    (<= (setq len (+ (length string) column)) 76)
		    )
	       (setq rwl (cdr rwl))
	       )
	      (t
	       (setq string "\n ")
	       (setq len 1)
	       ))
      (cond ((and (setq len (car ret))
		  (<= (+ column len) 76)
		  )
	     (setq string
		   (tm-eword::encode-encoded-text
		    (tm-eword::rword-charset rword)
		    (tm-eword::rword-encoding rword)
		    (cdr ret)
		    (tm-eword::rword-type rword)
		    ))
	     (setq len (+ (length string) column))
	     (setq rwl (cdr rwl))
	     )
	    (t
	     (setq string (car rword))
	     (let* ((p 0) np
		    (str "") nstr)
	       (while (and (< p len)
			   (progn
			     (setq np (+ p (char-length (sref string p))))
			     (setq nstr (substring string 0 np))
			     (setq ret (tm-eword::encoded-word-length
					(cons nstr (cdr rword))
					))
			     (setq nstr (cdr ret))
			     (setq len (+ (car ret) column))
			     (<= len 76)
			     ))
		 (setq str nstr
		       p np))
	       (if (string-equal str "")
		   (setq string "\n "
			 len 1)
		 (setq rwl (cons (cons (substring string p) (cdr rword))
				 (cdr rwl)))
		 (setq string
		       (tm-eword::encode-encoded-text
			(tm-eword::rword-charset rword)
			(tm-eword::rword-encoding rword)
			str
			(tm-eword::rword-type rword)))
		 (setq len (+ (length string) column))
		 )
	       )))
      )
    (list string len rwl)
    ))

(defun tm-eword::encode-rwl (column rwl)
  (let (ret dest ps special str ew-f pew-f)
    (while rwl
      (setq ew-f (nth 2 (car rwl)))
      (if (and pew-f ew-f)
	  (setq rwl (cons '(" ") rwl)
		pew-f nil)
	(setq pew-f ew-f)
	)
      (setq ret (tm-eword::encode-string-1 column rwl))
      (setq str (car ret))
      (if (eq (elt str 0) ?\n)
	  (if (eq special ?\()
	      (progn
		(setq dest (concat dest "\n ("))
		(setq ret (tm-eword::encode-string-1 2 rwl))
		(setq str (car ret))
		))
	(cond ((eq special 32)
	       (if (string= str "(")
		   (setq ps t)
		 (setq dest (concat dest " "))
		 (setq ps nil)
		 ))
	      ((eq special ?\()
	       (if ps
		   (progn
		     (setq dest (concat dest " ("))
		     (setq ps nil)
		     )
		 (setq dest (concat dest "("))
		 )
	       )))
      (cond ((string= str " ")
	     (setq special 32)
	     )
	    ((string= str "(")
	     (setq special ?\()
	     )
	    (t
	     (setq special nil)
	     (setq dest (concat dest str))
	     ))
      (setq column (nth 1 ret)
	    rwl (nth 2 ret))
      )
    (list dest column)
    ))

(defun tm-eword::encode-string (column str &optional mode)
  (tm-eword::encode-rwl column (tm-eword::split-string str mode))
  )


;;; @ converter
;;;

(defun tm-eword::phrase-to-rwl (phrase)
  (let (token type dest str)
    (while phrase
      (setq token (car phrase))
      (setq type (car token))
      (cond ((eq type 'quoted-string)
	     (setq str (concat "\"" (cdr token) "\""))
	     (setq dest
		   (append dest
			   (list
			    (let ((ret (tm-eword::find-charset-rule
					(find-non-ascii-charset-string str))))
			      (tm-eword::make-rword
			       str (car ret)(nth 1 ret) 'phrase)
			      )
			    )))
	     )
	    ((eq type 'comment)
	     (setq dest
		   (append dest
			   '(("(" nil nil))
			   (tm-eword::words-to-ruled-words
			    (tm-eword::lc-words-to-words
			     (tm-eword::split-to-lc-words (cdr token)))
			    'comment)
			   '((")" nil nil))
			   ))
	     )
	    (t
	     (setq dest (append dest
				(tm-eword::words-to-ruled-words
				 (tm-eword::lc-words-to-words
				  (tm-eword::split-to-lc-words (cdr token))
				  ) 'phrase)))
	     ))
      (setq phrase (cdr phrase))
      )
    (tm-eword::space-process dest)
    ))

(defun tm-eword::phrase-route-addr-to-rwl (phrase-route-addr)
  (if (eq (car phrase-route-addr) 'phrase-route-addr)
      (let ((phrase (nth 1 phrase-route-addr))
	    (route (nth 2 phrase-route-addr))
	    dest)
	(if (eq (car (car phrase)) 'spaces)
	    (setq phrase (cdr phrase))
	  )
	(setq dest (tm-eword::phrase-to-rwl phrase))
	(if dest
	    (setq dest (append dest '((" " nil nil))))
	  )
	(append
	 dest
	 (list (list (concat "<" (std11-addr-to-string route) ">") nil nil))
	 ))))

(defun tm-eword::addr-spec-to-rwl (addr-spec)
  (if (eq (car addr-spec) 'addr-spec)
      (list (list (std11-addr-to-string (cdr addr-spec)) nil nil))
    ))

(defun tm-eword::mailbox-to-rwl (mbox)
  (let ((addr (nth 1 mbox))
	(comment (nth 2 mbox))
	dest)
    (setq dest (or (tm-eword::phrase-route-addr-to-rwl addr)
		   (tm-eword::addr-spec-to-rwl addr)
		   ))
    (if comment
	(setq dest
	      (append dest
		      '((" " nil nil)
			("(" nil nil))
		      (tm-eword::split-string comment 'comment)
		      '((")" nil nil))
		      )))
    dest))

(defun tm-eword::addresses-to-rwl (addresses)
  (let ((dest (tm-eword::mailbox-to-rwl (car addresses))))
    (if dest
	(while (setq addresses (cdr addresses))
	  (setq dest (append dest
			     '(("," nil nil))
			     '((" " nil nil))
			     (tm-eword::mailbox-to-rwl (car addresses))
			     ))
	  ))
    dest))

(defun tm-eword::encode-address-list (column str)
  (tm-eword::encode-rwl
   column
   (tm-eword::addresses-to-rwl (std11-parse-addresses-string str))
   ))


;;; @ application interfaces
;;;

(defun eword-encode-field (str)
  (setq str (std11-unfold-string str))
  (let ((ret (string-match std11-field-head-regexp str)))
    (or (if ret
	    (let ((field-name (substring str 0 (1- (match-end 0))))
		  (field-body (eliminate-top-spaces
			       (substring str (match-end 0))))
		  fname)
	      (if (setq ret
			(cond ((string-equal field-body "") "")
			      ((member (setq fname (downcase field-name))
				       '("reply-to" "from" "sender"
					 "resent-reply-to" "resent-from"
					 "resent-sender" "to" "resent-to"
					 "cc" "resent-cc"
					 "bcc" "resent-bcc" "dcc")
				       )
			       (car (tm-eword::encode-address-list
				     (+ (length field-name) 2) field-body))
			       )
			      (t
			       (car (tm-eword::encode-string
				     (+ (length field-name) 1)
				     field-body 'text))
			       ))
			)
		  (concat field-name ": " ret)
		)))
	(car (tm-eword::encode-string 0 str))
	)))

(defun eword-in-subject-p ()
  (let ((str (std11-field-body "Subject")))
    (if (and str (string-match eword-encoded-word-regexp str))
	str)))

(defun eword-encode-header (&optional code-conversion)
  "Encode header fields to network representation, such as MIME encoded-word.

It refer variable `eword-field-encoding-method-alist'."
  (interactive "*")
  (save-excursion
    (save-restriction
      (std11-narrow-to-header mail-header-separator)
      (goto-char (point-min))
      (let ((default-cs (mime-charset-to-coding-system default-mime-charset))
	    beg end field-name)
	(while (re-search-forward std11-field-head-regexp nil t)
	  (setq beg (match-beginning 0))
	  (setq field-name (buffer-substring beg (1- (match-end 0))))
	  (setq end (std11-field-end))
	  (and (find-non-ascii-charset-region beg end)
	       (let ((ret (or (let ((fname  (downcase field-name)))
				(assoc-if
				 (function
				  (lambda (str)
				    (and (stringp str)
					 (string= fname (downcase str))
					 )))
				 eword-field-encoding-method-alist))
			      (assq t eword-field-encoding-method-alist)
			      )))
		 (if ret
		     (let ((method (cdr ret)))
		       (cond ((eq method 'mime)
			      (let ((field
				     (buffer-substring-no-properties beg end)
				     ))
				(delete-region beg end)
				(insert (eword-encode-field field))
				))
			     (code-conversion
			      (let ((cs
				     (or (mime-charset-to-coding-system
					  method)
					 default-cs)))
				(encode-coding-region beg end cs)
				)))
		       ))
		 ))
	  ))
      (and eword-generate-X-Nsubject
	   (or (std11-field-body "X-Nsubject")
	       (let ((str (eword-in-subject-p)))
		 (if str
		     (progn
		       (setq str
			     (eword-decode-string
			      (std11-unfold-string str)))
		       (if code-conversion
			   (setq str
				 (encode-mime-charset-string
				  str
				  (or (cdr (assoc-if
					    (function
					     (lambda (str)
					       (and (stringp str)
						    (string= "x-nsubject"
							     (downcase str2))
						    )))
					    eword-field-encoding-method-alist))
				      'iso-2022-jp-2)))
			 )
		       (insert (concat "\nX-Nsubject: " str))
		       )))))
      )))

(defun eword-encode-string (str &optional column mode)
  (car (tm-eword::encode-rwl (or column 0) (tm-eword::split-string str mode)))
  )


;;; @ end
;;;

(provide 'eword-encode)

;;; eword-encode.el ends here
