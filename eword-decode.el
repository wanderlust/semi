;;; eword-decode.el --- RFC 2047 based encoded-word decoder for GNU Emacs

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: ENAMI Tsugutomo <enami@sys.ptg.sony.co.jp>
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1995/10/03
;; Original: 1992/07/20 ENAMI Tsugutomo's `mime.el'.
;;	Renamed: 1993/06/03 to tiny-mime.el
;;	Renamed: 1995/10/03 from tiny-mime.el (split off encoder)
;;	Renamed: 1997/02/22 from tm-ew-d.el
;; Keywords: encoded-word, MIME, multilingual, header, mail, news

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

(require 'std11-parse)
(require 'mel)
(require 'mime-def)

(defgroup eword-decode nil
  "Encoded-word decoding"
  :group 'mime)

(defconst eword-decode-version
  `,(mapconcat #'number-to-string (cdr semi-version) "."))


;;; @ MIME encoded-word definition
;;;

(defconst eword-encoded-text-regexp "[!->@-~]+")
(defconst eword-encoded-word-regexp
  (concat (regexp-quote "=?")
	  "\\("
	  mime-charset-regexp
	  "\\)"
	  (regexp-quote "?")
	  "\\(B\\|Q\\)"
	  (regexp-quote "?")
	  "\\("
	  eword-encoded-text-regexp
	  "\\)"
	  (regexp-quote "?=")))


;;; @@ Base64
;;;

(defconst base64-token-regexp "[A-Za-z0-9+/]")
(defconst base64-token-padding-regexp "[A-Za-z0-9+/=]")

(defconst eword-B-encoded-text-regexp
  (concat "\\(\\("
	  base64-token-regexp
	  base64-token-regexp
	  base64-token-regexp
	  base64-token-regexp
	  "\\)*"
	  base64-token-regexp
	  base64-token-regexp
	  base64-token-padding-regexp
	  base64-token-padding-regexp
          "\\)"))

;; (defconst eword-B-encoding-and-encoded-text-regexp
;;   (concat "\\(B\\)\\?" eword-B-encoded-text-regexp))


;;; @@ Quoted-Printable
;;;

(defconst quoted-printable-hex-chars "0123456789ABCDEF")
(defconst quoted-printable-octet-regexp
  (concat "=[" quoted-printable-hex-chars
	  "][" quoted-printable-hex-chars "]"))

(defconst eword-Q-encoded-text-regexp
  (concat "\\([^=?]\\|" quoted-printable-octet-regexp "\\)+"))
;; (defconst eword-Q-encoding-and-encoded-text-regexp
;;   (concat "\\(Q\\)\\?" eword-Q-encoded-text-regexp))


;;; @ for string
;;;

(defvar eword-decode-sticked-encoded-word nil
  "*If non-nil, decode encoded-words sticked on encoded-words, atoms, etc.")

(defun eword-decode-first-encoded-words (string after-regexp &optional must-unfold)
  (if eword-decode-sticked-encoded-word (setq after-regexp ""))
  (let ((between-ewords-regexp (if eword-decode-sticked-encoded-word "\\(\n?[ \t]\\)*" "\\(\n?[ \t]\\)+"))
  	(src string)	; sequence of octets.
  	(dst ""))	; sequence of characters.
    (if (string-match (concat "\\`\\(" eword-encoded-word-regexp "\\)" after-regexp) src)
      (let* (p
      	     (q (match-end 1))
      	     (ew (substring src 0 q))
      	     (dw (eword-decode-encoded-word ew must-unfold)))
        (setq dst (concat dst dw)
	      src (substring src q))
	(if (not (string= ew dw))
	  (progn
	    (while
	      (and
	        (string-match
		  (concat "\\`\\(" between-ewords-regexp "\\)\\(" eword-encoded-word-regexp "\\)" after-regexp)
		  src)
		(progn
		  (setq p (match-end 1)
		  	q (match-end 3)
		        ew (substring src p q)
		        dw (eword-decode-encoded-word ew must-unfold))
		  (if (string= ew dw)
		    (progn
		      (setq dst (concat dst (substring src 0 q))
			    src (substring src q))
		      nil)
		    t)))
	      (setq dst (concat dst dw)
	            src (substring src q)))))
	(cons dst src))
      nil)))

(defun eword-decode-comment-string (string &optional must-unfold)
  (let ((src string)
	(buf "")
  	(dst "")
	(flag-ew t))
    (while (< 0 (length src))
      (let ((ch (aref src 0))
      	    (decoded (and flag-ew (eword-decode-first-encoded-words src "\\([ \t()\\\\]\\|$\\)" must-unfold))))
	(if (and (not (string= buf ""))
		 (or decoded (eq ch ?\() (eq ch ?\))))
	  (setq dst (concat dst (std11-wrap-as-quoted-pairs (decode-mime-charset-string buf default-mime-charset) '(?( ?))))
		buf ""))
	(cond
	  (decoded
	    (setq dst (concat dst (std11-wrap-as-quoted-pairs (car decoded) '(?( ?))))
		  src (cdr decoded)))
	  ((or (eq ch ?\() (eq ch ?\)))
	    (setq dst (concat dst (list ch))
		  src (substring src 1)
		  flag-ew t))
	  ((eq ch ?\\)
	    (setq buf (concat buf (list (aref src 1)))
		  src (substring src 2)
		  flag-ew t))
	  ((or (eq ch ?\ ) (eq ch ?\t) (eq ch ?\n))
	    (setq buf (concat buf (list ch))
		  src (substring src 1)
		  flag-ew t))
	  ((string-match "\\`=?[^ \t\n()\\\\=]*" src)
	    (setq buf (concat buf (substring src 0 (match-end 0)))
		  src (substring src (match-end 0))
		  flag-ew eword-decode-sticked-encoded-word))
	  (t (error "something wrong")))))
    (if (not (string= buf ""))
      (setq dst (concat dst (std11-wrap-as-quoted-pairs (decode-mime-charset-string buf default-mime-charset) '(?( ?))))))
    dst))

(defun eword-decode-unstructured-string (string &optional must-unfold)
  (let ((src string)
	(buf "")
  	(dst "")
	(flag-ew t))
    (while (< 0 (length src))
      (let ((ch (aref src 0))
      	    (decoded (and flag-ew (eword-decode-first-encoded-words src "\\([ \t]\\|$\\)" must-unfold))))
	(if (and (not (string= buf ""))
		 decoded)
	  (setq dst (concat dst (decode-mime-charset-string buf default-mime-charset))
		buf ""))
	(cond
	  (decoded
	    (setq dst (concat dst (car decoded))
		  src (cdr decoded)))
	  ((or (eq ch ?\ ) (eq ch ?\t) (eq ch ?\n))
	    (setq buf (concat buf (list ch))
		  src (substring src 1)
		  flag-ew t))
	  ((string-match "\\`=?[^ \t\n=]*" src)
	    (setq buf (concat buf (substring src 0 (match-end 0)))
		  src (substring src (match-end 0))
		  flag-ew eword-decode-sticked-encoded-word))
	  (t (error "something wrong")))))
    (if (not (string= buf ""))
      (setq dst (concat dst (decode-mime-charset-string buf default-mime-charset))))
    dst))

(defun eword-decode-string (string &optional must-unfold)
  "Decode MIME encoded-words in STRING.

STRING is unfolded before decoding.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is not decoded.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-words (generated by bad manner MUA
such as a version of Net$cape)."
  (eword-decode-unstructured-string (std11-unfold-string string) must-unfold))


;;; @ for region
;;;

(defun eword-decode-region (start end &optional unfolding must-unfold)
  "Decode MIME encoded-words in region between START and END.

If UNFOLDING is not nil, it unfolds before decoding.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-words (generated by bad manner MUA
such as a version of Net$cape)."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (if unfolding
	  (eword-decode-unfold)
	)
      (let ((str (eword-decode-unstructured-string (buffer-substring (point-min) (point-max)) must-unfold)))
	(delete-region (point-min) (point-max))
	(insert str)))))


;;; @ for message header
;;;

(defcustom eword-decode-ignored-field-list
  '(newsgroups path lines nntp-posting-host message-id date)
  "*List of field-names to be ignored when decoding.
Each field name must be symbol."
  :group 'eword-decode
  :type '(repeat symbol))

(defcustom eword-decode-structured-field-list
  '(reply-to resent-reply-to from resent-from sender resent-sender
	     to resent-to cc resent-cc bcc resent-bcc dcc
	     mime-version content-type content-transfer-encoding
	     content-disposition)
  "*List of field-names to decode as structured field.
Each field name must be symbol."
  :group 'eword-decode
  :type '(repeat symbol))

(defun eword-decode-header (&optional code-conversion separator)
  "Decode MIME encoded-words in header fields.
If CODE-CONVERSION is nil, it decodes only encoded-words.  If it is
mime-charset, it decodes non-ASCII bit patterns as the mime-charset.
Otherwise it decodes non-ASCII bit patterns as the
default-mime-charset.
If SEPARATOR is not nil, it is used as header separator."
  (interactive "*")
  (save-excursion
    (save-restriction
      (std11-narrow-to-header separator)
      (let ((default-charset
	      (if code-conversion
		  (if (mime-charset-to-coding-system code-conversion)
		      code-conversion
		    default-mime-charset))))
	(if default-charset
	    (let (beg p end field-name len)
	      (goto-char (point-min))
	      (while (re-search-forward std11-field-head-regexp nil t)
		(setq beg (match-beginning 0)
		      p (match-end 0)
		      field-name (buffer-substring beg (1- p))
		      len (string-width field-name)
		      field-name (intern (downcase field-name))
		      end (std11-field-end))
		(cond ((memq field-name eword-decode-ignored-field-list)
		       ;; Don't decode
		       )
		      ((memq field-name eword-decode-structured-field-list)
		       ;; Decode as structured field
		       (let ((body (buffer-substring p end))
			     (default-mime-charset default-charset))
			 (delete-region p end)
			 (insert (eword-decode-and-fold-structured-field
				  body (1+ len)))
			 ))
		      (t
		       ;; Decode as unstructured field
		       (save-restriction
			 (narrow-to-region beg (1+ end))
			 (goto-char p)
			 (eword-decode-region beg (point-max) 'unfold)
			 (goto-char (point-max))
			 )))))
	  (eword-decode-region (point-min) (point-max) t)
	  )))))

(defun eword-decode-unfold ()
  (goto-char (point-min))
  (let (field beg end)
    (while (re-search-forward std11-field-head-regexp nil t)
      (setq beg (match-beginning 0)
            end (std11-field-end))
      (setq field (buffer-substring beg end))
      (if (string-match eword-encoded-word-regexp field)
          (save-restriction
            (narrow-to-region (goto-char beg) end)
            (while (re-search-forward "\n\\([ \t]\\)" nil t)
              (replace-match (match-string 1))
              )
	    (goto-char (point-max))
	    ))
      )))


;;; @ encoded-word decoder
;;;

(defvar eword-warning-face nil "Face used for invalid encoded-word.")

(defun eword-decode-encoded-word (word &optional must-unfold)
  "Decode WORD if it is an encoded-word.

If your emacs implementation can not decode the charset of WORD, it
returns WORD.  Similarly the encoded-word is broken, it returns WORD.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-word (generated by bad manner MUA such
as a version of Net$cape)."
  (or (if (string-match eword-encoded-word-regexp word)
	  (let ((charset
		 (substring word (match-beginning 1) (match-end 1))
		 )
		(encoding
		 (upcase
		  (substring word (match-beginning 2) (match-end 2))
		  ))
		(text
		 (substring word (match-beginning 3) (match-end 3))
		 ))
            (condition-case err
                (eword-decode-encoded-text charset encoding text must-unfold)
              (error
               (and
		(add-text-properties 0 (length word)
				     (and eword-warning-face
					  (list 'face eword-warning-face))
				     word)
		word)))
            ))
      word))


;;; @ encoded-text decoder
;;;

(defun eword-decode-encoded-text (charset encoding string
					  &optional must-unfold)
  "Decode STRING as an encoded-text.

If your emacs implementation can not decode CHARSET, it returns nil.

If ENCODING is not \"B\" or \"Q\", it occurs error.
So you should write error-handling code if you don't want break by errors.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-text (generated by bad manner MUA such
as a version of Net$cape)."
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(let ((dest
               (cond
                ((string-equal "B" encoding)
                 (if (and (string-match eword-B-encoded-text-regexp string)
                          (string-equal string (match-string 0 string)))
                     (base64-decode-string string)
                   (error "Invalid encoded-text %s" string)))
                ((string-equal "Q" encoding)
                 (if (and (string-match eword-Q-encoded-text-regexp string)
                          (string-equal string (match-string 0 string)))
                     (q-encoding-decode-string string)
                   (error "Invalid encoded-text %s" string)))
                (t
                 (error "Invalid encoding %s" encoding)
                 )))
              )
	  (if dest
	      (progn
		(setq dest (decode-coding-string dest cs))
		(if must-unfold
		    (mapconcat (function
				(lambda (chr)
				  (cond
                                   ((eq chr ?\n) "")
                                   ((eq chr ?\t) " ")
                                   (t (char-to-string chr)))
				  ))
			       (std11-unfold-string dest)
			       "")
		  dest)
		))))))


;;; @ lexical analyze
;;;

(defvar eword-lexical-analyze-cache nil)
(defvar eword-lexical-analyze-cache-max 299
  "*Max position of eword-lexical-analyze-cache.
It is max size of eword-lexical-analyze-cache - 1.")

(defcustom eword-lexical-analyzers
  '(eword-analyze-quoted-string
    eword-analyze-domain-literal
    eword-analyze-comment
    eword-analyze-spaces
    eword-analyze-special
    eword-analyze-encoded-word
    eword-analyze-atom)
  "*List of functions to return result of lexical analyze.
Each function must have two arguments: STRING and MUST-UNFOLD.
STRING is the target string to be analyzed.
If MUST-UNFOLD is not nil, each function must unfold and eliminate
bare-CR and bare-LF from the result even if they are included in
content of the encoded-word.
Each function must return nil if it can not analyze STRING as its
format.

Previous function is preferred to next function.  If a function
returns nil, next function is used.  Otherwise the return value will
be the result."
  :group 'eword-decode
  :type '(repeat function))

(defun eword-analyze-quoted-string (string &optional must-unfold)
  (let ((p (std11-check-enclosure string ?\" ?\")))
    (if p
	(cons (cons 'quoted-string
		    (std11-wrap-as-quoted-string
		     (decode-mime-charset-string
		      (std11-strip-quoted-pair (substring string 1 (1- p)))
		      default-mime-charset)))
	      (substring string p))
      )))

(defun eword-analyze-domain-literal (string &optional must-unfold)
  (std11-analyze-domain-literal string))

(defun eword-analyze-comment (string &optional must-unfold)
  (let ((p (std11-check-enclosure string ?\( ?\) t)))
    (if p
	(cons (cons 'comment (eword-decode-comment-string (substring string 0 p)))
	      (substring string p))
      )))

(defun eword-analyze-spaces (string &optional must-unfold)
  (std11-analyze-spaces string))

(defun eword-analyze-special (string &optional must-unfold)
  (std11-analyze-special string))

(defun eword-analyze-encoded-word (string &optional must-unfold)
  (let ((decoded (eword-decode-first-encoded-words string "\\([ \t(]\\|$\\)" must-unfold)))
    (if decoded
      (cons (cons 'atom (car decoded)) (cdr decoded)))))

(defun eword-analyze-atom (string &optional must-unfold)
  (if (string-match std11-atom-regexp string)
      (let ((end (match-end 0)))
	(cons (cons 'atom (decode-mime-charset-string
			   (substring string 0 end)
			   default-mime-charset))
	      (substring string end)
	      ))))

(defun eword-lexical-analyze-internal (string must-unfold)
  (let (dest ret)
    (while (not (string-equal string ""))
      (setq ret
	    (let ((rest eword-lexical-analyzers)
		  func r)
	      (while (and (setq func (car rest))
			  (null (setq r (funcall func string must-unfold)))
			  )
		(setq rest (cdr rest)))
	      (or r '((error) . ""))
	      ))
      (setq dest (cons (car ret) dest))
      (setq string (cdr ret))
      )
    (nreverse dest)
    ))

(defun eword-lexical-analyze (string &optional must-unfold)
  "Return lexical analyzed list corresponding STRING.
It is like std11-lexical-analyze, but it decodes non us-ascii
characters encoded as encoded-words or invalid \"raw\" format.
\"Raw\" non us-ascii characters are regarded as variable
`default-mime-charset'."
  (let ((key (copy-sequence string))
	ret)
    (set-text-properties 0 (length key) nil key)
    (if (setq ret (assoc key eword-lexical-analyze-cache))
	(cdr ret)
      (setq ret (eword-lexical-analyze-internal key must-unfold))
      (setq eword-lexical-analyze-cache
	    (cons (cons key ret)
		  (last eword-lexical-analyze-cache
			eword-lexical-analyze-cache-max)))
      ret)))

(defun eword-decode-token (token)
  (cdr token))

(defun eword-decode-and-fold-structured-field
  (string start-column &optional max-column must-unfold)
  "Decode and fold (fill) STRING as structured field body.
It decodes non us-ascii characters in FULL-NAME encoded as
encoded-words or invalid \"raw\" string.  \"Raw\" non us-ascii
characters are regarded as variable `default-mime-charset'.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is not decoded.

If MAX-COLUMN is omitted, `fill-column' is used.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-words (generated by bad manner MUA
such as a version of Net$cape)."
  (or max-column
      (setq max-column fill-column))
  (let ((c start-column)
	(tokens (eword-lexical-analyze string must-unfold))
	(result "")
	token)
    (while (and (setq token (car tokens))
		(setq tokens (cdr tokens)))
      (let* ((type (car token)))
	(if (eq type 'spaces)
	    (let* ((next-token (car tokens))
		   (next-str (eword-decode-token next-token))
		   (next-len (string-width next-str))
		   (next-c (+ c next-len 1)))
	      (if (< next-c max-column)
		  (setq result (concat result " " next-str)
			c next-c)
		(setq result (concat result "\n " next-str)
		      c (1+ next-len)))
	      (setq tokens (cdr tokens))
	      )
	  (let* ((str (eword-decode-token token)))
	    (setq result (concat result str)
		  c (+ c (string-width str)))
	    ))))
    (if token
	(concat result (eword-decode-token token))
      result)))

(defun eword-decode-and-unfold-structured-field (string)
  "Decode and unfold STRING as structured field body.
It decodes non us-ascii characters in FULL-NAME encoded as
encoded-words or invalid \"raw\" string.  \"Raw\" non us-ascii
characters are regarded as variable `default-mime-charset'.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is not decoded."
  (let ((tokens (eword-lexical-analyze string 'must-unfold))
	(result ""))
    (while tokens
      (let* ((token (car tokens))
	     (type (car token)))
	(setq tokens (cdr tokens))
	(setq result
	      (if (eq type 'spaces)
		  (concat result " ")
		(concat result (eword-decode-token token))
		))))
    result))

(defun eword-decode-structured-field-body (string &optional must-unfold
						  start-column max-column)
  "Decode non us-ascii characters in STRING as structured field body.
STRING is unfolded before decoding.

It decodes non us-ascii characters in FULL-NAME encoded as
encoded-words or invalid \"raw\" string.  \"Raw\" non us-ascii
characters are regarded as variable `default-mime-charset'.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is not decoded.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-words (generated by bad manner MUA
such as a version of Net$cape)."
  (if start-column
      ;; fold with max-column
      (eword-decode-and-fold-structured-field
       string start-column max-column must-unfold)
    ;; Don't fold
    (mapconcat (function eword-decode-token)
	       (eword-lexical-analyze string must-unfold)
	       "")
    ))

(defun eword-decode-unstructured-field-body (string &optional must-unfold)
  "Decode non us-ascii characters in STRING as unstructured field body.
STRING is unfolded before decoding.

It decodes non us-ascii characters in FULL-NAME encoded as
encoded-words or invalid \"raw\" string.  \"Raw\" non us-ascii
characters are regarded as variable `default-mime-charset'.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is not decoded.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-words (generated by bad manner MUA
such as a version of Net$cape)."
  (eword-decode-string string must-unfold))

(defun eword-extract-address-components (string)
  "Extract full name and canonical address from STRING.
Returns a list of the form (FULL-NAME CANONICAL-ADDRESS).
If no name can be extracted, FULL-NAME will be nil.
It decodes non us-ascii characters in FULL-NAME encoded as
encoded-words or invalid \"raw\" string.  \"Raw\" non us-ascii
characters are regarded as variable `default-mime-charset'."
  (let* ((structure (car (std11-parse-address
			  (eword-lexical-analyze
			   (std11-unfold-string string) 'must-unfold))))
         (phrase  (std11-full-name-string structure))
         (address (std11-address-string structure))
         )
    (list phrase address)
    ))


;;; @ end
;;;

(provide 'eword-decode)

;;; eword-decode.el ends here
