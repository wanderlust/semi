;;; mime-def.el --- definition module for SEMI

;; Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id$
;; Keywords: definition, MIME, multimedia, mail, news

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

;;; @ variables
;;;

(defvar mime/tmp-dir (or (getenv "TM_TMP_DIR") "/tmp/"))

(defvar mime/use-multi-frame
  (and (>= emacs-major-version 19) window-system))

(defvar mime/find-file-function
  (if mime/use-multi-frame
      (function find-file-other-frame)
    (function find-file)
    ))

(defvar mime/output-buffer-window-is-shared-with-bbdb t
  "*If t, mime/output-buffer window is shared with BBDB window.")


;;; @ constants
;;;

(defconst mime/output-buffer-name "*MIME-out*")
(defconst mime/temp-buffer-name " *MIME-temp*")


;;; @ definitions about MIME
;;;

(defconst mime/tspecials "][\000-\040()<>@,\;:\\\"/?.=")
(defconst mime/token-regexp (concat "[^" mime/tspecials "]+"))
(defconst mime-charset-regexp mime/token-regexp)

(defconst mime/content-type-subtype-regexp
  (concat mime/token-regexp "/" mime/token-regexp))

(defconst mime/disposition-type-regexp mime/token-regexp)


;;; @ MIME charset
;;;

(defvar charsets-mime-charset-alist
  '(((ascii)						. us-ascii)
    ((ascii latin-iso8859-1)				. iso-8859-1)
    ((ascii latin-iso8859-2)				. iso-8859-2)
    ((ascii latin-iso8859-3)				. iso-8859-3)
    ((ascii latin-iso8859-4)				. iso-8859-4)
;;; ((ascii cyrillic-iso8859-5)				. iso-8859-5)
    ((ascii cyrillic-iso8859-5)				. koi8-r)
    ((ascii arabic-iso8859-6)				. iso-8859-6)
    ((ascii greek-iso8859-7)				. iso-8859-7)
    ((ascii hebrew-iso8859-8)				. iso-8859-8)
    ((ascii latin-iso8859-9)				. iso-8859-9)
    ((ascii latin-jisx0201
	    japanese-jisx0208-1978 japanese-jisx0208)	. iso-2022-jp)
    ((ascii korean-ksc5601)				. euc-kr)
    ((ascii chinese-gb2312)				. cn-gb-2312)
    ((ascii chinese-big5-1 chinese-big5-2)		. cn-big5)
    ((ascii latin-iso8859-1 greek-iso8859-7
	    latin-jisx0201 japanese-jisx0208-1978
	    chinese-gb2312 japanese-jisx0208
	    korean-ksc5601 japanese-jisx0212)		. iso-2022-jp-2)
    ((ascii latin-iso8859-1 greek-iso8859-7
	    latin-jisx0201 japanese-jisx0208-1978
	    chinese-gb2312 japanese-jisx0208
	    korean-ksc5601 japanese-jisx0212
	    chinese-cns11643-1 chinese-cns11643-2)	. iso-2022-int-1)
    ((ascii latin-iso8859-1 latin-iso8859-2
	    cyrillic-iso8859-5 greek-iso8859-7
	    latin-jisx0201 japanese-jisx0208-1978
	    chinese-gb2312 japanese-jisx0208
	    korean-ksc5601 japanese-jisx0212
	    chinese-cns11643-1 chinese-cns11643-2
	    chinese-cns11643-3 chinese-cns11643-4
	    chinese-cns11643-5 chinese-cns11643-6
	    chinese-cns11643-7)				. iso-2022-cjk)
    ))

(defvar default-mime-charset 'x-ctext)

(defvar mime-charset-coding-system-alist
  '((x-ctext		. ctext)
    (gb2312		. cn-gb-2312)
    (iso-2022-jp-2	. iso-2022-ss2-7)
    ))

(defun mime-charset-to-coding-system (charset &optional lbt)
  (if (stringp charset)
      (setq charset (intern (downcase charset)))
    )
  (let ((cs
	 (or (cdr (assq charset mime-charset-coding-system-alist))
	     (and (coding-system-p charset) charset)
	     )))
    (if lbt
	(intern (concat (symbol-name cs) "-" (symbol-name lbt)))
      cs)))

(defun detect-mime-charset-region (start end)
  "Return MIME charset for region between START and END."
  (charsets-to-mime-charset
   (find-charset-string (buffer-substring start end))
   ))

(defun encode-mime-charset-region (start end charset)
  "Encode the text between START and END as MIME CHARSET."
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(encode-coding-region start end cs)
      )))

(defun decode-mime-charset-region (start end charset)
  "Decode the text between START and END as MIME CHARSET."
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(decode-coding-region start end cs)
      )))

(defun encode-mime-charset-string (string charset)
  "Encode the STRING as MIME CHARSET."
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(encode-coding-string string cs)
      string)))

(defun decode-mime-charset-string (string charset)
  "Decode the STRING as MIME CHARSET."
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(decode-coding-string string cs)
      string)))


;;; @ button
;;;

(defvar running-xemacs (string-match "XEmacs" emacs-version))

(if running-xemacs
    (require 'overlay)
  )

(defvar mime-button-face 'bold
  "Face used for content-button or URL-button of MIME-Preview buffer.")

(defvar mime-button-mouse-face 'highlight
  "Face used for MIME-preview buffer mouse highlighting.")

(defun mime-add-button (from to func &optional data)
  "Create a button between FROM and TO with callback FUNC and data DATA."
  (and mime-button-face
       (overlay-put (make-overlay from to) 'face mime-button-face))
  (add-text-properties from to
		       (nconc
			(and mime-button-mouse-face
			     (list 'mouse-face mime-button-mouse-face))
			(list 'mime-button-callback func)
			(and data (list 'mime-button-data data))
			))
  )

(defvar mime-button-mother-dispatcher nil)

(defun mime-button-dispatcher (event)
  "Select the button under point."
  (interactive "e")
  (let (buf point func data)
    (save-window-excursion
      (mouse-set-point event)
      (setq buf (current-buffer)
	    point (point)
	    func (get-text-property (point) 'mime-button-callback)
	    data (get-text-property (point) 'mime-button-data)
	    )
      )
    (save-excursion
      (set-buffer buf)
      (goto-char point)
      (if func
	  (apply func data)
	(if (fboundp mime-button-mother-dispatcher)
	    (funcall mime-button-mother-dispatcher event)
	  )
	))))


;;; @ PGP
;;;

(defvar pgp-function-alist
  '(
    ;; for mime-pgp
    (verify		mc-verify			"mc-toplev")
    (decrypt		mc-decrypt			"mc-toplev")
    (fetch-key		mc-pgp-fetch-key		"mc-pgp")
    (snarf-keys		mc-snarf-keys			"mc-toplev")
    ;; for mime-edit
    (mime-sign		tm:mc-pgp-sign-region		"mime-mc")
    (traditional-sign	mc-pgp-sign-region		"mc-pgp")
    (encrypt		tm:mc-pgp-encrypt-region	"mime-mc")
    (insert-key		mc-insert-public-key		"mc-toplev")
    )
  "Alist of service names vs. corresponding functions and its filenames.
Each element looks like (SERVICE FUNCTION FILE).

SERVICE is a symbol of PGP processing.  It allows `verify', `decrypt',
`fetch-key', `snarf-keys', `mime-sign', `traditional-sign', `encrypt'
or `insert-key'.

Function is a symbol of function to do specified SERVICE.

FILE is string of filename which has definition of corresponding
FUNCTION.")

(defmacro pgp-function (method)
  "Return function to do service METHOD."
  (` (car (cdr (assq (, method) (symbol-value 'pgp-function-alist)))))
  )

(mapcar (function
	 (lambda (method)
	   (autoload (second method)(third method))
	   ))
	pgp-function-alist)


;;; @ method selector kernel
;;;

;;; @@ field unifier
;;;

(defun field-unifier-for-default (a b)
  (let ((ret
	 (cond ((equal a b)    a)
	       ((null (cdr b)) a)
	       ((null (cdr a)) b)
	       )))
    (if ret
	(list nil ret nil)
      )))

(defun field-unifier-for-mode (a b)
  (let ((va (cdr a)))
    (if (if (consp va)
	    (member (cdr b) va)
	  (equal va (cdr b))
	  )
	(list nil b nil)
      )))

(defun field-unify (a b)
  (let ((sym (intern (concat "field-unifier-for-" (symbol-name (car a))))))
    (or (fboundp sym)
	(setq sym (function field-unifier-for-default))
	)
    (funcall sym a b)
    ))


;;; @@ type unifier
;;;

(defun assoc-unify (class instance)
  (catch 'tag
    (let ((cla (copy-alist class))
	  (ins (copy-alist instance))
	  (r class)
	  cell aret ret prev rest)
      (while r
	(setq cell (car r))
	(setq aret (assoc (car cell) ins))
	(if aret
	    (if (setq ret (field-unify cell aret))
		(progn
		  (if (car ret)
		      (setq prev (put-alist (car (car ret))
					    (cdr (car ret))
					    prev))
		    )
		  (if (nth 2 ret)
		      (setq rest (put-alist (car (nth 2 ret))
					    (cdr (nth 2 ret))
					    rest))
		    )
		  (setq cla (put-alist (car cell)(cdr (nth 1 ret)) cla))
		  (setq ins (del-alist (car cell) ins))
		  )
	      (throw 'tag nil)
	      ))
	(setq r (cdr r))
	)
      (setq r (copy-alist ins))
      (while r
	(setq cell (car r))
	(setq aret (assoc (car cell) cla))
	(if aret
	    (if (setq ret (field-unify cell aret))
		(progn
		  (if (car ret)
		      (setq prev (put-alist (car (car ret))
					    (cdr (car ret))
					    prev))
		    )
		  (if (nth 2 ret)
		      (setq rest (put-alist (car (nth 2 ret))
					    (cdr (nth 2 ret))
					    rest))
		    )
		  (setq cla (del-alist (car cell) cla))
		  (setq ins (put-alist (car cell)(cdr (nth 1 ret)) ins))
		  )
	      (throw 'tag nil)
	      ))
	(setq r (cdr r))
	)
      (list prev (append cla ins) rest)
      )))

(defun get-unified-alist (db al)
  (let ((r db) ret)
    (catch 'tag
      (while r
	(if (setq ret (nth 1 (assoc-unify (car r) al)))
	    (throw 'tag ret)
	  )
	(setq r (cdr r))
	))))

(defun delete-atype (atl al)
  (let* ((r atl) ret oal)
    (setq oal
	  (catch 'tag
	    (while r
	      (if (setq ret (nth 1 (assoc-unify (car r) al)))
		  (throw 'tag (car r))
		)
	      (setq r (cdr r))
	      )))
    (delete oal atl)
    ))

(defun remove-atype (sym al)
  (and (boundp sym)
       (set sym (delete-atype (eval sym) al))
       ))

(defun replace-atype (atl old-al new-al)
  (let* ((r atl) ret oal)
    (if (catch 'tag
	  (while r
	    (if (setq ret (nth 1 (assoc-unify (car r) old-al)))
		(throw 'tag (rplaca r new-al))
	      )
	    (setq r (cdr r))
	    ))
	atl)))

(defun set-atype (sym al &rest options)
  (if (null (boundp sym))
      (set sym al)
    (let* ((replacement (memq 'replacement options))
	   (ignore-fields (car (cdr (memq 'ignore options))))
	   (remove (or (car (cdr (memq 'remove options)))
		       (let ((ral (copy-alist al)))
			 (mapcar (function
				  (lambda (type)
				    (setq ral (del-alist type ral))
				    ))
				 ignore-fields)
			 ral)))
	   )
      (set sym
	   (or (if replacement
		   (replace-atype (eval sym) remove al)
		 )
	       (cons al
		     (delete-atype (eval sym) remove)
		     )
	       )))))


;;; @ rot13-47
;;;
;; caesar-region written by phr@prep.ai.mit.edu  Nov 86
;; modified by tower@prep Nov 86
;; gnus-caesar-region
;; Modified by umerin@flab.flab.Fujitsu.JUNET for ROT47.
(defun tm:caesar-region (&optional n)
  "Caesar rotation of region by N, default 13, for decrypting netnews.
ROT47 will be performed for Japanese text in any case."
  (interactive (if current-prefix-arg	; Was there a prefix arg?
		   (list (prefix-numeric-value current-prefix-arg))
		 (list nil)))
  (cond ((not (numberp n)) (setq n 13))
	(t (setq n (mod n 26))))	;canonicalize N
  (if (not (zerop n))		; no action needed for a rot of 0
      (progn
	(if (or (not (boundp 'caesar-translate-table))
		(/= (aref caesar-translate-table ?a) (+ ?a n)))
	    (let ((i 0) (lower "abcdefghijklmnopqrstuvwxyz") upper)
	      (message "Building caesar-translate-table...")
	      (setq caesar-translate-table (make-vector 256 0))
	      (while (< i 256)
		(aset caesar-translate-table i i)
		(setq i (1+ i)))
	      (setq lower (concat lower lower) upper (upcase lower) i 0)
	      (while (< i 26)
		(aset caesar-translate-table (+ ?a i) (aref lower (+ i n)))
		(aset caesar-translate-table (+ ?A i) (aref upper (+ i n)))
		(setq i (1+ i)))
	      ;; ROT47 for Japanese text.
	      ;; Thanks to ichikawa@flab.fujitsu.junet.
	      (setq i 161)
	      (let ((t1 (logior ?O 128))
		    (t2 (logior ?! 128))
		    (t3 (logior ?~ 128)))
		(while (< i 256)
		  (aset caesar-translate-table i
			(let ((v (aref caesar-translate-table i)))
			  (if (<= v t1) (if (< v t2) v (+ v 47))
			    (if (<= v t3) (- v 47) v))))
		  (setq i (1+ i))))
	      (message "Building caesar-translate-table...done")))
	(let ((from (region-beginning))
	      (to (region-end))
	      (i 0) str len)
	  (setq str (buffer-substring from to))
	  (setq len (length str))
	  (while (< i len)
	    (aset str i (aref caesar-translate-table (aref str i)))
	    (setq i (1+ i)))
	  (goto-char from)
	  (delete-region from to)
	  (insert str)))))


;;; @ field
;;;

(defsubst regexp-or (&rest args)
  (concat "\\(" (mapconcat (function identity) args "\\|") "\\)"))

(defun tm:set-fields (sym field-list &optional regexp-sym)
  (or regexp-sym
      (setq regexp-sym
	    (let ((name (symbol-name sym)))
	      (intern
	       (concat (if (string-match "\\(.*\\)-list" name)
			   (substring name 0 (match-end 1))
			 name)
		       "-regexp")
	       )))
      )
  (set sym field-list)
  (set regexp-sym
       (concat "^" (apply (function regexp-or) field-list) ":"))
  )

(defun tm:add-fields (sym field-list &optional regexp-sym)
  (or regexp-sym
      (setq regexp-sym
	    (let ((name (symbol-name sym)))
	      (intern
	       (concat (if (string-match "\\(.*\\)-list" name)
			   (substring name 0 (match-end 1))
			 name)
		       "-regexp")
	       )))
      )
  (let ((fields (eval sym)))
    (mapcar (function
	     (lambda (field)
	       (or (member field fields)
		   (setq fields (cons field fields))
		   )
	       ))
	    (reverse field-list)
	    )
    (set regexp-sym
	 (concat "^" (apply (function regexp-or) fields) ":"))
    (set sym fields)
    ))

(defun tm:delete-fields (sym field-list &optional regexp-sym)
  (or regexp-sym
      (setq regexp-sym
	    (let ((name (symbol-name sym)))
	      (intern
	       (concat (if (string-match "\\(.*\\)-list" name)
			   (substring name 0 (match-end 1))
			 name)
		       "-regexp")
	       )))
      )
  (let ((fields (eval sym)))
    (mapcar (function
	     (lambda (field)
	       (setq fields (delete field fields))
	       ))
	    field-list)
    (set regexp-sym
	 (concat "^" (apply (function regexp-or) fields) ":"))
    (set sym fields)
    ))


;;; @ RCS version
;;;

(defsubst get-version-string (id)
  "Return a version-string from RCS ID."
  (and (string-match ",v \\([0-9][0-9.][0-9.]+\\)" id)
       (substring id (match-beginning 1)(match-end 1))
       ))


;;; @ Other Utility
;;;

(defun call-after-loaded (module func &optional hook-name)
  "If MODULE is provided, then FUNC is called.
Otherwise func is set to MODULE-load-hook.
If optional argument HOOK-NAME is specified,
it is used as hook to set."
  (if (featurep module)
      (funcall func)
    (or hook-name
	(setq hook-name (intern (concat (symbol-name module) "-load-hook")))
	)
    (add-hook hook-name func)
    ))


;;; @ end
;;;

(provide 'mime-def)

;;; mime-def.el ends here
