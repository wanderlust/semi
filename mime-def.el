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

;;; @ for XEmacs
;;;

(defvar running-xemacs (string-match "XEmacs" emacs-version))

(if running-xemacs
    (require 'overlay)
  )


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


;;; @ button
;;;

(defvar mime-button-face 'bold
  "Face used for content-button or URL-button of MIME-Preview buffer.")

(defvar mime-button-mouse-face 'highlight
  "Face used for MIME-preview buffer mouse highlighting.")

(defun tm:add-button (from to func &optional data)
  "Create a button between FROM and TO with callback FUNC and data DATA."
  (and mime-button-face
       (overlay-put (make-overlay from to) 'face mime-button-face))
  (tl:add-text-properties from to
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
    (mime-sign		tm:mc-pgp-sign-region		"mime-edit-mc")
    (traditional-sign	mc-pgp-sign-region		"mc-pgp")
    (encrypt		tm:mc-pgp-encrypt-region	"mime-edit-mc")
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


;;; @ definitions about MIME
;;;

(defconst mime/tspecials "][\000-\040()<>@,\;:\\\"/?.=")
(defconst mime/token-regexp (concat "[^" mime/tspecials "]+"))
(defconst mime-charset-regexp mime/token-regexp)

(defconst mime/content-type-subtype-regexp
  (concat mime/token-regexp "/" mime/token-regexp))

(defconst mime/disposition-type-regexp mime/token-regexp)


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


;;; @ end
;;;

(provide 'mime-def)

;;; mime-def.el ends here
