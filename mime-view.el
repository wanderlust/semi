;;; mime-view.el --- interactive MIME viewer for GNU Emacs

;; Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1994/7/13
;;	Renamed: 1994/8/31 from tm-body.el
;;	Renamed: 1997/02/19 from tm-view.el
;; Version: $Revision$
;; Keywords: MIME, multimedia, mail, news

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

(require 'cl)
(require 'std11)
(require 'mel)
(require 'eword-decode)
(require 'mime-parse)
(require 'mime-text)


;;; @ version
;;;

(defconst mime-view-RCS-ID
  "$Id$")

(defconst mime-view-version (get-version-string mime-view-RCS-ID))


;;; @ variables
;;;

(defvar mime/content-decoding-condition
  '(((type . "text/plain")
     (method "tm-plain" nil 'file 'type 'encoding 'mode 'name)
     (mode "play" "print")
     )
    ((type . "text/html")
     (method "tm-html" nil 'file 'type 'encoding 'mode 'name)
     (mode . "play")
     )
    ((type . "text/x-rot13-47")
     (method . mime-article/decode-caesar)
     (mode . "play")
     )
    ((type . "audio/basic")
     (method "tm-au"    nil 'file 'type 'encoding 'mode 'name)
     (mode . "play")
     )
    
    ((type . "image/jpeg")
     (method "tm-image" nil 'file 'type 'encoding 'mode 'name)
     (mode "play" "print")
     )
    ((type . "image/gif")
     (method "tm-image" nil 'file 'type 'encoding 'mode 'name)
     (mode "play" "print")
     )
    ((type . "image/tiff")
     (method "tm-image" nil 'file 'type 'encoding 'mode 'name)
     (mode "play" "print")
     )
    ((type . "image/x-tiff")
     (method "tm-image" nil 'file 'type 'encoding 'mode 'name)
     (mode "play" "print")
     )
    ((type . "image/x-xbm")
     (method "tm-image" nil 'file 'type 'encoding 'mode 'name)
     (mode "play" "print")
     )
    ((type . "image/x-pic")
     (method "tm-image" nil 'file 'type 'encoding 'mode 'name)
     (mode "play" "print")
     )
    ((type . "image/x-mag")
     (method "tm-image" nil 'file 'type 'encoding 'mode 'name)
     (mode "play" "print")
     )
    
    ((type . "video/mpeg")
     (method "tm-mpeg"  nil 'file 'type 'encoding 'mode 'name)
     (mode . "play")
     )
    
    ((type . "application/postscript")
     (method "tm-ps" nil 'file 'type 'encoding 'mode 'name)
     (mode "play" "print")
     )
    ((type . "application/octet-stream")
     (method "tm-file"  nil 'file 'type 'encoding 'mode 'name)
     (mode "play" "print")
     )
    
    ;;((type . "message/external-body")
    ;; (method "xterm" nil
    ;;	       "-e" "showexternal"
    ;;         'file '"access-type" '"name" '"site" '"directory"))
    ((type . "message/external-body")
     ("access-type" . "anon-ftp")
     (method . mime-article/decode-message/external-ftp)
     )
    ((type . "message/rfc822")
     (method . mime-article/view-message/rfc822)
     (mode . "play")
     )
    ((type . "message/partial")
     (method . mime-article/decode-message/partial)
     (mode . "play")
     )
    
    ((method "metamail" t "-m" "tm" "-x" "-d" "-z" "-e" 'file)
     (mode . "play")
     )
    ((method "tm-file"  nil 'file 'type 'encoding 'mode 'name)
     (mode . "extract")
     )
    ))

(defvar mime-view-childrens-header-showing-Content-Type-list
  '("message/rfc822" "message/news"))

(defvar mime-view-visible-media-type-list
  '("text/plain" nil "text/richtext" "text/enriched"
    "text/rfc822-headers"
    "text/x-latex" "application/x-latex"
    "message/delivery-status"
    "application/pgp" "text/x-pgp"
    "application/octet-stream"
    "application/x-selection" "application/x-comment")
  "*List of media-types to be able to display in MIME-View buffer.
Each elements are string of TYPE/SUBTYPE, e.g. \"text/plain\".")

(defvar mime-view-content-button-visible-ctype-list
  '("application/pgp"))

(defvar mime-view-uuencode-encoding-name-list '("x-uue" "x-uuencode"))

(defvar mime-view-ignored-field-list
  '(".*Received" ".*Path" ".*Id" "References"
    "Replied" "Errors-To"
    "Lines" "Sender" ".*Host" "Xref"
    "Content-Type" "Precedence"
    "Status" "X-VM-.*")
  "All fields that match this list will be hidden in MIME preview buffer.
Each elements are regexp of field-name. [mime-view.el]")

(defvar mime-view-ignored-field-regexp
  (concat "^"
	  (apply (function regexp-or) mime-view-ignored-field-list)
	  ":"))

(defvar mime-view-visible-field-list '("Dnas.*" "Message-Id")
  "All fields that match this list will be displayed in MIME preview buffer.
Each elements are regexp of field-name.")

(defvar mime-view-redisplay nil)

(defvar mime-view-announcement-for-message/partial
  (if (and (>= emacs-major-version 19) window-system)
      "\
\[[ This is message/partial style split message. ]]
\[[ Please press `v' key in this buffer          ]]
\[[ or click here by mouse button-2.             ]]"
    "\
\[[ This is message/partial style split message. ]]
\[[ Please press `v' key in this buffer.         ]]"
    ))


;;; @@ predicate functions
;;;

(defun mime-view-header-visible-p (rcnum cinfo)
  "Return non-nil if header of current entity is visible."
  (or (null rcnum)
      (member (mime::content-info/type
	       (mime-article/rcnum-to-cinfo (cdr rcnum) cinfo))
	      mime-view-childrens-header-showing-Content-Type-list)
      ))

(defun mime-view-body-visible-p (rcnum cinfo &optional ctype)
  (let (ccinfo)
    (or ctype
	(setq ctype
	      (mime::content-info/type
	       (setq ccinfo (mime-article/rcnum-to-cinfo rcnum cinfo))
	       ))
	)
    (and (member ctype mime-view-visible-media-type-list)
	 (if (string-equal ctype "application/octet-stream")
	     (progn
	       (or ccinfo
		   (setq ccinfo (mime-article/rcnum-to-cinfo rcnum cinfo))
		   )
	       (member (mime::content-info/encoding ccinfo)
		       '(nil "7bit" "8bit"))
	       )
	   t))
    ))


;;; @@ entity button
;;;

(defun mime-view-insert-entity-button (rcnum cinfo ctype params subj encoding)
  "Insert entity-button."
  (save-restriction
    (narrow-to-region (point)(point))
    (let ((access-type (assoc "access-type" params))
	  (num (or (cdr (assoc "x-part-number" params))
		   (if (consp rcnum)
		       (mapconcat (function
				   (lambda (num)
				     (format "%s" (1+ num))
				     ))
				  (reverse rcnum) ".")
		     "0"))
	       ))
      (cond (access-type
	     (let ((server (assoc "server" params)))
	       (setq access-type (cdr access-type))
	       (if server
		   (insert (format "[%s %s ([%s] %s)]\n" num subj
				   access-type (cdr server)))
		 (let ((site (cdr (assoc "site" params)))
		       (dir (cdr (assoc "directory" params)))
		       )
		   (insert (format "[%s %s ([%s] %s:%s)]\n" num subj
				   access-type site dir))
		   )))
	     )
	    (t
	     (let ((charset (cdr (assoc "charset" params))))
	       (insert (concat "[" num " " subj))
	       (let ((rest
		      (concat " <" ctype
			      (if charset
				  (concat "; " charset)
				(if encoding (concat " (" encoding ")"))
				)
			      ">]\n")))
		 (if (>= (+ (current-column)(length rest))(window-width))
		     (insert "\n\t")
		   )
		 (insert rest)
		 ))))
      )
    (mime-add-button (point-min)(1- (point-max))
		     (function mime-view-play-current-entity))
    ))

(defun mime-view-entity-button-function
  (rcnum cinfo ctype params subj encoding)
  "Insert entity button conditionally.
Please redefine this function if you want to change default setting."
  (or (null rcnum)
      (string= ctype "application/x-selection")
      (and (string= ctype "application/octet-stream")
	   (string= (mime::content-info/type
		     (mime-article/rcnum-to-cinfo (cdr rcnum) cinfo))
		    "multipart/encrypted"))
      (mime-view-insert-entity-button rcnum cinfo ctype params subj encoding)
      ))


;;; @@ content header filter
;;;

(defsubst mime-view-cut-header ()
  (goto-char (point-min))
  (while (re-search-forward mime-view-ignored-field-regexp nil t)
    (let* ((beg (match-beginning 0))
	   (end (match-end 0))
	   (name (buffer-substring beg end))
	   )
      (or (member-if (function
		      (lambda (regexp)
			(string-match regexp name)
			)) mime-view-visible-field-list)
	  (delete-region beg
			 (if (re-search-forward "^\\([^ \t]\\|$\\)" nil t)
			     (match-beginning 0)
			   (point-max)))
	  ))))

(defun mime-view-default-content-header-filter ()
  (mime-view-cut-header)
  (eword-decode-header)
  )

(defvar mime-view-content-header-filter-alist nil)


;;; @@ content filter
;;;

(defvar mime-view-content-filter-alist
  '(("text/enriched" . mime-preview/filter-for-text/enriched)
    ("text/richtext" . mime-preview/filter-for-text/richtext)
    (t . mime-preview/filter-for-text/plain)
    ))


;;; @@ entity separator
;;;

(defun mime-view-entity-separator-function (rcnum cinfo ctype params subj)
  "Insert entity separator conditionally.
Please redefine this function if you want to change default setting."
  (or (mime-view-header-visible-p rcnum cinfo)
      (mime-view-body-visible-p rcnum cinfo ctype)
      (progn
	(goto-char (point-max))
	(insert "\n")
	)))


;;; @@ buffer local variables
;;;

;;; @@@ in raw buffer
;;;

(make-variable-buffer-local 'mime::article/content-info)

(defvar mime::article/preview-buffer nil)
(make-variable-buffer-local 'mime::article/preview-buffer)


;;; @@@ in view buffer
;;;

(make-variable-buffer-local 'mime::preview/mother-buffer)
(make-variable-buffer-local 'mime::preview/content-list)

(defvar mime::preview/article-buffer nil)
(make-variable-buffer-local 'mime::preview/article-buffer)

(make-variable-buffer-local 'mime::preview/original-major-mode)
(make-variable-buffer-local 'mime::preview/original-window-configuration)


;;; @@ quitting method
;;;

(defvar mime-view-quitting-method-alist
  '((mime-show-message-mode
     . mime-view-quitting-method-for-mime-show-message-mode))
  "Alist of major-mode vs. quitting-method of mime-view.")

(defvar mime-view-over-to-previous-method-alist nil)
(defvar mime-view-over-to-next-method-alist nil)

(defvar mime-view-show-summary-method nil
  "Alist of major-mode vs. show-summary-method.")


;;; @@ following method
;;;

(defvar mime-view-following-method-alist nil)

(defvar mime-view-following-required-fields-list
  '("From"))


;;; @@ X-Face
;;;

;; hack from Gnus 5.0.4.

(defvar mime-view-x-face-to-pbm-command
  "{ echo '/* Width=48, Height=48 */'; uncompface; } | icontopbm")

(defvar mime-view-x-face-command
  (concat mime-view-x-face-to-pbm-command
	  " | xv -quit -")
  "String to be executed to display an X-Face field.
The command will be executed in a sub-shell asynchronously.
The compressed face will be piped to this command.")

(defun mime-view-x-face-function ()
  "Function to display X-Face field. You can redefine to customize."
  ;; 1995/10/12 (c.f. tm-eng:130)
  ;;	fixed by Eric Ding <ericding@San-Jose.ate.slb.com>
  (save-restriction
    (narrow-to-region (point-min) (re-search-forward "^$" nil t))
    ;; end
    (goto-char (point-min))
    (if (re-search-forward "^X-Face:[ \t]*" nil t)
	(let ((beg (match-end 0))
	      (end (std11-field-end))
	      )
	  (call-process-region beg end "sh" nil 0 nil
			       "-c" mime-view-x-face-command)
	  ))))


;;; @@ utility
;;;

(defun mime-preview/get-original-major-mode ()
  (if mime::preview/mother-buffer
      (save-excursion
	(set-buffer mime::preview/mother-buffer)
	(mime-preview/get-original-major-mode)
	)
    mime::preview/original-major-mode))


;;; @ data structures
;;;

;;; @@ preview-content-info
;;;

(define-structure mime::preview-content-info
  point-min point-max buffer content-info)


;;; @ buffer setup
;;;

(defun mime-view-setup-buffer (&optional ctl encoding ibuf obuf)
  (if ibuf
      (progn
	(get-buffer ibuf)
	(set-buffer ibuf)
	))
  (or mime-view-redisplay
      (setq mime::article/content-info (mime-parse-message ctl encoding))
      )
  (let ((ret (mime-view-make-preview-buffer obuf)))
    (setq mime::article/preview-buffer (car ret))
    ret))

(defun mime-view-make-preview-buffer (&optional obuf)
  (let* ((cinfo mime::article/content-info)
	 (pcl (mime/flatten-content-info cinfo))
	 (dest (make-list (length pcl) nil))
	 (the-buf (current-buffer))
	 (mode major-mode)
	 )
    (or obuf
	(setq obuf (concat "*Preview-" (buffer-name the-buf) "*")))
    (set-buffer (get-buffer-create obuf))
    (setq buffer-read-only nil)
    (widen)
    (erase-buffer)
    (setq mime::preview/article-buffer the-buf)
    (setq mime::preview/original-major-mode mode)
    (setq major-mode 'mime-view-mode)
    (setq mode-name "MIME-View")
    (let ((drest dest))
      (while pcl
	(setcar drest
		(mime-view-display-entity (car pcl) cinfo the-buf obuf))
	(setq pcl (cdr pcl)
	      drest (cdr drest))
	))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (set-buffer the-buf)
    (list obuf dest)
    ))

(defun mime-view-display-entity (content cinfo ibuf obuf)
  "Display entity from content-info CONTENT."
  (let* ((beg (mime::content-info/point-min content))
	 (end (mime::content-info/point-max content))
	 (ctype (mime::content-info/type content))
	 (params (mime::content-info/parameters content))
	 (encoding (mime::content-info/encoding content))
	 (rcnum (mime::content-info/rcnum content))
	 he e nb ne subj)
    (set-buffer ibuf)
    (goto-char beg)
    (setq he (if (re-search-forward "^$" nil t)
		 (1+ (match-end 0))
	       end))
    (if (> he end)
	(setq he end)
      )
    (save-restriction
      (narrow-to-region beg end)
      (setq subj
	    (eword-decode-string
	     (mime-article/get-subject params encoding)))
      )
    (set-buffer obuf)
    (setq nb (point))
    (narrow-to-region nb nb)
    (mime-view-entity-button-function rcnum cinfo ctype params subj encoding)
    (if (mime-view-header-visible-p rcnum cinfo)
	(mime-preview/display-header beg he)
      )
    (if (and (null rcnum)
	     (member
	      ctype mime-view-content-button-visible-ctype-list))
	(save-excursion
	  (goto-char (point-max))
	  (mime-view-insert-entity-button
	   rcnum cinfo ctype params subj encoding)
	  ))
    (cond ((mime-view-body-visible-p rcnum cinfo ctype)
	   (mime-preview/display-body he end
				      rcnum cinfo ctype params subj encoding)
	   )
	  ((equal ctype "message/partial")
	   (mime-preview/display-message/partial)
	   )
	  ((and (null rcnum)
		(null (mime::content-info/children cinfo))
		)
	   (goto-char (point-max))
	   (mime-view-insert-entity-button
	    rcnum cinfo ctype params subj encoding)
	   ))
    (mime-view-entity-separator-function rcnum cinfo ctype params subj)
    (prog1
	(progn
	  (setq ne (point-max))
	  (widen)
	  (put-text-property nb ne 'mime-view-raw-buffer ibuf)
	  (put-text-property nb ne 'mime-view-cinfo content)
	  (mime::preview-content-info/create nb (1- ne) ibuf content)
	  )
      (goto-char ne)
      )))

(defun mime-preview/display-header (beg end)
  (save-restriction
    (narrow-to-region (point)(point))
    (insert-buffer-substring mime::preview/article-buffer beg end)
    (let ((f (cdr (assq mime::preview/original-major-mode
			mime-view-content-header-filter-alist))))
      (if (functionp f)
	  (funcall f)
	(mime-view-default-content-header-filter)
	))
    (run-hooks 'mime-view-content-header-filter-hook)
    ))

(defun mime-preview/display-body (beg end
				      rcnum cinfo ctype params subj encoding)
  (save-restriction
    (narrow-to-region (point-max)(point-max))
    (insert-buffer-substring mime::preview/article-buffer beg end)
    (let ((f (cdr (or (assoc ctype mime-view-content-filter-alist)
		      (assq t mime-view-content-filter-alist)))))
      (and (functionp f)
	   (funcall f ctype params encoding)
	   )
      )))

(defun mime-preview/display-message/partial ()
  (save-restriction
    (goto-char (point-max))
    (if (not (search-backward "\n\n" nil t))
	(insert "\n")
      )
    (let ((be (point-max)))
      (narrow-to-region be be)
      (insert mime-view-announcement-for-message/partial)
      (mime-add-button (point-min)(point-max)
		       (function mime-view-play-current-entity))
      )))

(defun mime-article/get-uu-filename (param &optional encoding)
  (if (member (or encoding
		  (cdr (assq 'encoding param))
		  )
	      mime-view-uuencode-encoding-name-list)
      (save-excursion
	(or (if (re-search-forward "^begin [0-9]+ " nil t)
		(if (looking-at ".+$")
		    (buffer-substring (match-beginning 0)(match-end 0))
		  ))
	    ""))
    ))

(defun mime-article/get-subject (param &optional encoding)
  (or (std11-find-field-body '("Content-Description" "Subject"))
      (let (ret)
	(if (or (and (setq ret (mime/Content-Disposition))
		     (setq ret (assoc "filename" (cdr ret)))
		     )
		(setq ret (assoc "name" param))
		(setq ret (assoc "x-name" param))
		)
	    (std11-strip-quoted-string (cdr ret))
	  ))
      (mime-article/get-uu-filename param encoding)
      ""))


;;; @ content information
;;;

(defun mime-article/point-content-number (p &optional cinfo)
  (or cinfo
      (setq cinfo mime::article/content-info)
      )
  (let ((b (mime::content-info/point-min cinfo))
	(e (mime::content-info/point-max cinfo))
	(c (mime::content-info/children cinfo))
	)
    (if (and (<= b p)(<= p e))
	(or (let (co ret (sn 0))
	      (catch 'tag
		(while c
		  (setq co (car c))
		  (setq ret (mime-article/point-content-number p co))
		  (cond ((eq ret t) (throw 'tag (list sn)))
			(ret (throw 'tag (cons sn ret)))
			)
		  (setq c (cdr c))
		  (setq sn (1+ sn))
		  )))
	    t))))

(defun mime-article/rcnum-to-cinfo (rcnum &optional cinfo)
  (or cinfo
      (setq cinfo mime::article/content-info)
      )
  (find-if (function
	    (lambda (ci)
	      (equal (mime::content-info/rcnum ci) rcnum)
	      ))
	   (mime/flatten-content-info cinfo)
	   ))

(defun mime-article/cnum-to-cinfo (cn &optional cinfo)
  (or cinfo
      (setq cinfo mime::article/content-info)
      )
  (if (eq cn t)
      cinfo
    (let ((sn (car cn)))
      (if (null sn)
	  cinfo
	(let ((rc (nth sn (mime::content-info/children cinfo))))
	  (if rc
	      (mime-article/cnum-to-cinfo (cdr cn) rc)
	    ))
	))))

(defun mime/flatten-content-info (&optional cinfo)
  (or cinfo
      (setq cinfo mime::article/content-info)
      )
  (let ((dest (list cinfo))
	(rcl (mime::content-info/children cinfo))
	)
    (while rcl
      (setq dest (nconc dest (mime/flatten-content-info (car rcl))))
      (setq rcl (cdr rcl))
      )
    dest))


;;; @ MIME viewer mode
;;;

(defconst mime-view-menu-title "MIME-View")
(defconst mime-view-menu-list
  '((up		 "Move to upper content"      mime-view-move-to-upper)
    (previous	 "Move to previous content"   mime-view-move-to-previous)
    (next	 "Move to next content"	      mime-view-move-to-next)
    (scroll-down "Scroll to previous content" mime-view-scroll-down-entity)
    (scroll-up	 "Scroll to next content"     mime-view-scroll-up-entity)
    (play	 "Play Content"               mime-view-play-current-entity)
    (extract	 "Extract Content"            mime-view-extract-current-entity)
    (print	 "Print"                      mime-view-print-current-entity)
    (x-face	 "Show X Face"                mime-view-display-x-face)
    )
  "Menu for MIME Viewer")

(cond (running-xemacs
       (defvar mime-view-xemacs-popup-menu
	 (cons mime-view-menu-title
	       (mapcar (function
			(lambda (item)
			  (vector (nth 1 item)(nth 2 item) t)
			  ))
		       mime-view-menu-list)))
       (defun mime-view-xemacs-popup-menu (event)
	 "Popup the menu in the MIME Viewer buffer"
	 (interactive "e")
	 (select-window (event-window event))
	 (set-buffer (event-buffer event))
	 (popup-menu 'mime-view-xemacs-popup-menu))
       (defvar mouse-button-2 'button2)
       )
      (t
       (defvar mouse-button-2 [mouse-2])
       ))

(defun mime-view-define-keymap (&optional default)
  (let ((mime-view-mode-map (if (keymapp default)
				(copy-keymap default)
			      (make-sparse-keymap)
			      )))
    (define-key mime-view-mode-map
      "u"        (function mime-view-move-to-upper))
    (define-key mime-view-mode-map
      "p"        (function mime-view-move-to-previous))
    (define-key mime-view-mode-map
      "n"        (function mime-view-move-to-next))
    (define-key mime-view-mode-map
      "\e\t"     (function mime-view-move-to-previous))
    (define-key mime-view-mode-map
      "\t"       (function mime-view-move-to-next))
    (define-key mime-view-mode-map
      " "        (function mime-view-scroll-up-entity))
    (define-key mime-view-mode-map
      "\M- "     (function mime-view-scroll-down-entity))
    (define-key mime-view-mode-map
      "\177"     (function mime-view-scroll-down-entity))
    (define-key mime-view-mode-map
      "\C-m"     (function mime-view-next-line-content))
    (define-key mime-view-mode-map
      "\C-\M-m"  (function mime-view-previous-line-content))
    (define-key mime-view-mode-map
      "v"        (function mime-view-play-current-entity))
    (define-key mime-view-mode-map
      "e"        (function mime-view-extract-current-entity))
    (define-key mime-view-mode-map
      "\C-c\C-p" (function mime-view-print-current-entity))
    (define-key mime-view-mode-map
      "a"        (function mime-view-follow-content))
    (define-key mime-view-mode-map
      "q"        (function mime-view-quit))
    (define-key mime-view-mode-map
      "h"        (function mime-view-show-summary))
    (define-key mime-view-mode-map
      "\C-c\C-x" (function mime-view-kill-buffer))
    (define-key mime-view-mode-map
      "<"        (function beginning-of-buffer))
    (define-key mime-view-mode-map
      ">"        (function end-of-buffer))
    (define-key mime-view-mode-map
      "?"        (function describe-mode))
    (if (functionp default)
	(setq mime-view-mode-map
	      (append mime-view-mode-map (list (cons t default)))
	      ))
    (if mouse-button-2
	(define-key mime-view-mode-map
	  mouse-button-2 (function mime-button-dispatcher))
      )
    (cond (running-xemacs
	   (define-key mime-view-mode-map
	     mouse-button-3 (function mime-view-xemacs-popup-menu))
	   )
	  ((>= emacs-major-version 19)
	   (define-key mime-view-mode-map [menu-bar mime-view]
	     (cons mime-view-menu-title
		   (make-sparse-keymap mime-view-menu-title)))
	   (mapcar (function
		    (lambda (item)
		      (define-key mime-view-mode-map
			(vector 'menu-bar 'mime-view (car item))
			(cons (nth 1 item)(nth 2 item))
			)
		      ))
		   (reverse mime-view-menu-list)
		   )
	   ))
    (use-local-map mime-view-mode-map)
    (run-hooks 'mime-view-define-keymap-hook)
    ))

(defun mime-view-mode (&optional mother ctl encoding ibuf obuf
				 default-keymap-or-function)
  "Major mode for viewing MIME message.

Here is a list of the standard keys for mime-view-mode.

key		feature
---		-------

u		Move to upper content
p or M-TAB	Move to previous content
n or TAB	Move to next content
SPC		Scroll up or move to next content
M-SPC or DEL	Scroll down or move to previous content
RET		Move to next line
M-RET		Move to previous line
v		Decode current content as `play mode'
e		Decode current content as `extract mode'
C-c C-p		Decode current content as `print mode'
a		Followup to current content.
x		Display X-Face
q		Quit
button-2	Move to point under the mouse cursor
        	and decode current content as `play mode'
"
  (interactive)
  (let ((buf (get-buffer mime/output-buffer-name)))
    (if buf
	(save-excursion
	  (set-buffer buf)
	  (erase-buffer)
	  )))
  (let ((ret (mime-view-setup-buffer ctl encoding ibuf obuf))
	(win-conf (current-window-configuration))
	)
    (prog1
	(switch-to-buffer (car ret))
      (setq mime::preview/original-window-configuration win-conf)
      (if mother
	  (progn
	    (setq mime::preview/mother-buffer mother)
	    ))
      (mime-view-define-keymap default-keymap-or-function)
      (setq mime::preview/content-list (nth 1 ret))
      (goto-char
       (let ((ce (mime::preview-content-info/point-max
		  (car mime::preview/content-list)
		  ))
	     e)
	 (goto-char (point-min))
	 (search-forward "\n\n" nil t)
	 (setq e (match-end 0))
	 (if (<= e ce)
	     e
	   ce)))
      (run-hooks 'mime-view-mode-hook)
      )))

(defun mime-preview/cinfo-to-pcinfo (cinfo)
  (let ((rpcl mime::preview/content-list) cell)
    (catch 'tag
      (while rpcl
	(setq cell (car rpcl))
	(if (eq cinfo (mime::preview-content-info/content-info cell))
	    (throw 'tag cell)
	  )
	(setq rpcl (cdr rpcl))
	))))

(autoload 'mime-view-play-current-entity "mime-play" "Play current entity." t)

(defun mime-view-extract-current-entity ()
  "Extract current entity into file (maybe).
It decodes current entity to call internal or external method as
\"extract\" mode.  The method is selected from variable
`mime/content-decoding-condition'."
  (interactive)
  (mime-view-play-current-entity "extract")
  )

(defun mime-view-print-current-entity ()
  "Print current entity (maybe).
It decodes current entity to call internal or external method as
\"print\" mode.  The method is selected from variable
`mime/content-decoding-condition'."
  (interactive)
  (mime-view-play-current-entity "print")
  )

(defun mime-view-follow-content ()
  (interactive)
  (let ((root-cinfo
	 (mime::preview-content-info/content-info
	  (car mime::preview/content-list)))
	pc p-beg p-end cinfo rcnum)
    (let ((rest mime::preview/content-list)
	  b e cell len rc)
      (if (catch 'tag
	    (while (setq cell (car rest))
	      (setq b (mime::preview-content-info/point-min cell)
		    e (mime::preview-content-info/point-max cell))
	      (setq rest (cdr rest))
	      (if (and (<= b (point))(<= (point) e))
		  (throw 'tag cell)
		)
	      ))
	  (progn
	    (setq pc cell
		  cinfo (mime::preview-content-info/content-info pc)
		  rcnum (mime::content-info/rcnum cinfo))
	    (setq len (length rcnum))
	    (setq p-beg (mime::preview-content-info/point-min pc)
		  p-end (mime::preview-content-info/point-max pc))
	    (while (and (setq cell (car rest))
			(progn
			  (setq rc
				(mime::content-info/rcnum
				 (mime::preview-content-info/content-info
				  cell)))
			  (equal rcnum
				 (nthcdr (- (length rc) len) rc))
			  ))
	      (setq p-end (mime::preview-content-info/point-max cell))
	      (setq rest (cdr rest))
	      ))))
    (if pc
	(let* ((mode (mime-preview/get-original-major-mode))
	       (new-name (format "%s-%s" (buffer-name) (reverse rcnum)))
	       new-buf
	       (the-buf (current-buffer))
	       (a-buf mime::preview/article-buffer)
	       fields)
	  (save-excursion
	    (set-buffer (setq new-buf (get-buffer-create new-name)))
	    (erase-buffer)
	    (insert-buffer-substring the-buf p-beg p-end)
	    (goto-char (point-min))
	    (if (mime-view-header-visible-p rcnum root-cinfo)
		(delete-region (goto-char (point-min))
			       (if (re-search-forward "^$" nil t)
				   (match-end 0)
				 (point-min)))
	      )
	    (goto-char (point-min))
	    (insert "\n")
	    (goto-char (point-min))
	    (let ((rcnum (mime::content-info/rcnum cinfo)) ci str)
	      (while (progn
		       (setq str
			     (save-excursion
			       (set-buffer a-buf)
			       (setq ci (mime-article/rcnum-to-cinfo rcnum))
			       (save-restriction
				 (narrow-to-region
				  (mime::content-info/point-min ci)
				  (mime::content-info/point-max ci)
				  )
				 (std11-header-string-except
				  (concat "^"
					  (apply (function regexp-or) fields)
					  ":") ""))))
		       (if (string= (mime::content-info/type ci)
				    "message/rfc822")
			   nil
			 (if str
			     (insert str)
			   )
			 rcnum))
		(setq fields (std11-collect-field-names)
		      rcnum (cdr rcnum))
		)
	      )
	    (let ((rest mime-view-following-required-fields-list))
	      (while rest
		(let ((field-name (car rest)))
		  (or (std11-field-body field-name)
		      (insert
		       (format
			(concat field-name
				": "
				(save-excursion
				  (set-buffer the-buf)
				  (set-buffer mime::preview/mother-buffer)
				  (set-buffer mime::preview/article-buffer)
				  (std11-field-body field-name)
				  )
				"\n")))
		      ))
		(setq rest (cdr rest))
		))
	    (eword-decode-header)
	    )
	  (let ((f (cdr (assq mode mime-view-following-method-alist))))
	    (if (functionp f)
		(funcall f new-buf)
	      (message
	       (format
		"Sorry, following method for %s is not implemented yet."
		mode))
	      ))
	  ))))

(defun mime-view-display-x-face ()
  (interactive)
  (save-window-excursion
    (set-buffer mime::preview/article-buffer)
    (mime-view-x-face-function)
    ))

(defun mime-view-move-to-upper ()
  "Move to upper entity.
If there is no upper entity, call function `mime-view-quit'."
  (interactive)
  (let (cinfo)
    (while (null (setq cinfo (get-text-property (point) 'mime-view-cinfo)))
      (backward-char)
      )
    (let ((r (mime-article/rcnum-to-cinfo
	      (cdr (mime::content-info/rcnum cinfo))
	      (get-text-property 1 'mime-view-cinfo)))
	  point)
      (catch 'tag
	(while (setq point (previous-single-property-change
			    (point) 'mime-view-cinfo))
	  (goto-char point)
	  (if (eq r (get-text-property (point) 'mime-view-cinfo))
	      (throw 'tag t)
	    )
	  )
	(mime-view-quit)
	))))

(defun mime-view-move-to-previous ()
  "Move to previous entity.
If there is no previous entity, it calls function registered in
variable `mime-view-over-to-previous-method-alist'."
  (interactive)
  (while (null (get-text-property (point) 'mime-view-cinfo))
    (backward-char)
    )
  (let ((point (previous-single-property-change (point) 'mime-view-cinfo)))
    (if point
	(goto-char point)
      (let ((f (assq mime::preview/original-major-mode
		     mime-view-over-to-previous-method-alist)))
	(if f
	    (funcall (cdr f))
	  ))
      )))

(defun mime-view-move-to-next ()
  "Move to next entity.
If there is no previous entity, it calls function registered in
variable `mime-view-over-to-next-method-alist'."
  (interactive)
  (let ((point (next-single-property-change (point) 'mime-view-cinfo)))
    (if point
	(goto-char point)
      (let ((f (assq mime::preview/original-major-mode
		     mime-view-over-to-next-method-alist)))
	(if f
	    (funcall (cdr f))
	  ))
      )))

(defun mime-view-scroll-up-entity (&optional h)
  "Scroll up current entity.
If reached to (point-max), it calls function registered in variable
`mime-view-over-to-next-method-alist'."
  (interactive)
  (or h
      (setq h (1- (window-height)))
      )
  (if (= (point) (point-max))
      (let ((f (assq mime::preview/original-major-mode
                     mime-view-over-to-next-method-alist)))
        (if f
            (funcall (cdr f))
          ))
    (let ((point
	   (or (next-single-property-change (point) 'mime-view-cinfo)
	       (point-max))))
      (forward-line h)
      (if (> (point) point)
          (goto-char point)
        )
      )))

(defun mime-view-scroll-down-entity (&optional h)
  "Scroll down current entity.
If reached to (point-min), it calls function registered in variable
`mime-view-over-to-previous-method-alist'."
  (interactive)
  (or h
      (setq h (1- (window-height)))
      )
  (if (= (point) (point-min))
      (let ((f (assq mime::preview/original-major-mode
                     mime-view-over-to-previous-method-alist)))
        (if f
            (funcall (cdr f))
          ))
    (let (point)
      (save-excursion
	(catch 'tag
	  (while (> (point) 1)
	    (if (setq point
		      (previous-single-property-change (point)
						       'mime-view-cinfo))
		(throw 'tag t)
	      )
	    (backward-char)
	    )
	  (setq point (point-min))
	  ))
      (forward-line (- h))
      (if (< (point) point)
          (goto-char point)
        ))))

(defun mime-view-next-line-content ()
  (interactive)
  (mime-view-scroll-up-entity 1)
  )

(defun mime-view-previous-line-content ()
  (interactive)
  (mime-view-scroll-down-entity 1)
  )

(defun mime-view-quit ()
  "Quit from MIME-View buffer.
It calls function registered in variable
`mime-view-quitting-method-alist'."
  (interactive)
  (let ((r (assq mime::preview/original-major-mode
		 mime-view-quitting-method-alist)))
    (if r
	(funcall (cdr r))
      )))

(defun mime-view-show-summary ()
  "Show summary.
It calls function registered in variable
`mime-view-show-summary-method'."
  (interactive)
  (let ((r (assq mime::preview/original-major-mode
		 mime-view-show-summary-method)))
    (if r
	(funcall (cdr r))
      )))

(defun mime-view-kill-buffer ()
  (interactive)
  (kill-buffer (current-buffer))
  )


;;; @ end
;;;

(provide 'mime-view)

(run-hooks 'mime-view-load-hook)

;;; mime-view.el ends here
