;;; mime-view.el --- interactive MIME viewer for GNU Emacs

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1994/07/13
;;	Renamed: 1994/08/31 from tm-body.el
;;	Renamed: 1997/02/19 from tm-view.el
;; Keywords: MIME, multimedia, mail, news

;; This file is part of SEMI (Sample of Elastic MIME Interfaces).

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
(require 'mime)
(require 'semi-def)
(require 'calist)
(require 'alist)
(require 'mailcap)


;;; @ version
;;;

(defconst mime-view-version
  (concat (mime-product-name mime-user-interface-product) " MIME-View "
	  (mapconcat #'number-to-string
		     (mime-product-version mime-user-interface-product) ".")
	  " (" (mime-product-code-name mime-user-interface-product) ")"))


;;; @ variables
;;;

(defgroup mime-view nil
  "MIME view mode"
  :group 'mime)

(defcustom mime-view-find-every-acting-situation t
  "*Find every available acting-situation if non-nil."
  :group 'mime-view
  :type 'boolean)

(defcustom mime-acting-situation-examples-file "~/.mime-example"
  "*File name of example about acting-situation demonstrated by user."
  :group 'mime-view
  :type 'file)

(defcustom mime-preview-move-scroll nil
  "*Decides whether to scroll when moving to next entity.
When t, scroll the buffer. Non-nil but not t means scroll when
the next entity is within next-screen-context-lines from top or
buttom. Nil means don't scroll at all."
  :group 'mime-view
  :type '(choice (const :tag "Off" nil)
		 (const :tag "On" t)
		 (sexp :tag "Situation" 1)))

(defcustom mime-preview-scroll-full-screen nil
  "*When non-nil, always scroll full screen.
If nil, point will be moved to the next entity if exists."
  :group 'mime-view
  :type '(choice (const :tag "On" t)
		 (const :tag "Off" nil)))

(defcustom mime-view-force-inline-types '(text multipart)
  "*List of MIME types that \"attachment\" should be ignored.
The element can be type or type/subtype. When t, inline everything
if possible."
  :group 'mime-view
  :type '(choice (const :tag "Nothing" nil)
		 (const :tag "All" t)
		 (list (repeat symbol))))

(defcustom mime-view-button-place-alist
  '((message . around)
    (application . before)
    (multipart/alternative . around))
  "*Alist of MIME type or type/subtype vs. button place.
When around, button will be inserted before and after that part.
When after or before, button will be inserted that place.
If not specified, that type will not have button."
  :group 'mime-view
  :type '(choice (const :tag "Nothing" nil)
		 (list (repeat symbol))))

;; Rename this.
(defcustom mime-view-type-subtype-score-alist
  '(((text . enriched) . 3)
    ((text . richtext) . 2)
    ((text . plain)    . 1)
    (t . 0))
  "Alist MEDIA-TYPE vs corresponding score.
MEDIA-TYPE must be (TYPE . SUBTYPE), TYPE or t.  t means default."
  :group 'mime-view
  :type '(repeat (cons (choice :tag "Media-Type"
			       (cons :tag "Type/Subtype"
				     (symbol :tag "Primary-type")
				     (symbol :tag "Subtype"))
			       (symbol :tag "Type")
			       (const :tag "Default" t))
		       integer)))

;;; @ in raw-buffer (representation space)
;;;

(defvar mime-preview-buffer nil
  "MIME-preview buffer corresponding with the (raw) buffer.")
(make-variable-buffer-local 'mime-preview-buffer)


(defvar mime-raw-representation-type-alist
  '((mime-show-message-mode     . binary)
    (mime-temp-message-mode     . binary)
    (t                          . cooked)
    )
  "Alist of major-mode vs. representation-type of mime-raw-buffer.
Each element looks like (SYMBOL . REPRESENTATION-TYPE).  SYMBOL is
major-mode or t.  t means default.  REPRESENTATION-TYPE must be
`binary' or `cooked'.")


;; (defun mime-raw-find-entity-from-point (point &optional message-info)
;;   "Return entity from POINT in mime-raw-buffer.
;; If optional argument MESSAGE-INFO is not specified,
;; `mime-message-structure' is used."
;;   (or message-info
;;       (setq message-info mime-message-structure))
;;   (if (and (<= (mime-entity-point-min message-info) point)
;;            (<= point (mime-entity-point-max message-info)))
;;       (let ((children (mime-entity-children message-info)))
;;         (catch 'tag
;;           (while children
;;             (let ((ret
;;                    (mime-raw-find-entity-from-point point (car children))))
;;               (if ret
;;                   (throw 'tag ret)
;;                 ))
;;             (setq children (cdr children)))
;;           message-info))))
;; (make-obsolete 'mime-raw-find-entity-from-point "don't use it.")


;;; @ in preview-buffer (presentation space)
;;;

(defvar mime-mother-buffer nil
  "Mother buffer corresponding with the (MIME-preview) buffer.
If current MIME-preview buffer is generated by other buffer, such as
message/partial, it is called `mother-buffer'.")
(make-variable-buffer-local 'mime-mother-buffer)

;; (defvar mime-raw-buffer nil
;;   "Raw buffer corresponding with the (MIME-preview) buffer.")
;; (make-variable-buffer-local 'mime-raw-buffer)

(defvar mime-preview-original-window-configuration nil
  "Window-configuration before mime-view-mode is called.")
(make-variable-buffer-local 'mime-preview-original-window-configuration)

(defun mime-preview-original-major-mode (&optional recursive point)
  "Return major-mode of original buffer.
If optional argument RECURSIVE is non-nil and current buffer has
mime-mother-buffer, it returns original major-mode of the
mother-buffer."
  (if (and recursive mime-mother-buffer)
      (save-excursion
	(set-buffer mime-mother-buffer)
	(mime-preview-original-major-mode recursive)
	)
    (cdr (assq 'major-mode
	       (get-text-property (or point
				      (if (> (point) (buffer-size))
					  (max (1- (point-max)) (point-min))
					(point)))
				  'mime-view-situation)))))


;;; @ entity information
;;;

(defun mime-entity-situation (entity &optional situation)
  "Return situation of ENTITY."
  (let (rest param name)
    ;; Content-Type
    (unless (assq 'type situation)
      (setq rest (or (mime-entity-content-type entity)
		     (make-mime-content-type 'text 'plain))
	    situation (cons (car rest) situation)
	    rest (cdr rest))
      )
    (unless (assq 'subtype situation)
      (or rest
	  (setq rest (or (cdr (mime-entity-content-type entity))
			 '((subtype . plain)))))
      (setq situation (cons (car rest) situation)
	    rest (cdr rest))
      )
    (while rest
      (setq param (car rest))
      (or (assoc (car param) situation)
	  (setq situation (cons param situation)))
      (setq rest (cdr rest)))
    
    ;; Content-Disposition
    (setq rest nil)
    (unless (assq 'disposition-type situation)
      (setq rest (mime-entity-content-disposition entity))
      (if rest
	  (setq situation (cons (cons 'disposition-type
				      (mime-content-disposition-type rest))
				situation)
		rest (mime-content-disposition-parameters rest))
	))
    (while rest
      (setq param (car rest)
	    name (car param))
      (if (cond ((string= name "filename")
		 (if (assq 'filename situation)
		     nil
		   (setq name 'filename)))
		((string= name "creation-date")
		 (if (assq 'creation-date situation)
		     nil
		   (setq name 'creation-date)))
		((string= name "modification-date")
		 (if (assq 'modification-date situation)
		     nil
		   (setq name 'modification-date)))
		((string= name "read-date")
		 (if (assq 'read-date situation)
		     nil
		   (setq name 'read-date)))
		((string= name "size")
		 (if (assq 'size situation)
		     nil
		   (setq name 'size)))
		(t (setq name (cons 'disposition name))
		   (if (assoc name situation)
		       nil
		     name)))
	  (setq situation
		(cons (cons name (cdr param))
		      situation)))
      (setq rest (cdr rest)))
    
    ;; Content-Transfer-Encoding
    (or (assq 'encoding situation)
	(setq situation
	      (cons (cons 'encoding (or (mime-entity-encoding entity)
					"7bit"))
		    situation)))
    
    situation))

(defun mime-view-entity-title (entity)
  (or (mime-entity-read-field entity 'Content-Description)
      (mime-entity-read-field entity 'Subject)
      (mime-entity-filename entity)
      ""))


;; (defsubst mime-raw-point-to-entity-node-id (point &optional message-info)
;;   "Return entity-node-id from POINT in mime-raw-buffer.
;; If optional argument MESSAGE-INFO is not specified,
;; `mime-message-structure' is used."
;;   (mime-entity-node-id (mime-raw-find-entity-from-point point message-info)))

;; (make-obsolete 'mime-raw-point-to-entity-node-id "don't use it.")

;; (defsubst mime-raw-point-to-entity-number (point &optional message-info)
;;   "Return entity-number from POINT in mime-raw-buffer.
;; If optional argument MESSAGE-INFO is not specified,
;; `mime-message-structure' is used."
;;   (mime-entity-number (mime-raw-find-entity-from-point point message-info)))

;; (make-obsolete 'mime-raw-point-to-entity-number "don't use it.")

;; (defun mime-raw-flatten-message-info (&optional message-info)
;;   "Return list of entity in mime-raw-buffer.
;; If optional argument MESSAGE-INFO is not specified,
;; `mime-message-structure' is used."
;;   (or message-info
;;       (setq message-info mime-message-structure))
;;   (let ((dest (list message-info))
;;         (rcl (mime-entity-children message-info)))
;;     (while rcl
;;       (setq dest (nconc dest (mime-raw-flatten-message-info (car rcl))))
;;       (setq rcl (cdr rcl)))
;;     dest))


;;; @ presentation of preview
;;;

;;; @@ entity-button
;;;

;;; @@@ predicate function
;;;

;; fix flim
(defun mime-view-entity-type/subtype (entity)
  (if (not (mime-entity-media-type entity))
      'text/plain
    (intern (format "%s/%s"
		    (mime-entity-media-type entity)
		    (mime-entity-media-subtype entity)))))

(defun mime-view-entity-button-visible-p (entity)
  "Return non-nil if header of ENTITY is visible.
You can customize the visibility by changing `mime-view-button-place-alist'."
  (or
   ;; Check current entity
   ;; type/subtype
   (memq (cdr (assq (mime-view-entity-type/subtype entity)
		    mime-view-button-place-alist))
	 '(around before))
   ;; type
   (memq (cdr (assq (mime-entity-media-type entity)
		    mime-view-button-place-alist))
	 '(around before))
   (and (mime-entity-parent entity)
   (let ((prev-entity
	 (cadr (memq entity
		     (reverse (mime-entity-children
			       (mime-entity-parent entity)))))))
     ;; When previous entity exists
     (and prev-entity
	  (or
	   ;; Check previous eneity
	   ;; type/subtype
	   (memq (cdr
		  (assq
		   (mime-view-entity-type/subtype prev-entity)
		   mime-view-button-place-alist))
		 '(around after))
	   ;; type
	   (memq (cdr
		  (assq
		   (mime-entity-media-type prev-entity)
		   mime-view-button-place-alist))
		 '(around after))))))))

;;; @@@ entity button generator
;;;

(defun mime-view-insert-entity-button (entity &optional body-is-invisible)
  "Insert entity-button of ENTITY."
  (let ((entity-node-id (mime-entity-node-id entity))
	(params (mime-entity-parameters entity))
	(subject (mime-view-entity-title entity)))
    (mime-insert-button
     (concat
      (let ((access-type (assoc "access-type" params))
	    (num (or (cdr (assoc "x-part-number" params))
		     (if (consp entity-node-id)
			 (mapconcat (function
				     (lambda (num)
				       (format "%s" (1+ num))
				       ))
				    (reverse entity-node-id) ".")
		       "0"))
		 ))
	(cond (access-type
	       (let ((server (assoc "server" params)))
		 (setq access-type (cdr access-type))
		 (if server
		     (format "%s %s ([%s] %s)"
			     num subject access-type (cdr server))
		   (let ((site (cdr (assoc "site" params)))
			 (dir (cdr (assoc "directory" params)))
			 (url (cdr (assoc "url" params)))
			 )
		     (if url
			 (format "%s %s ([%s] %s)"
				 num subject access-type url)
		       (format "%s %s ([%s] %s:%s)"
			       num subject access-type site dir))
		     )))
	       )
	      (t
	       (let ((media-type (mime-entity-media-type entity))
		     (media-subtype (mime-entity-media-subtype entity))
		     (charset (cdr (assoc "charset" params)))
		     (encoding (mime-entity-encoding entity)))
		 (concat
		  num " " subject
		  (let ((rest
			 (format " <%s/%s%s%s>"
				 media-type media-subtype
				 (if charset
				     (concat "; " charset)
				   "")
				 (if encoding
				     (concat " (" encoding ")")
				   ""))))
		    (if (>= (+ (current-column)(length rest))(window-width))
			"\n\t")
		    rest)))
	       )))
      (if body-is-invisible
	  " ..."
	""))
     (function mime-preview-play-current-entity))
    ))


;;; @@ entity-header
;;;

(defvar mime-header-presentation-method-alist nil
  "Alist of major mode vs. corresponding header-presentation-method functions.
Each element looks like (SYMBOL . FUNCTION).
SYMBOL must be major mode in raw-buffer or t.  t means default.
Interface of FUNCTION must be (ENTITY SITUATION).")

(defvar mime-view-ignored-field-list
  '(".*Received:" ".*Path:" ".*Id:" "^References:"
    "^Replied:" "^Errors-To:"
    "^Lines:" "^Sender:" ".*Host:" "^Xref:"
    "^Content-Type:" "^Precedence:"
    "^Status:" "^X-VM-.*:")
  "All fields that match this list will be hidden in MIME preview buffer.
Each elements are regexp of field-name.")

(defvar mime-view-visible-field-list '("^Dnas.*:" "^Message-Id:")
  "All fields that match this list will be displayed in MIME preview buffer.
Each elements are regexp of field-name.")


;;; @@ entity-body
;;;

;;; @@@ predicate function
;;;

(in-calist-package 'mime-view)

(defun mime-calist::field-match-method-as-default-rule (calist
							field-type field-value)
  (let ((s-field (assq field-type calist)))
    (cond ((null s-field)
	   (cons (cons field-type field-value) calist)
	   )
	  (t calist))))

(define-calist-field-match-method
  'header #'mime-calist::field-match-method-as-default-rule)

(define-calist-field-match-method
  'body #'mime-calist::field-match-method-as-default-rule)


(defvar mime-preview-condition nil
  "Condition-tree about how to display entity.")

(ctree-set-calist-strictly
 'mime-preview-condition '((type . application)(subtype . t)
			   (encoding . nil)
			   (body . visible)))
(ctree-set-calist-strictly
 'mime-preview-condition '((type . application)(subtype . t)
			   (encoding . "7bit")
			   (body . visible)))
(ctree-set-calist-strictly
 'mime-preview-condition '((type . application)(subtype . t)
			   (encoding . "8bit")
			   (body . visible)))

(ctree-set-calist-strictly
 'mime-preview-condition '((type . application)(subtype . pgp)
			   (body . visible)))

(ctree-set-calist-strictly
 'mime-preview-condition '((type . application)(subtype . x-latex)
			   (body . visible)))

(ctree-set-calist-strictly
 'mime-preview-condition '((type . application)(subtype . x-selection)
			   (body . visible)))

(ctree-set-calist-strictly
 'mime-preview-condition '((type . application)(subtype . x-comment)
			   (body . visible)))

(ctree-set-calist-strictly
 'mime-preview-condition '((type . message)(subtype . delivery-status)
			   (body . visible)))

(ctree-set-calist-strictly
 'mime-preview-condition
 '((body . visible)
   (body-presentation-method . mime-display-text/plain)))

(ctree-set-calist-strictly
 'mime-preview-condition
 '((type . nil)
   (body . visible)
   (body-presentation-method . mime-display-text/plain)))

(ctree-set-calist-strictly
 'mime-preview-condition
 '((type . text)(subtype . enriched)
   (body . visible)
   (body-presentation-method . mime-display-text/enriched)))

(ctree-set-calist-strictly
 'mime-preview-condition
 '((type . text)(subtype . richtext)
   (body . visible)
   (body-presentation-method . mime-display-text/richtext)))

(ctree-set-calist-strictly
 'mime-preview-condition
 '((type . application)(subtype . x-postpet)
   (body . visible)
   (body-presentation-method . mime-display-application/x-postpet)))

(ctree-set-calist-strictly
 'mime-preview-condition
 '((type . text)(subtype . t)
   (body . visible)
   (body-presentation-method . mime-display-text/plain)))

(ctree-set-calist-strictly
 'mime-preview-condition
 '((type . text)(subtype . x-rot13-47-48)
   (body . visible)
   (body-presentation-method . mime-display-text/x-rot13-47-48)))

(ctree-set-calist-strictly
 'mime-preview-condition
 '((type . multipart)(subtype . alternative)
   (body . visible)
   (body-presentation-method . mime-display-multipart/alternative)))

(ctree-set-calist-strictly
 'mime-preview-condition '((type . message)(subtype . partial)
			   (body-presentation-method
			    . mime-display-message/partial-button)))

(ctree-set-calist-strictly
 'mime-preview-condition '((type . message)(subtype . rfc822)
			   (body-presentation-method . nil)
			   (childrens-situation (header . visible)
						(entity-button . invisible))))

(ctree-set-calist-strictly
 'mime-preview-condition '((type . message)(subtype . news)
			   (body-presentation-method . nil)
			   (childrens-situation (header . visible)
						(entity-button . invisible))))


;;; @@@ entity presentation
;;;

(defun mime-display-text/plain (entity situation)
  (save-restriction
    (narrow-to-region (point-max)(point-max))
    (mime-insert-text-content entity)
    (run-hooks 'mime-text-decode-hook)
    (goto-char (point-max))
    (if (not (eq (char-after (1- (point))) ?\n))
	(insert "\n")
      )
    (mime-add-url-buttons)
    (run-hooks 'mime-display-text/plain-hook)
    ))

(defun mime-display-text/richtext (entity situation)
  (save-restriction
    (narrow-to-region (point-max)(point-max))
    (mime-insert-text-content entity)
    (run-hooks 'mime-text-decode-hook)
    (let ((beg (point-min)))
      (remove-text-properties beg (point-max) '(face nil))
      (richtext-decode beg (point-max))
      )))

(defun mime-display-text/enriched (entity situation)
  (save-restriction
    (narrow-to-region (point-max)(point-max))
    (mime-insert-text-content entity)
    (run-hooks 'mime-text-decode-hook)
    (let ((beg (point-min)))
      (remove-text-properties beg (point-max) '(face nil))
      (enriched-decode beg (point-max))
      )))

(defun mime-display-text/x-rot13-47-48 (entity situation)
  (save-restriction
    (narrow-to-region (point-max)(point-max))
    (mime-insert-text-content entity)
    (goto-char (point-max))
    (if (not (eq (char-after (1- (point))) ?\n))
	(insert "\n"))
    (mule-caesar-region (point-min) (point-max))
    (mime-add-url-buttons)))

(put 'unpack 'lisp-indent-function 1)
(defmacro unpack (string &rest body)
  `(let* ((*unpack*string* (string-as-unibyte ,string))
	  (*unpack*index* 0))
     ,@body))

(defun unpack-skip (len)
  (setq *unpack*index* (+ len *unpack*index*)))

(defun unpack-fixed (len)
  (prog1
      (substring *unpack*string* *unpack*index* (+ *unpack*index* len))
    (unpack-skip len)))

(defun unpack-byte ()
  (char-int (aref (unpack-fixed 1) 0)))

(defun unpack-short ()
  (let* ((b0 (unpack-byte))
	 (b1 (unpack-byte)))
    (+ (* 256 b0) b1)))

(defun unpack-long ()
  (let* ((s0 (unpack-short))
	 (s1 (unpack-short)))
    (+ (* 65536 s0) s1)))

(defun unpack-string ()
  (let ((len (unpack-byte)))
    (unpack-fixed len)))

(defun unpack-string-sjis ()
  (decode-mime-charset-string (unpack-string) 'shift_jis))

(defun postpet-decode (string)
  (condition-case nil
      (unpack string
	(let (res)
	  (unpack-skip 4)
	  (set-alist 'res 'carryingcount (unpack-long))
	  (unpack-skip 8)
	  (set-alist 'res 'sentyear (unpack-short))
	  (set-alist 'res 'sentmonth (unpack-short))
	  (set-alist 'res 'sentday (unpack-short))
	  (unpack-skip 8)
	  (set-alist 'res 'petname (unpack-string-sjis))
	  (set-alist 'res 'owner (unpack-string-sjis))
	  (set-alist 'res 'pettype (unpack-fixed 4))
	  (set-alist 'res 'health (unpack-short))
	  (unpack-skip 2)
	  (set-alist 'res 'sex (unpack-long))
	  (unpack-skip 1)
	  (set-alist 'res 'brain (unpack-byte))
	  (unpack-skip 39)
	  (set-alist 'res 'happiness (unpack-byte))
	  (unpack-skip 14)
	  (set-alist 'res 'petbirthyear (unpack-short))
	  (set-alist 'res 'petbirthmonth (unpack-short))
	  (set-alist 'res 'petbirthday (unpack-short))
	  (unpack-skip 8)
	  (set-alist 'res 'from (unpack-string))
	  (unpack-skip 5)
	  (unpack-skip 160)
	  (unpack-skip 4)
	  (unpack-skip 8)
	  (unpack-skip 8)
	  (unpack-skip 26)
	  (set-alist 'res 'treasure (unpack-short))
	  (set-alist 'res 'money (unpack-long))
	  res))
    (error nil)))

(defun mime-display-application/x-postpet (entity situation)
  (save-restriction
    (narrow-to-region (point-max)(point-max))
    (let ((pet (postpet-decode (mime-entity-content entity))))
      (if pet
	  (insert "Petname: " (cdr (assq 'petname pet)) "\n"
		  "Owner: " (cdr (assq 'owner pet)) "\n"
		  "Pettype: " (cdr (assq 'pettype pet)) "\n"
		  "From: " (cdr (assq 'from pet)) "\n"
		  "CarryingCount: " (int-to-string (cdr (assq 'carryingcount pet))) "\n"
		  "SentYear: " (int-to-string (cdr (assq 'sentyear pet))) "\n"
		  "SentMonth: " (int-to-string (cdr (assq 'sentmonth pet))) "\n"
		  "SentDay: " (int-to-string (cdr (assq 'sentday pet))) "\n"
		  "PetbirthYear: " (int-to-string (cdr (assq 'petbirthyear pet))) "\n"
		  "PetbirthMonth: " (int-to-string (cdr (assq 'petbirthmonth pet))) "\n"
		  "PetbirthDay: " (int-to-string (cdr (assq 'petbirthday pet))) "\n"
		  "Health: " (int-to-string (cdr (assq 'health pet))) "\n"
		  "Sex: " (int-to-string (cdr (assq 'sex pet))) "\n"
		  "Brain: " (int-to-string (cdr (assq 'brain pet))) "\n"
		  "Happiness: " (int-to-string (cdr (assq 'happiness pet))) "\n"
		  "Treasure: " (int-to-string (cdr (assq 'treasure pet))) "\n"
		  "Money: " (int-to-string (cdr (assq 'money pet))) "\n"
		  )
	(insert "Invalid format\n"))
      (run-hooks 'mime-display-application/x-postpet-hook))))


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

(defun mime-display-message/partial-button (&optional entity situation)
  (save-restriction
    (goto-char (point-max))
    (if (not (search-backward "\n\n" nil t))
	(insert "\n")
      )
    (goto-char (point-max))
    (narrow-to-region (point-max)(point-max))
    (insert mime-view-announcement-for-message/partial)
    (mime-add-button (point-min)(point-max)
		     #'mime-preview-play-current-entity)
    ))

(defun mime-display-multipart/mixed (entity situation)
  (let ((children (mime-entity-children entity))
	(original-major-mode-cell (assq 'major-mode situation))
	(default-situation
	  (cdr (assq 'childrens-situation situation))))
    (if original-major-mode-cell
	(setq default-situation
	      (cons original-major-mode-cell default-situation)))
    (while children
      (mime-display-entity (car children) nil default-situation)
      (setq children (cdr children))
      )))

(defun mime-display-multipart/alternative (entity situation)
  (let* ((children (mime-entity-children entity))
	 (original-major-mode-cell (assq 'major-mode situation))
	 (default-situation
	   (cdr (assq 'childrens-situation situation)))
	 (i 0)
	 (p 0)
	 (max-score 0)
	 situations)
    (if original-major-mode-cell
	(setq default-situation
	      (cons original-major-mode-cell default-situation)))
    (setq situations
	  (mapcar (function
		   (lambda (child)
		     (let ((situation
			    (or (ctree-match-calist
				 mime-preview-condition
				 (append (mime-entity-situation child)
					 default-situation))
				default-situation)))
		       (if (cdr (assq 'body-presentation-method situation))
			   (let ((score
				  (cdr
				   (or (assoc
					(cons
					 (cdr (assq 'type situation))
					 (cdr (assq 'subtype situation)))
					mime-view-type-subtype-score-alist)
				       (assq
					(cdr (assq 'type situation))
					mime-view-type-subtype-score-alist)
				       (assq
					t
					mime-view-type-subtype-score-alist)
				       ))))
			     (if (> score max-score)
				 (setq p i
				       max-score score)
			       )))
		       (setq i (1+ i))
		       situation)
		     ))
		  children))
    (setq i 0)
    (while children
      (let ((child (car children))
	    (situation (car situations)))
	(mime-display-entity child (if (= i p)
				       situation
				     (del-alist 'body-presentation-method
						(copy-alist situation))))
	)
      (setq children (cdr children)
	    situations (cdr situations)
	    i (1+ i))
      )))

(defun mime-preview-inline ()
  "View part as text without code conversion"
  (interactive)
  (let ((inhibit-read-only t)
	(entity (get-text-property (point) 'mime-view-entity))
	(situation (get-text-property (point) 'mime-view-situation))
	start end)
    (when (and entity
	       (not (get-text-property (point) 'mime-view-entity-header))
	       (not (memq (mime-entity-media-type entity)
			  '(multipart message))))
      (setq start (or (and (not (mime-entity-parent entity))
			   (1+ (previous-single-property-change
				(point)
				'mime-view-entity-header)))
		      (and (not (eq (point) (point-min)))
			   (not (eq (get-text-property (1- (point))
						       'mime-view-entity)
				    entity))
			   (point))
		      (previous-single-property-change (point)
						   'mime-view-entity)
		      (point)))
      (delete-region start
		     (1-
		      (or (next-single-property-change (point)
						       'mime-view-entity)
			  (point-max))))
      (setq start (point))
      (if (mime-view-entity-button-visible-p entity)
	  (mime-view-insert-entity-button entity))
      (insert (mime-entity-content entity))
      (if (and (bolp) (eolp))
	  (delete-char 1)
	(forward-char 1))
      (add-text-properties start (point)
			   (list 'mime-view-entity entity
				 'mime-view-situation situation))
      (goto-char start))))

(defun mime-preview-text (&optional ask-coding)
  "View part as text. MIME charset will be guessed automatically.
With prefix, it prompts for coding-system."
  (interactive "P")
  (let ((inhibit-read-only t)
	(entity (get-text-property (point) 'mime-view-entity))
	(situation (get-text-property (point) 'mime-view-situation))
	(coding (if ask-coding
		    (or (read-coding-system "Coding system: ")
			'undecided)
		  'undecided)))
    (when (and entity
	       (not (get-text-property (point) 'mime-view-entity-header))
	       (not (memq (mime-entity-media-type entity)
			  '(multipart message))))
      (setq start (or (and (not (mime-entity-parent entity))
			   (1+ (previous-single-property-change
				(point)
				'mime-view-entity-header)))
		      (and (not (eq (point) (point-min)))
			   (not (eq (get-text-property (1- (point))
						       'mime-view-entity)
				    entity))
			   (point))
		      (previous-single-property-change (point)
						       'mime-view-entity)
		      (point)))
      (delete-region start
		     (1-
		      (or (next-single-property-change (point)
						       'mime-view-entity)
			  (point-max))))
      (setq start (point))
      (if (mime-view-entity-button-visible-p entity)
	  (mime-view-insert-entity-button entity))
      (insert (decode-coding-string (mime-entity-content entity) coding))
      (if (and (bolp) (eolp))
	  (delete-char 1)
	(forward-char 1))
      (add-text-properties start (point)
			   (list 'mime-view-entity entity
				 'mime-view-situation situation))
      (goto-char start))))
	      

(defun mime-preview-type ()
  "View part as text without code conversion"
  (interactive)
  (let ((inhibit-read-only t)
	(entity (get-text-property (point) 'mime-view-entity))
	(situation (get-text-property (point) 'mime-view-situation))
	(mime-view-force-inline-types t)
	start end)
    (when (and entity
	       (not (get-text-property (point) 'mime-view-entity-header))
	       (not (memq (mime-entity-media-type entity)
			  '(multipart message))))
      (setq start (or (and (not (mime-entity-parent entity))
			   (1+ (previous-single-property-change
				(point)
			       'mime-view-entity-header)))
		      (and (not (eq (point) (point-min)))
			   (not (eq (get-text-property (1- (point))
						       'mime-view-entity)
				    entity))
			   (point))
		      (previous-single-property-change (point)
						   'mime-view-entity)
		      (point)))
      (delete-region start
		     (1-
		      (or (next-single-property-change (point)
						       'mime-view-entity)
			  (point-max))))
      (save-excursion
	(save-restriction
	  (narrow-to-region (point) (point))
	  (mime-display-entity entity (if (eq (assq 'body situation)
					      'invisible)
					  situation
					(put-alist 'body 'visible
						   situation))))
	(if (and (bolp) (eolp))
	      (delete-char 1))))))

(defun mime-preview-buttonize ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
	  point)
      (while (setq point (next-single-property-change
			  (point) 'mime-view-entity))
	(goto-char point)
	(unless (get-text-property (point) 'mime-button-callback)
	  (mime-view-insert-entity-button
	   (get-text-property (point) 'mime-view-entity)))))))

(defun mime-preview-unbuttonize ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
	  point)
      (while (setq point (next-single-property-change
			  (point) 'mime-view-entity))
	(goto-char point)
	(if (get-text-property (point) 'mime-button-callback)
	    (delete-region (point) (save-excursion
				     (goto-char
				      (next-single-property-change
				       (point) 'mime-button-callback)))))))))
	  

;;; @ acting-condition
;;;

(defvar mime-acting-condition nil
  "Condition-tree about how to process entity.")

(if (file-readable-p mailcap-file)
    (let ((entries (mailcap-parse-file)))
      (while entries
	(let ((entry (car entries))
	      view print shared)
	  (while entry
	    (let* ((field (car entry))
		   (field-type (car field)))
	      (cond ((eq field-type 'view)  (setq view field))
		    ((eq field-type 'print) (setq print field))
		    ((memq field-type '(compose composetyped edit)))
		    (t (setq shared (cons field shared))))
	      )
	    (setq entry (cdr entry))
	    )
	  (setq shared (nreverse shared))
	  (ctree-set-calist-with-default
	   'mime-acting-condition
	   (append shared (list '(mode . "play")(cons 'method (cdr view)))))
	  (if print
	      (ctree-set-calist-with-default
	       'mime-acting-condition
	       (append shared
		       (list '(mode . "print")(cons 'method (cdr view))))
	       ))
	  )
	(setq entries (cdr entries))
	)))

(ctree-set-calist-strictly
 'mime-acting-condition
 '((type . application)(subtype . octet-stream)
   (mode . "play")
   (method . mime-detect-content)
   ))

(ctree-set-calist-with-default
 'mime-acting-condition
 '((mode . "extract")
   (method . mime-save-content)))

(ctree-set-calist-strictly
 'mime-acting-condition
 '((type . text)(subtype . x-rot13-47)(mode . "play")
   (method . mime-view-caesar)
   ))
(ctree-set-calist-strictly
 'mime-acting-condition
 '((type . text)(subtype . x-rot13-47-48)(mode . "play")
   (method . mime-view-caesar)
   ))

(ctree-set-calist-strictly
 'mime-acting-condition
 '((type . message)(subtype . rfc822)(mode . "play")
   (method . mime-view-message/rfc822)
   ))
(ctree-set-calist-strictly
 'mime-acting-condition
 '((type . message)(subtype . partial)(mode . "play")
   (method . mime-store-message/partial-piece)
   ))

(ctree-set-calist-strictly
 'mime-acting-condition
 '((type . message)(subtype . external-body)
   ("access-type" . "anon-ftp")
   (method . mime-view-message/external-anon-ftp)
   ))

(ctree-set-calist-strictly
 'mime-acting-condition
 '((type . message)(subtype . external-body)
   ("access-type" . "url")
   (method . mime-view-message/external-url)
   ))

(ctree-set-calist-strictly
 'mime-acting-condition
 '((type . application)(subtype . octet-stream)
   (method . mime-save-content)
   ))


;;; @ quitting method
;;;

(defvar mime-preview-quitting-method-alist
  '((mime-show-message-mode
     . mime-preview-quitting-method-for-mime-show-message-mode))
  "Alist of major-mode vs. quitting-method of mime-view.")

(defvar mime-preview-over-to-previous-method-alist nil
  "Alist of major-mode vs. over-to-previous-method of mime-view.")

(defvar mime-preview-over-to-next-method-alist nil
  "Alist of major-mode vs. over-to-next-method of mime-view.")


;;; @ following method
;;;

(defvar mime-preview-following-method-alist nil
  "Alist of major-mode vs. following-method of mime-view.")

(defvar mime-view-following-required-fields-list
  '("From"))


;;; @ buffer setup
;;;

(defun mime-display-entity (entity &optional situation
				   default-situation preview-buffer)
  (or preview-buffer
      (setq preview-buffer (current-buffer)))
  (let (e nb ne nhb nbb)
    (mime-goto-header-start-point entity)
    (in-calist-package 'mime-view)
    (or situation
	(setq situation
	      (or (ctree-match-calist mime-preview-condition
				      (append (mime-entity-situation entity)
					      default-situation))
		  default-situation)))
    (let ((button-is-invisible
	   (or (eq (cdr (assq 'entity-button situation)) 'invisible)
	       (not (mime-view-entity-button-visible-p entity))))
	   (header-is-visible
	    (eq (cdr (assq 'header situation)) 'visible))
	   (header-presentation-method
	    (or (cdr (assq 'header-presentation-method situation))
		(cdr (assq (cdr (assq 'major-mode situation))
			   mime-header-presentation-method-alist))))
	   (body-is-visible
	    (eq (cdr (assq 'body situation)) 'visible))
	   (body-presentation-method
	    (cdr (assq 'body-presentation-method situation)))
	   (children (mime-entity-children entity)))
      ;; Check if attachment is specified.
      ;; if inline is forced or not.
      (if (not (or (eq t mime-view-force-inline-types)
		   (memq (mime-entity-media-type entity)
			 mime-view-force-inline-types)
		   (memq (mime-view-entity-type/subtype entity)
			 mime-view-force-inline-types)
		   ;; whether Content-Disposition header exists.
		   ;; #### FIXME.  This is way too ugly.
		   (not (and
			 (mime-entity-content-disposition entity)
			 (not (eq 'inline
				  (mime-content-disposition-type
				   (mime-entity-content-disposition entity))))))))
	  ;; This is attachment
	  (setq header-is-visible nil
		body-is-visible nil))
      (set-buffer preview-buffer)
      (setq nb (point))
      (save-restriction
	(narrow-to-region nb nb)
	(or button-is-invisible
	    (if (mime-view-entity-button-visible-p entity)
		(mime-view-insert-entity-button entity
						;; work around composite type
						(not (or children
							 body-is-visible)))))
	(when header-is-visible
	  (setq nhb (point))
	  (if header-presentation-method
	      (funcall header-presentation-method entity situation)
	    (mime-insert-header entity
				mime-view-ignored-field-list
				mime-view-visible-field-list))
	  (run-hooks 'mime-display-header-hook)
	  (put-text-property nhb (point-max) 'mime-view-entity-header entity)
	  (goto-char (point-max))
	  (insert "\n"))
	(setq nbb (point))
	(cond (children)
	      ((and body-is-visible
		    (functionp body-presentation-method))
	       (funcall body-presentation-method entity situation))
	      (t
	       (when button-is-invisible
		 (goto-char (point-max))
		 (mime-view-insert-entity-button entity
						 ;; work around composite type
						 (not (or children
							  body-is-visible))))
	       (or header-is-visible
		   (progn
		     (goto-char (point-max))
		     (insert "\n")
		     ))
	       ))
	(setq ne (point-max)))
      (put-text-property nb ne 'mime-view-entity entity)
      (put-text-property nb ne 'mime-view-situation situation)
      (put-text-property nbb ne 'mime-view-entity-body entity)
      (goto-char ne)
      (if children
	  (if (functionp body-presentation-method)
	      (funcall body-presentation-method entity situation)
	    (mime-display-multipart/mixed entity situation))))))


;;; @ MIME viewer mode
;;;

(defconst mime-view-menu-title "MIME-View")
(defconst mime-view-menu-list
  '((up		 "Move to upper entity"    mime-preview-move-to-upper)
    (previous	 "Move to previous entity" mime-preview-move-to-previous)
    (next	 "Move to next entity"	   mime-preview-move-to-next)
    (scroll-down "Scroll-down"             mime-preview-scroll-down-entity)
    (scroll-up	 "Scroll-up"               mime-preview-scroll-up-entity)
    (play	 "Play current entity"     mime-preview-play-current-entity)
    (extract	 "Extract current entity"  mime-preview-extract-current-entity)
    (print	 "Print current entity"    mime-preview-print-current-entity)
    (raw "View text without code conversion" mime-preview-inline)
    (text "View text with code conversion" mime-preview-text)
    (type "View internally as type" mime-preview-type)
    )
  "Menu for MIME Viewer")

(cond ((featurep 'xemacs)
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
      "u"        (function mime-preview-move-to-upper))
    (define-key mime-view-mode-map
      "p"        (function mime-preview-move-to-previous))
    (define-key mime-view-mode-map
      "n"        (function mime-preview-move-to-next))
    (define-key mime-view-mode-map
      "\e\t"     (function mime-preview-move-to-previous))
    (define-key mime-view-mode-map
      "\t"       (function mime-preview-move-to-next))
    (define-key mime-view-mode-map
      " "        (function mime-preview-scroll-up-entity))
    (define-key mime-view-mode-map
      "\M- "     (function mime-preview-scroll-down-entity))
    (define-key mime-view-mode-map
      "\177"     (function mime-preview-scroll-down-entity))
    (define-key mime-view-mode-map
      "\C-m"     (function mime-preview-next-line-entity))
    (define-key mime-view-mode-map
      "\C-\M-m"  (function mime-preview-previous-line-entity))
    (define-key mime-view-mode-map
      "v"        (function mime-preview-play-current-entity))
    (define-key mime-view-mode-map
      "e"        (function mime-preview-extract-current-entity))
    (define-key mime-view-mode-map
      "i"        (function mime-preview-inline))
    (define-key mime-view-mode-map
      "c"        (function mime-preview-text))
    (define-key mime-view-mode-map
      "t"        (function mime-preview-type))
    (define-key mime-view-mode-map
      "b"        (function mime-preview-buttonize))
    (define-key mime-view-mode-map
      "B"        (function mime-preview-unbuttonize))
    (define-key mime-view-mode-map
      "\C-c\C-p" (function mime-preview-print-current-entity))
    (define-key mime-view-mode-map
      "a"        (function mime-preview-follow-current-entity))
    (define-key mime-view-mode-map
      "q"        (function mime-preview-quit))
    (define-key mime-view-mode-map
      "\C-c\C-x" (function mime-preview-kill-buffer))
    ;; (define-key mime-view-mode-map
    ;;   "<"        (function beginning-of-buffer))
    ;; (define-key mime-view-mode-map
    ;;   ">"        (function end-of-buffer))
    (define-key mime-view-mode-map
      "?"        (function describe-mode))
    (define-key mime-view-mode-map
      [tab] (function mime-preview-move-to-next))
    (define-key mime-view-mode-map
      [delete] (function mime-preview-scroll-down-entity))
    (define-key mime-view-mode-map
      [backspace] (function mime-preview-scroll-down-entity))
    (if (functionp default)
	(cond ((featurep 'xemacs)
	       (set-keymap-default-binding mime-view-mode-map default)
	       )
	      (t
	       (setq mime-view-mode-map
		     (append mime-view-mode-map (list (cons t default))))
	       )))
    (if mouse-button-2
	(define-key mime-view-mode-map
	  mouse-button-2 (function mime-button-dispatcher))
      )
    (cond ((featurep 'xemacs)
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

(defsubst mime-maybe-hide-echo-buffer ()
  "Clear mime-echo buffer and delete window for it."
  (let ((buf (get-buffer mime-echo-buffer-name)))
    (if buf
	(save-excursion
	  (set-buffer buf)
	  (erase-buffer)
	  (let ((win (get-buffer-window buf)))
	    (if win
		(delete-window win)
	      ))
	  (bury-buffer buf)
	  ))))

(defvar mime-view-redisplay nil)

;;;###autoload
(defun mime-display-message (message &optional preview-buffer
				     mother default-keymap-or-function
				     original-major-mode)
  "View MESSAGE in MIME-View mode.

Optional argument PREVIEW-BUFFER specifies the buffer of the
presentation.  It must be either nil or a name of preview buffer.

Optional argument MOTHER specifies mother-buffer of the preview-buffer.

Optional argument DEFAULT-KEYMAP-OR-FUNCTION is nil, keymap or
function.  If it is a keymap, keymap of MIME-View mode will be added
to it.  If it is a function, it will be bound as default binding of
keymap of MIME-View mode."
  (mime-maybe-hide-echo-buffer)
  (let ((win-conf (current-window-configuration)))
    (or preview-buffer
	(setq preview-buffer
	      (concat "*Preview-" (mime-entity-name message) "*")))
    (or original-major-mode
	(setq original-major-mode
	      (with-current-buffer (mime-entity-header-buffer message)
		major-mode)))
    (let ((inhibit-read-only t))
      (set-buffer (get-buffer-create preview-buffer))
      (widen)
      (erase-buffer)
      (if mother
	  (setq mime-mother-buffer mother)
	)
      (setq mime-preview-original-window-configuration win-conf)
      (setq major-mode 'mime-view-mode)
      (setq mode-name "MIME-View")
      (mime-display-entity message nil
			   `((entity-button . invisible)
			     (header . visible)
			     (major-mode . ,original-major-mode))
			   preview-buffer)
      (mime-view-define-keymap default-keymap-or-function)
      (let ((point
	     (next-single-property-change (point-min) 'mime-view-entity)))
	(if point
	    (goto-char point)
	  (goto-char (point-min))
	  (search-forward "\n\n" nil t)
	  ))
      (run-hooks 'mime-view-mode-hook)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      preview-buffer)))

;;;###autoload
(defun mime-view-buffer (&optional raw-buffer preview-buffer mother
				   default-keymap-or-function
				   representation-type)
  "View RAW-BUFFER in MIME-View mode.
Optional argument PREVIEW-BUFFER is either nil or a name of preview
buffer.
Optional argument DEFAULT-KEYMAP-OR-FUNCTION is nil, keymap or
function.  If it is a keymap, keymap of MIME-View mode will be added
to it.  If it is a function, it will be bound as default binding of
keymap of MIME-View mode.
Optional argument REPRESENTATION-TYPE is representation-type of
message.  It must be nil, `binary' or `cooked'.  If it is nil,
`cooked' is used as default."
  (interactive)
  (or raw-buffer
      (setq raw-buffer (current-buffer)))
  (or representation-type
      (setq representation-type
	    (save-excursion
	      (set-buffer raw-buffer)
	      (cdr (or (assq major-mode mime-raw-representation-type-alist)
		       (assq t mime-raw-representation-type-alist)))
	      )))
  (if (eq representation-type 'binary)
      (setq representation-type 'buffer)
    )
  (setq preview-buffer (mime-display-message
			(mime-open-entity representation-type raw-buffer)
			preview-buffer mother default-keymap-or-function))
  (or (get-buffer-window preview-buffer)
      (let ((r-win (get-buffer-window raw-buffer)))
	(if r-win
	    (set-window-buffer r-win preview-buffer)
	  (let ((m-win (and mother (get-buffer-window mother))))
	    (if m-win
		(set-window-buffer m-win preview-buffer)
	      (switch-to-buffer preview-buffer)
	      ))))))

(defun mime-view-mode (&optional mother ctl encoding
				 raw-buffer preview-buffer
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
q		Quit
button-2	Move to point under the mouse cursor
        	and decode current content as `play mode'
"
  (interactive)
  (unless mime-view-redisplay
    (save-excursion
      (if raw-buffer (set-buffer raw-buffer))
      (let ((type
	     (cdr
	      (or (assq major-mode mime-raw-representation-type-alist)
		  (assq t mime-raw-representation-type-alist)))))
	(if (eq type 'binary)
	    (setq type 'buffer)
	  )
	(setq mime-message-structure (mime-open-entity type raw-buffer))
	(or (mime-entity-content-type mime-message-structure)
	    (mime-entity-set-content-type-internal
	     mime-message-structure ctl))
	)
      (or (mime-entity-encoding mime-message-structure)
	  (mime-entity-set-encoding-internal mime-message-structure encoding))
      ))
  (mime-display-message mime-message-structure preview-buffer
			mother default-keymap-or-function)
  )


;;; @@ playing
;;;

(autoload 'mime-preview-play-current-entity "mime-play"
  "Play current entity." t)

(defun mime-preview-extract-current-entity (&optional ignore-examples)
  "Extract current entity into file (maybe).
It decodes current entity to call internal or external method as
\"extract\" mode.  The method is selected from variable
`mime-acting-condition'."
  (interactive "P")
  (mime-preview-play-current-entity ignore-examples "extract")
  )

(defun mime-preview-print-current-entity (&optional ignore-examples)
  "Print current entity (maybe).
It decodes current entity to call internal or external method as
\"print\" mode.  The method is selected from variable
`mime-acting-condition'."
  (interactive "P")
  (mime-preview-play-current-entity ignore-examples "print")
  )


;;; @@ following
;;;

(defun mime-preview-follow-current-entity ()
  "Write follow message to current entity.
It calls following-method selected from variable
`mime-preview-following-method-alist'."
  (interactive)
  (let (entity)
    (while (null (setq entity
		       (get-text-property (point) 'mime-view-entity)))
      (backward-char)
      )
    (let* ((p-beg
	    (previous-single-property-change (point) 'mime-view-entity))
	   p-end
	   ph-end
	   (entity-node-id (mime-entity-node-id entity))
	   (len (length entity-node-id))
	   )
      (cond ((null p-beg)
	     (setq p-beg
		   (if (eq (next-single-property-change (point-min)
							'mime-view-entity)
			   (point))
		       (point)
		     (point-min)))
	     )
	    ((eq (next-single-property-change p-beg 'mime-view-entity)
		 (point))
	     (setq p-beg (point))
	     ))
      (setq p-end (next-single-property-change p-beg 'mime-view-entity))
      (cond ((null p-end)
	     (setq p-end (point-max))
	     )
	    ((null entity-node-id)
	     (setq p-end (point-max))
	     )
	    (t
	     (save-excursion
	       (goto-char p-end)
	       (catch 'tag
		 (let (e)
		   (while (setq e
				(next-single-property-change
				 (point) 'mime-view-entity))
		     (goto-char e)
		     (let ((rc (mime-entity-node-id
				(get-text-property (point)
						   'mime-view-entity))))
		       (or (equal entity-node-id
				  (nthcdr (- (length rc) len) rc))
			   (throw 'tag nil)
			   ))
		     (setq p-end e)
		     ))
		 (setq p-end (point-max))
		 ))
	     ))
      (setq ph-end
	    (previous-single-property-change p-end 'mime-view-entity-header))
      (if (or (null ph-end)
	      (< ph-end p-beg))
	  (setq ph-end p-beg)
	)
      (let* ((mode (mime-preview-original-major-mode 'recursive))
	     (new-name
	      (format "%s-%s" (buffer-name) (reverse entity-node-id)))
	     new-buf
	     (the-buf (current-buffer))
	     fields)
	(save-excursion
	  (set-buffer (setq new-buf (get-buffer-create new-name)))
	  (erase-buffer)
	  (insert-buffer-substring the-buf ph-end p-end)
	  (when (= ph-end p-beg)
	    (goto-char (point-min))
	    (insert ?\n))
	  (goto-char (point-min))
          (let ((current-entity
		 (if (and (eq (mime-entity-media-type entity) 'message)
			  (eq (mime-entity-media-subtype entity) 'rfc822))
		     (mime-entity-children entity)
		   entity))
		str)
	    (while (and current-entity
			(progn
			  (setq str
				(with-current-buffer
				    (mime-entity-header-buffer current-entity)
				  (save-restriction
				    (narrow-to-region
				     (mime-entity-header-start-point
				      current-entity)
				     (mime-entity-header-end-point
				      current-entity))
				    (std11-header-string-except
				     (concat
				      "^"
				      (apply (function regexp-or) fields)
				      ":") ""))))
			  (if (and (eq (mime-entity-media-type
					current-entity) 'message)
				   (eq (mime-entity-media-subtype
					current-entity) 'rfc822))
			      nil
			    (if str
				(insert str)
			      )
			    t)))
	      (setq fields (std11-collect-field-names)
		    current-entity (mime-entity-parent current-entity))
	      )
	    )
	  (let ((rest mime-view-following-required-fields-list)
		field-name ret)
	    (while rest
	      (setq field-name (car rest))
	      (or (std11-field-body field-name)
		  (progn
		    (save-excursion
		      (set-buffer the-buf)
		      (setq ret
			    (when mime-mother-buffer
			      (set-buffer mime-mother-buffer)
			      (mime-entity-fetch-field
			       (get-text-property (point)
						  'mime-view-entity)
			       field-name))))
		    (if ret
			(insert (concat field-name ": " ret "\n"))
		      )))
	      (setq rest (cdr rest))
	      ))
	  (mime-decode-header-in-buffer)
	  )
	(let ((f (cdr (assq mode mime-preview-following-method-alist))))
	  (if (functionp f)
	      (funcall f new-buf)
	    (message
	     (format
	      "Sorry, following method for %s is not implemented yet."
	      mode))
	    ))
	))))


;;; @@ moving
;;;

(defun mime-preview-move-to-upper ()
  "Move to upper entity.
If there is no upper entity, call function `mime-preview-quit'."
  (interactive)
  (let (cinfo)
    (while (null (setq cinfo
		       (get-text-property (point) 'mime-view-entity)))
      (backward-char))
    (let ((r (mime-entity-parent cinfo))
	  point)
      (catch 'tag
	(while (setq point (previous-single-property-change
			    (point) 'mime-view-entity))
	  (goto-char point)
	  (when (eq r (get-text-property (point) 'mime-view-entity))
	    (if (or (eq mime-preview-move-scroll t)
		    (and mime-preview-move-scroll
			 (>= point
			     (save-excursion
			       (move-to-window-line -1)
			       (forward-line (* -1 next-screen-context-lines))
			       (beginning-of-line)
			       (point)))))
		(recenter next-screen-context-lines))
	    (throw 'tag t)))
	(mime-preview-quit)))))

(defun mime-preview-move-to-previous ()
  "Move to previous entity.
If there is no previous entity, it calls function registered in
variable `mime-preview-over-to-previous-method-alist'."
  (interactive)
  (while (and (not (bobp))
	      (null (get-text-property (point) 'mime-view-entity)))
    (backward-char)
    )
  (let ((point (previous-single-property-change (point) 'mime-view-entity)))
    (if (and point
	     (>= point (point-min)))
	(if (get-text-property (1- point) 'mime-view-entity)
	    (progn (goto-char point)
		   (if
		    (or (eq mime-preview-move-scroll t)
			(and mime-preview-move-scroll
			     (<= point
				(save-excursion
				  (move-to-window-line 0)
				  (forward-line next-screen-context-lines)
				  (end-of-line)
				  (point)))))
			(recenter next-screen-context-lines)))
	  (goto-char (1- point))
	  (mime-preview-move-to-previous)
	  )
      (let ((f (assq (mime-preview-original-major-mode)
		     mime-preview-over-to-previous-method-alist)))
	(if f
	    (funcall (cdr f)))))))

(defun mime-preview-move-to-next ()
  "Move to next entity.
If there is no previous entity, it calls function registered in
variable `mime-preview-over-to-next-method-alist'."
  (interactive)
  (while (and (not (eobp))
	      (null (get-text-property (point) 'mime-view-entity)))
    (forward-char))
  (let ((point (next-single-property-change (point) 'mime-view-entity)))
    (if (and point
	     (<= point (point-max)))
	(progn
	  (goto-char point)
	  (if (null (get-text-property point 'mime-view-entity))
	      (mime-preview-move-to-next)
	    (and
	     (or (eq mime-preview-move-scroll t)
		 (and mime-preview-move-scroll
		      (>= point
			 (save-excursion
			   (move-to-window-line -1)
			   (forward-line
			    (* -1 next-screen-context-lines))
			   (beginning-of-line)
			   (point)))))
		 (recenter next-screen-context-lines))))
      (let ((f (assq (mime-preview-original-major-mode)
		     mime-preview-over-to-next-method-alist)))
	(if f
	    (funcall (cdr f)))))))

(defun mime-preview-scroll-up-entity (&optional h)
  "Scroll up current entity.
If reached to (point-max), it calls function registered in variable
`mime-preview-over-to-next-method-alist'."
  (interactive)
  (if (eobp)
      (let ((f (assq (mime-preview-original-major-mode)
		     mime-preview-over-to-next-method-alist)))
	(if f
	    (funcall (cdr f))))
    (let ((point
	   (or (next-single-property-change (point) 'mime-view-entity)
	       (point-max)))
	  (bottom (window-end (selected-window))))
      (if (and (not h)
	       (> bottom point)
	       (not mime-preview-scroll-full-screen))
	  (progn (goto-char point)
		 (recenter next-screen-context-lines))
	(condition-case nil
	    (scroll-up h)
	  (end-of-buffer
	   (goto-char (point-max))))))))

(defun mime-preview-scroll-down-entity (&optional h)
  "Scroll down current entity.
If reached to (point-min), it calls function registered in variable
`mime-preview-over-to-previous-method-alist'."
  (interactive)
  (if (bobp)
      (let ((f (assq (mime-preview-original-major-mode)
		     mime-preview-over-to-previous-method-alist)))
	(if f
	    (funcall (cdr f))))
    (let ((point
	   (or (previous-single-property-change (point) 'mime-view-entity)
	       (point-min)))
	  (top (window-start (selected-window))))
      (if (and (not h)
	       (< top point)
	       (not mime-preview-scroll-full-screen))
	  (progn (goto-char point)
		 (recenter (* -1 next-screen-context-lines)))
	(condition-case nil
	    (scroll-down h)
	  (beginning-of-buffer
	   (goto-char (point-min))))))))

(defun mime-preview-next-line-entity (&optional lines)
  "Scroll up one line (or prefix LINES lines).
If LINES is negative, scroll down LINES lines."
  (interactive "p")
  (mime-preview-scroll-up-entity (or lines 1)))

(defun mime-preview-previous-line-entity (&optional lines)
  "Scrroll down one line (or prefix LINES lines).
If LINES is negative, scroll up LINES lines."
  (interactive "p")
  (mime-preview-scroll-down-entity (or lines 1)))

;;; @@ quitting
;;;

(defun mime-preview-quit ()
  "Quit from MIME-preview buffer.
It calls function registered in variable
`mime-preview-quitting-method-alist'."
  (interactive)
  (let ((r (assq (mime-preview-original-major-mode)
		 mime-preview-quitting-method-alist)))
    (if r
	(funcall (cdr r))
      )))

(defun mime-preview-kill-buffer ()
  (interactive)
  (kill-buffer (current-buffer))
  )


;;; @ end
;;;

(provide 'mime-view)

(run-hooks 'mime-view-load-hook)

;;; mime-view.el ends here
