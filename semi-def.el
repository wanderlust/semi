;;; semi-def.el --- definition module for SEMI

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: definition, MIME, multimedia, mail, news

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

(require 'emu)

(eval-when-compile (require 'cl))


(defconst mime-module-version '("SEMI" "Tomari" 1 4 5)
  "Implementation name, version name and numbers of MIME-kernel package.")

(autoload 'mule-caesar-region "mule-caesar"
  "Caesar rotation of current region." t)


;;; @ variables
;;;

(defvar mime/use-multi-frame
  (and (>= emacs-major-version 19) window-system))


;;; @ constants
;;;

(defconst mime-echo-buffer-name "*MIME-echo*"
  "Name of buffer to display MIME-playing information.")

(defconst mime-temp-buffer-name " *MIME-temp*")


;;; @ button
;;;

(defcustom mime-button-face 'bold
  "Face used for content-button or URL-button of MIME-Preview buffer."
  :group 'mime
  :type 'face)

(defcustom mime-button-mouse-face 'highlight
  "Face used for MIME-preview buffer mouse highlighting."
  :group 'mime
  :type 'face)

(defsubst mime-add-button (from to function &optional data)
  "Create a button between FROM and TO with callback FUNCTION and DATA."
  (let ((overlay (make-overlay from to)))
    (and mime-button-face
	 (overlay-put overlay 'face mime-button-face))
    (and mime-button-mouse-face
	 (overlay-put overlay 'mouse-face mime-button-mouse-face))
    (add-text-properties from to (list 'mime-button-callback function))
    (and data
	 (add-text-properties from to (list 'mime-button-data data)))
    ;;(add-text-properties from to (list 'keymap widget-keymap))
    ))

(defsubst mime-insert-button (string function &optional data)
  "Insert STRING as button with callback FUNCTION and DATA."
  (save-restriction
    (narrow-to-region (point)(point))
    (insert (concat "[" string "]"))
    ;; (widget-push-button-value-create
    ;;  (widget-convert 'push-button
    ;;                  :notify (lambda (&rest ignore)
    ;;                            (mime-preview-play-current-entity)
    ;;                            )
    ;;                  string))
    (insert "\n")
    (mime-add-button (point-min)(point-max) function data)
    ))

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


;;; @ menu
;;;

(if window-system
    (if (featurep 'xemacs)
	(defun select-menu-alist (title menu-alist)
	  (let (ret)
	    (popup-menu
	     (list* title
		    "---"
		    (mapcar (function
			     (lambda (cell)
			       (vector (car cell)
				       `(progn
					  (setq ret ',(cdr cell))
					  (throw 'exit nil)
					  )
				       t)
			       ))
			    menu-alist)
		    ))
	    (recursive-edit)
	    ret))
      (defun select-menu-alist (title menu-alist)
	(x-popup-menu
	 (list '(1 1) (selected-window))
	 (list title (cons title menu-alist))
	 ))
      )
  (defun select-menu-alist (title menu-alist)
    (cdr
     (assoc (completing-read (concat title " : ") menu-alist)
	    menu-alist)
     ))
  )


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
    (mime-sign		mime-mc-pgp-sign-region		"mime-mc")
    (traditional-sign	mc-pgp-sign-region		"mc-pgp")
    (encrypt		mime-mc-pgp-encrypt-region	"mime-mc")
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
  `(cadr (assq ,method (symbol-value 'pgp-function-alist)))
  )

(mapcar (function
	 (lambda (method)
	   (autoload (cadr method)(nth 2 method))
	   ))
	pgp-function-alist)


;;; @ field
;;;

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


(defvar mime-condition-type-alist
  '((preview . mime-preview-condition)
    (action . mime-acting-condition)))

(defvar mime-condition-mode-alist
  '((with-default . ctree-set-calist-with-default)
    (t . ctree-set-calist-strictly)))

(defun mime-add-condition (target-type condition &optional mode file)
  "Add CONDITION to database specified by TARGET-TYPE.
TARGET-TYPE must be 'preview or 'action.  
If optional argument MODE is 'strict or nil (omitted), CONDITION is
added strictly.
If optional argument MODE is 'with-default, CONDITION is added with
default rule.
If optional argument FILE is specified, it is loaded when CONDITION is
activate."
  (let ((sym (cdr (assq target-type mime-condition-type-alist))))
    (if sym
	(let ((func (cdr (or (assq mode mime-condition-mode-alist)
			     (assq t mime-condition-mode-alist)))))
	  (if (fboundp func)
	      (progn
		(funcall func sym condition)
		(if file
		    (let ((method (cdr (assq 'method condition))))
		      (autoload method file)
		      ))
		)
	    (error "Function for mode `%s' is not found." mode)
	    ))
      (error "Variable for target-type `%s' is not found." target-type)
      )))


;;; @ end
;;;

(provide 'semi-def)

;;; semi-def.el ends here
