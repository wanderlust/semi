;;; semi-def.el --- definition module for WEMI -*- coding: iso-8859-4; -*-

;; Copyright (C) 1995,96,97,98,99,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: definition, MIME, multimedia, mail, news

;; This file is part of WEMI (Widget based Emacs MIME Implementation).

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

(eval-when-compile (require 'static))
(require 'custom)

(defconst mime-user-interface-product ["WEMI" (1 14 5) "Kakegawa"]
  "Product name, version number and code name of MIME-kernel package.")

(autoload 'mule-caesar-region "mule-caesar"
  "Caesar rotation of current region." t)

(autoload 'widget-convert-button "wid-edit")


;;; @ constants
;;;

(defconst mime-echo-buffer-name "*MIME-echo*"
  "Name of buffer to display MIME-playing information.")

(defconst mime-temp-buffer-name " *MIME-temp*")


;;; @ button
;;;

(defun mime-create-widget-button (string function)
  "Display STRING as a widget button with the callback FUNCTION.
Under XEmacs, the function `mime-create-xpm-button' might be identical
to the function `mime-create-widget-button' if the feature `xpm' is not
provided or the TTY frame is used."
  (let ((start (point)))
    (widget-create
     'push-button
     :action `(lambda (widget &optional event) (,function))
     :mouse-down-action `(lambda (widget event)
			   (let (buf point)
			     (save-window-excursion
			       (mouse-set-point event)
			       (setq buf (current-buffer)
				     point (point)))
			     (save-excursion
			       (set-buffer buf)
			       (goto-char point)
			       (,function))))
     string)
    ;; There may be only one string "*" behind the widget button.  We
    ;; should replace it with the string as it can be seen because it
    ;; will be yanked into the reply messages.
    (static-when (featurep 'xemacs)
      (let ((end (point))
	    extent)
	(insert "[" string "]")
	(while (setq extent (extent-at start nil nil extent))
	  ;; Avoid removing extents of next part.
	  (if (eq (extent-end-position extent) end)
	      (set-extent-endpoints extent end (point))))
	(delete-region start end)))
    (add-text-properties start (point)
			 (list 'start-open t
			       'mime-button t)))
  (insert "\n"))

(static-when (featurep 'xemacs)
  (defcustom mime-xpm-button-shadow-thickness 3
    "A number of pixels should be used for the shadows on the edges of
the buttons."
    :group 'mime
    :type 'integer)

  (defcustom mime-xpm-button-foreground "Yellow"
    "A color used to display the text."
    :group 'mime
    :type 'string)

  (defcustom mime-xpm-button-background "#a0a0d0"
    "A background color the text will be displayed upon."
    :group 'mime
    :type 'string)

  (defvar mime-xpm-button-glyph-cache nil)

  (if (featurep 'xpm)
      (defun mime-create-xpm-button (string function)
	"Display STRING as a XPM button with the callback FUNCTION.
It might be identical to the function `mime-create-widget-button'
if the TTY frame is used."
	;; `device-on-widow-system-p' must be checked at run-time.
	(if (device-on-window-system-p)
	    (progn
	      (let ((old-point (point)))
		(set-extent-properties (make-extent (point)
						    (progn
						      (insert "[" string "]")
						      (point)))
				       '(invisible t intangible t
						   start-open t))
		(add-text-properties old-point (point)
				     '(mime-button t start-open t)))
	      (let* ((spec (list string
				 mime-xpm-button-shadow-thickness
				 mime-xpm-button-foreground
				 mime-xpm-button-background))
		     (button (cdr (assoc spec mime-xpm-button-glyph-cache))))
		(or button
		    (set-alist 'mime-xpm-button-glyph-cache spec
			       (setq button (apply (function xpm-button-create)
						   spec))))
		(let* ((extent (make-extent (point) (point)))
		       (down-glyph (make-glyph (car (cdr button))))
		       (up-glyph (make-glyph (car button)))
		       (down-func `(lambda (event)
				     (interactive "e")
				     (set-extent-begin-glyph ,extent
							     ,down-glyph)))
		       (up-func `(lambda (event)
				   (interactive "e")
				   (mouse-set-point event)
				   (set-extent-begin-glyph ,extent ,up-glyph)
				   (,function)))
		       (keymap (make-sparse-keymap)))
		  (define-key keymap 'button1 down-func)
		  (define-key keymap 'button2 down-func)
		  (define-key keymap 'button1up up-func)
		  (define-key keymap 'button2up up-func)
		  (set-extent-begin-glyph extent up-glyph)
		  (set-extent-property extent 'keymap keymap))
		(insert "\n")))
	  (mime-create-widget-button string function)))
    (fset 'mime-create-xpm-button 'mime-create-widget-button)))

(defcustom mime-create-button-function 'mime-create-widget-button
  "A function called to create the content button."
  :group 'mime
  :type (list
	 'cons
	 :convert-widget
	 (function
	  (lambda (widget)
	    (list
	     'radio
	     :args
	     (append
	      '((const :tag "Widget button" mime-create-widget-button))
	      (static-when (featurep 'xemacs)
		'((const :tag "Xpm button" mime-create-xpm-button)))
	      '((function :tag "Other"))))))))

(defsubst mime-insert-button (string function &optional data)
  "Insert STRING as button with callback FUNCTION and DATA."
  (unless (bolp)
    (insert "\n"))
  (save-restriction
    (narrow-to-region (point) (point))
    (mapcar (function
	     (lambda (line)
	       (funcall mime-create-button-function line function)))
	    (split-string string "\n"))))

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
	    data (get-text-property (point) 'mime-button-data)))
    (save-excursion
      (set-buffer buf)
      (goto-char point)
      (if func
	  (apply func data)
	(if (fboundp mime-button-mother-dispatcher)
	    (funcall mime-button-mother-dispatcher event))))))


;;; @ for URL
;;;

(defcustom mime-browse-url-regexp
  (concat "\\(https?\\|ftps?\\|file\\|gopher\\|news\\|nntps?\\|telnets?\\|wais\\|mailto\\):"
	  "\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?"
	  "[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]*[-a-zA-Z0-9_=#$@~`%&*+|\\/]")
  "*Regexp to match URL in text body."
  :group 'mime
  :type 'regexp)

(defcustom mime-browse-url-function (function browse-url)
  "*Function to browse URL."
  :group 'mime
  :type 'function)

(defsubst mime-add-url-buttons ()
  "Add URL-buttons for text body."
  (goto-char (point-min))
  (while (re-search-forward mime-browse-url-regexp nil t)
    (let ((beg (match-beginning 0))
	  (end (match-end 0)))
      (widget-convert-button 'mime-url-link beg end
			     (buffer-substring beg end))
      (static-unless (featurep 'xemacs)
	(overlay-put (make-overlay beg end) 'local-map widget-keymap)))))

(define-widget 'mime-url-link 'link
  "A link to an www page."
  :help-echo 'widget-url-link-help-echo
  :action 'widget-mime-url-link-action)

(defun widget-mime-url-link-action (widget &optional event)
  "Open the url specified by WIDGET."
  (funcall mime-browse-url-function (widget-value widget)))


;;; @ menu
;;;

(static-if (featurep 'xemacs)
    (defun select-menu-alist (title menu-alist)
      ;; XEmacs can have both X and tty frames at the same time with
      ;; gnuclient.
      (if (device-on-window-system-p)
	  (let (ret)
	    (popup-menu
	     ;; list* is CL function, but CL is a part of XEmacs.
	     (list* title
		    "---"
		    (mapcar (function
			     (lambda (cell)
			       (vector (car cell)
				       `(progn
					  (setq ret ',(cdr cell))
					  (throw 'exit nil))
				       t)))
			    menu-alist)))
	    (recursive-edit)
	    ret)
	(cdr
	 (assoc (completing-read (concat title " : ") menu-alist)
		menu-alist))))
  (if window-system
      (defun select-menu-alist (title menu-alist)
	(x-popup-menu
	 (list '(1 1) (selected-window))
	 (list title (cons title menu-alist))))
    (defun select-menu-alist (title menu-alist)
      (cdr
       (assoc (completing-read (concat title " : ") menu-alist)
	      menu-alist)))))


;;; @ Other Utility
;;;

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
		      (autoload method file))))
	    (error "Function for mode `%s' is not found." mode)))
      (error "Variable for target-type `%s' is not found." target-type))))


;;; @ end
;;;

(provide 'semi-def)

;;; semi-def.el ends here
