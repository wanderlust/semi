;;; semi-def.el --- definition module for SEMI -*- coding: iso-8859-4; -*-

;; Copyright (C) 1995,96,97,98,99,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: definition, MIME, multimedia, mail, news

;; This file is part of SEMI (Sample of Emacs MIME Implementation).

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

(eval-when-compile (require 'cl))

(require 'custom)

(defconst mime-user-interface-product ["EMIKO" (1 14 1) "Choanoflagellata"]
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

(define-widget 'mime-button 'link
  "Widget for MIME button."
  :action 'mime-button-action)

(defun mime-button-action (widget &optional event)
  (let ((function (widget-get widget :mime-button-callback))
	(data (widget-get widget :mime-button-data)))
    (when function
      (funcall function data))))
    
(defsubst mime-insert-button (string function &optional data)
  "Insert STRING as button with callback FUNCTION and DATA."
  (save-restriction
    (narrow-to-region (point)(point))
    ;; Maybe we should introduce button formatter such as
    ;; `gnus-mime-button-line-format'.
    (insert "[" string "]")
    ;; XEmacs -- when `widget-glyph-enable' is non nil, widget values are not
    ;; guaranteed to be underlain.
    (widget-convert-button 'mime-button (point-min)(point-max)
			   :mime-button-callback function
			   :mime-button-data data)
    (insert "\n")))


;;; @ for URL
;;;

(defcustom mime-browse-url-regexp
  (concat "\\(https?\\|ftps?\\|file\\|gopher\\|news\\|nntps?\\|telnets?\\|wais\\|mailto\\):"
	  "\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?"
	  "[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,;]*[-a-zA-Z0-9_=#$@~`%&*+|\\/;]")
  "Regexp to match URL in text body."
  :group 'mime
  :type 'regexp)

(defcustom mime-browse-url-function (function browse-url)
  "Function to browse URL."
  :group 'mime
  :type 'function)

(define-widget 'mime-url-link 'url-link
  "A link to an www page.")

(defsubst mime-add-url-buttons ()
  "Add URL-buttons for text body."
  (goto-char (point-min))
  (while (re-search-forward mime-browse-url-regexp nil t)
    (widget-convert-button 'mime-url-link (match-beginning 0)(match-end 0)
			   (match-string-no-properties 0))))


;;; @ menu
;;;

(defmacro mime-popup-menu-bogus-filter-constructor (menu)
  ;; #### Kludge for FSF Emacs-style menu.
  (let ((bogus-menu (make-symbol "bogus-menu")))
    `(let (,bogus-menu selection function)
       (easy-menu-define ,bogus-menu nil nil ,menu)
       (setq selection (x-popup-menu t ,bogus-menu))
       (when selection
	 (setq function (lookup-key ,bogus-menu (apply #'vector selection)))
	 ;; If a callback entry has no name, easy-menu wraps its value.
	 ;; See `easy-menu-make-symbol'.
	 (if (eq t (compare-strings "menu-function-" 0 nil
				    (symbol-name function) 0 14))
	     (car (last (symbol-function function)))
	   function)))))

;;; While XEmacs can have both X and tty frames at the same time with
;;; gnuclient, we shouldn't emulate in text-mode here.

(static-if (featurep 'xemacs)
    (defalias 'mime-popup-menu-popup 'popup-menu)
  (defun mime-popup-menu-popup (menu &optional event)
    (let ((function (mime-popup-menu-bogus-filter-constructor menu)))
      (when (symbolp function)
	(funcall function)))))

(static-if (featurep 'xemacs)
    (defun mime-popup-menu-select (menu &optional event)
      (let ((selection (get-popup-menu-response menu event)))
	(event-object selection)))
  (defun mime-popup-menu-select (menu &optional event)
    (mime-popup-menu-bogus-filter-constructor menu)))

(static-if (featurep 'xemacs)
    (defun mime-should-use-popup-menu ()
      (mouse-event-p last-command-event))
  (defun mime-should-use-popup-menu ()
    (memq (event-basic-type last-command-event) '(mouse-1 mouse-2 mouse-3))))

(defun mime-menu-select (prompt menu &optional event)
  (if (mime-should-use-popup-menu)
      (mime-popup-menu-select menu event)
    (let ((rest (cdr menu)))
      (while rest
	(setcar rest (append (car rest) nil))
	(setq rest (cdr rest)))
      (nth 1 (assoc (completing-read prompt (cdr menu)) (cdr menu))))))


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
