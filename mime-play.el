;;; mime-play.el --- Playback processing module for mime-view.el

;; Copyright (C) 1994,1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1995/9/26 (separated from tm-view.el)
;;	Renamed: 1997/2/21 from tm-play.el
;; Keywords: MIME, multimedia, mail, news

;; This file is part of SEMI (Secretariat of Emacs MIME Interfaces).

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

(require 'mime-view)
(require 'alist)
(require 'filename)

(eval-when-compile (require 'mime-text))


(defvar mime-acting-situation-examples nil)

(defun mime-save-acting-situation-examples ()
  (let* ((file mime-acting-situation-examples-file)
	 (buffer (get-buffer-create " *mime-example*")))
    (unwind-protect
        (save-excursion
          (set-buffer buffer)
          (setq buffer-file-name file)
          (erase-buffer)
          (insert ";;; " (file-name-nondirectory file) "\n")
          (insert "\n;; This file is generated automatically by "
                  mime-view-version-string "\n\n")
	  (insert ";;; Code:\n\n")
	  (pp `(setq mime-acting-situation-examples
		     ',mime-acting-situation-examples)
	      (current-buffer))
	  (insert "\n;;; "
                  (file-name-nondirectory file)
                  " ends here.\n")
          (save-buffer))
      (kill-buffer buffer))))

(add-hook 'kill-emacs-hook 'mime-save-acting-situation-examples)

  
;;; @ content decoder
;;;

(defvar mime-preview-after-decoded-position nil)

(defun mime-preview-play-current-entity (&optional mode)
  "Play current entity.
It decodes current entity to call internal or external method.  The
method is selected from variable `mime-acting-condition'.
If MODE is specified, play as it.  Default MODE is \"play\"."
  (interactive)
  (or mode
      (setq mode "play"))
  (let ((entity-info (get-text-property (point) 'mime-view-entity)))
    (if entity-info
	(let ((the-buf (current-buffer))
	      (raw-buffer (get-text-property (point) 'mime-view-raw-buffer)))
	  (setq mime-preview-after-decoded-position (point))
	  (set-buffer raw-buffer)
	  (mime-raw-play-entity entity-info mode)
	  (when (eq (current-buffer) raw-buffer)
	    (set-buffer the-buf)
	    (goto-char mime-preview-after-decoded-position)
	    )))))

(defun mime-sort-situation (situation)
  (sort situation
	#'(lambda (a b)
	    (let ((a-t (car a))
		  (b-t (car b))
		  (order '((type . 1)
			   (subtype . 2)
			   (mode . 3)
			   (major-mode . 4)))
		  a-order b-order)
	      (if (symbolp a-t)
		  (let ((ret (assq a-t order)))
		    (if ret
			(setq a-order (cdr ret))
		      (setq a-order 5)
		      ))
		(setq a-order 6)
		)
	      (if (symbolp b-t)
		  (let ((ret (assq b-t order)))
		    (if ret
			(setq b-order (cdr ret))
		      (setq b-order 5)
		      ))
		(setq b-order 6)
		)
	      (if (= a-order b-order)
		  (string< (format "%s" a-t)(format "%s" b-t))
		(< a-order b-order))
	      )))
  )

(defun mime-raw-play-entity (entity-info &optional mode)
  "Play entity specified by ENTITY-INFO.
It decodes the entity to call internal or external method.  The method
is selected from variable `mime-acting-condition'.  If MODE is
specified, play as it.  Default MODE is \"play\"."
  (let ((beg (mime-entity-point-min entity-info))
	(end (mime-entity-point-max entity-info))
	(content-type (mime-entity-content-type entity-info))
	(encoding (mime-entity-encoding entity-info)))
    (or content-type
	(setq content-type (make-mime-content-type 'text 'plain)))
    (let (method cal ret)
      (setq cal (list* (cons 'major-mode major-mode)
		       (cons 'encoding encoding)
		       content-type))
      (if mode
	  (setq cal (cons (cons 'mode mode) cal))
	)
      (setq ret
	    (or (ctree-match-calist mime-acting-situation-examples cal)
		(ctree-match-calist-partially mime-acting-situation-examples
					      cal)
		cal))
      (setq ret
	    (or (ctree-find-calist mime-acting-condition ret
				   mime-view-find-every-acting-situation)
		(ctree-find-calist mime-acting-condition cal
				   mime-view-find-every-acting-situation)
		))
      (cond ((cdr ret)
	     (setq ret (select-menu-alist
			"Methods"
			(mapcar (function
				 (lambda (situation)
				   (cons
				    (format "%s"
					    (cdr (assq 'method situation)))
				    situation)))
				ret)))
	     (setq ret (mime-sort-situation ret))
	     (ctree-set-calist-strictly 'mime-acting-situation-examples ret)
	     )
	    (t
	     (setq ret (car ret))
	     ))
      (setq method (cdr (assq 'method ret)))
      (cond ((and (symbolp method)
		  (fboundp method))
	     (funcall method beg end ret)
	     )
	    ((stringp method)
	     (mime-activate-mailcap-method beg end ret)
	     )
	    ((and (listp method)(stringp (car method)))
	     (mime-activate-external-method beg end ret)
	     )
	    (t
	     (mime-show-echo-buffer
	      "No method are specified for %s\n"
	      (mime-type/subtype-string
	       (mime-content-type-primary-type content-type)
	       (mime-content-type-subtype content-type))
	      )))
      )))


;;; @ external decoder
;;;

(defvar mime-mailcap-method-filename-alist nil)

(defun mime-activate-mailcap-method (start end situation)
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let ((method (cdr (assoc 'method situation)))
	    (name (expand-file-name (mime-raw-get-filename situation)
				    mime-temp-directory)))
	(mime-write-decoded-region (if (re-search-forward "^$" end t)
				       (1+ (match-end 0))
				     (point-min))
				   end name
				   (cdr (assq 'encoding situation)))
	(message "External method is starting...")
	(let ((process
	       (let ((command
		      (mailcap-format-command
		       method
		       (cons (cons 'filename name) situation))))
		 (start-process command mime-echo-buffer-name
				shell-file-name shell-command-switch command)
		 )))
	  (set-alist 'mime-mailcap-method-filename-alist process name)
	  (set-process-sentinel process 'mime-mailcap-method-sentinel)
	  )
	;;(mime-show-echo-buffer)
	))))

(defun mime-mailcap-method-sentinel (process event)
  (let ((file (cdr (assq process mime-mailcap-method-filename-alist))))
    (if (file-exists-p file)
	(delete-file file)
      ))
  (remove-alist 'mime-mailcap-method-filename-alist process)
  (message (format "%s %s" process event)))

(defun mime-activate-external-method (beg end cal)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (let ((method (cdr (assoc 'method cal)))
	    (name (mime-raw-get-filename cal))
	    )
	(if method
	    (let ((file (make-temp-name
			 (expand-file-name "TM" mime-temp-directory)))
		  b args)
	      (if (nth 1 method)
		  (setq b beg)
		(setq b
		      (if (re-search-forward "^$" nil t)
			  (1+ (match-end 0))
			(point-min)
			))
		)
	      (goto-char b)
	      (write-region b end file)
	      (message "External method is starting...")
	      (setq cal (put-alist
			 'name (replace-as-filename name) cal))
	      (setq cal (put-alist 'file file cal))
	      (setq args (nconc
			  (list (car method)
				mime-echo-buffer-name (car method)
				)
			  (mime-make-external-method-args
			   cal (cdr (cdr method)))
			  ))
	      (apply (function start-process) args)
	      (mime-show-echo-buffer)
	      ))
	))))

(defun mime-make-external-method-args (cal format)
  (mapcar (function
	   (lambda (arg)
	     (if (stringp arg)
		 arg
	       (let* ((item (eval arg))
		      (ret (cdr (assoc item cal)))
		      )
		 (if ret
		     ret
		   (if (eq item 'encoding)
		       "7bit"
		     ""))
		 ))
	     ))
	  format))

(defvar mime-echo-window-is-shared-with-bbdb t
  "*If non-nil, mime-echo window is shared with BBDB window.")

(defvar mime-echo-window-height
  (function
   (lambda ()
     (/ (window-height) 5)
     ))
  "*Size of mime-echo window.
It allows function or integer.  If it is function,
`mime-show-echo-buffer' calls it to get height of mime-echo window.
Otherwise `mime-show-echo-buffer' uses it as height of mime-echo
window.")

(defun mime-show-echo-buffer (&rest forms)
  "Show mime-echo buffer to display MIME-playing information."
  (get-buffer-create mime-echo-buffer-name)
  (let ((the-win (selected-window))
	(win (get-buffer-window mime-echo-buffer-name))
	)
    (or win
	(if (and mime-echo-window-is-shared-with-bbdb
		 (boundp 'bbdb-buffer-name)
		 (setq win (get-buffer-window bbdb-buffer-name))
		 )
	    (set-window-buffer win mime-echo-buffer-name)
	  (select-window (get-buffer-window mime-preview-buffer))
	  (setq win (split-window-vertically
		     (- (window-height)
			(if (functionp mime-echo-window-height)
			    (funcall mime-echo-window-height)
			  mime-echo-window-height)
			)))
	  (set-window-buffer win mime-echo-buffer-name)
	  ))
    (select-window win)
    (goto-char (point-max))
    (if forms
	(insert (apply (function format) forms))
      )
    (select-window the-win)
    ))


;;; @ file name
;;;

(defvar mime-view-file-name-char-regexp "[A-Za-z0-9+_-]")

(defvar mime-view-file-name-regexp-1
  (concat mime-view-file-name-char-regexp "+\\."
	  mime-view-file-name-char-regexp "+"))

(defvar mime-view-file-name-regexp-2
  (concat (regexp-* mime-view-file-name-char-regexp)
	  "\\(\\." mime-view-file-name-char-regexp "+\\)*"))

(defun mime-raw-get-original-filename (param)
  (or (if (member (cdr (assq 'encoding param))
		  mime-view-uuencode-encoding-name-list)
	  (mime-raw-get-uu-filename))
      (let (ret)
	(or (if (or (and (setq ret (mime-read-Content-Disposition))
			 (setq ret
			       (assoc
				"filename"
				(mime-content-disposition-parameters ret)))
			 )
		    (setq ret (assoc "name" param))
		    (setq ret (assoc "x-name" param))
		    )
		(std11-strip-quoted-string (cdr ret))
	      )
	    (if (setq ret
		      (std11-find-field-body '("Content-Description"
					       "Subject")))
		(if (or (string-match mime-view-file-name-regexp-1 ret)
			(string-match mime-view-file-name-regexp-2 ret))
		    (substring ret (match-beginning 0)(match-end 0))
		  ))
	    ))
      ))

(defun mime-raw-get-filename (param)
  (replace-as-filename (mime-raw-get-original-filename param))
  )


;;; @ file extraction
;;;

(defun mime-method-to-save (beg end cal)
  (goto-char beg)
  (let* ((name
	  (save-restriction
	    (narrow-to-region beg end)
	    (mime-raw-get-filename cal)
	    ))
	 (encoding (or (cdr (assq 'encoding cal)) "7bit"))
	 (filename
          (if (and name (not (string-equal name "")))
	      (expand-file-name name
				(save-window-excursion
				  (call-interactively
				   (function
				    (lambda (dir)
				      (interactive "DDirectory: ")
				      dir)))))
	    (save-window-excursion
	      (call-interactively
	       (function
		(lambda (file)
		  (interactive "FFilename: ")
		  (expand-file-name file)))))))
	 )
    (if (file-exists-p filename)
        (or (yes-or-no-p (format "File %s exists. Save anyway? " filename))
            (error "")))
    (re-search-forward "\n\n")
    (mime-write-decoded-region (match-end 0) end filename encoding)
    ))


;;; @ mail/news message
;;;

(defun mime-preview-quitting-method-for-mime-show-message-mode ()
  "Quitting method for mime-view.
It is registered to variable `mime-preview-quitting-method-alist'."
  (let ((mother mime-mother-buffer)
	(win-conf mime-preview-original-window-configuration)
	)
    (kill-buffer mime-raw-buffer)
    (mime-preview-kill-buffer)
    (set-window-configuration win-conf)
    (pop-to-buffer mother)
    ))

(defun mime-method-to-display-message/rfc822 (beg end cal)
  (let* ((cnum (mime-raw-point-to-entity-number beg))
	 (new-name (format "%s-%s" (buffer-name) cnum))
	 (mother mime-preview-buffer)
	 (representation-type
	  (cdr (or (assq major-mode mime-raw-representation-type-alist)
		   (assq t mime-raw-representation-type-alist))))
	 str)
    (setq str (buffer-substring beg end))
    (switch-to-buffer new-name)
    (erase-buffer)
    (insert str)
    (goto-char (point-min))
    (if (re-search-forward "^\n" nil t)
	(delete-region (point-min) (match-end 0))
      )
    (setq major-mode 'mime-show-message-mode)
    (setq mime-raw-representation-type representation-type)
    (mime-view-mode mother)
    ))


;;; @ message/partial
;;;

(defun mime-raw-write-region (start end filename)
  "Write current region into specified file.
When called from a program, takes three arguments:
START, END and FILENAME.  START and END are buffer positions.
It refer `mime-raw-buffer-coding-system-alist' to choose coding-system
to write."
  (let ((coding-system-for-write
	 (cdr
	  (or (assq major-mode mime-raw-buffer-coding-system-alist)
	      (assq t mime-raw-buffer-coding-system-alist)
	      ))))
    (write-region start end filename)
    ))

(defun mime-method-to-store-message/partial (beg end cal)
  (goto-char beg)
  (let* ((root-dir
	  (expand-file-name
	   (concat "m-prts-" (user-login-name)) mime-temp-directory))
	 (id (cdr (assoc "id" cal)))
	 (number (cdr (assoc "number" cal)))
	 (total (cdr (assoc "total" cal)))
	 file
	 (mother mime-preview-buffer)
         )
    (or (file-exists-p root-dir)
	(make-directory root-dir)
	)
    (setq id (replace-as-filename id))
    (setq root-dir (concat root-dir "/" id))
    (or (file-exists-p root-dir)
	(make-directory root-dir)
	)
    (setq file (concat root-dir "/FULL"))
    (if (file-exists-p file)
	(let ((full-buf (get-buffer-create "FULL"))
	      (pwin (or (get-buffer-window mother)
			(get-largest-window)))
	      )
	  (save-window-excursion
	    (set-buffer full-buf)
	    (erase-buffer)
	    (as-binary-input-file (insert-file-contents file))
	    (setq major-mode 'mime-show-message-mode)
	    (mime-view-mode mother)
	    )
	  (set-window-buffer pwin
			     (save-excursion
			       (set-buffer full-buf)
			       mime-preview-buffer))
	  (select-window pwin)
	  )
      (re-search-forward "^$")
      (goto-char (1+ (match-end 0)))
      (setq file (concat root-dir "/" number))
      (mime-raw-write-region (point) end file)
      (let ((total-file (concat root-dir "/CT")))
	(setq total
	      (if total
		  (progn
		    (or (file-exists-p total-file)
			(save-excursion
			  (set-buffer
			   (get-buffer-create mime-temp-buffer-name))
			  (erase-buffer)
			  (insert total)
			  (write-region (point-min)(point-max) total-file)
			  (kill-buffer (current-buffer))
			  ))
		    (string-to-number total)
		    )
		(and (file-exists-p total-file)
		     (save-excursion
		       (set-buffer (find-file-noselect total-file))
		       (prog1
			   (and (re-search-forward "[0-9]+" nil t)
				(string-to-number
				 (buffer-substring (match-beginning 0)
						   (match-end 0)))
				)
			 (kill-buffer (current-buffer))
			 )))
		)))
      (if (and total (> total 0))
	  (catch 'tag
	    (save-excursion
	      (set-buffer (get-buffer-create mime-temp-buffer-name))
	      (let ((full-buf (current-buffer)))
		(erase-buffer)
		(let ((i 1))
		  (while (<= i total)
		    (setq file (concat root-dir "/" (int-to-string i)))
		    (or (file-exists-p file)
			(throw 'tag nil)
			)
		    (as-binary-input-file (insert-file-contents file))
		    (goto-char (point-max))
		    (setq i (1+ i))
		    ))
		(as-binary-output-file
                 (write-region (point-min)(point-max)
                               (expand-file-name "FULL" root-dir)))
		(let ((i 1))
		  (while (<= i total)
		    (let ((file (format "%s/%d" root-dir i)))
		      (and (file-exists-p file)
			   (delete-file file)
			   ))
		    (setq i (1+ i))
		    ))
		(let ((file (expand-file-name "CT" root-dir)))
		  (and (file-exists-p file)
		       (delete-file file)
		       ))
		(save-window-excursion
		  (setq major-mode 'mime-show-message-mode)
		  (mime-view-mode mother)
		  )
		(let ((pwin (or (get-buffer-window mother)
				(get-largest-window)
				))
		      (pbuf (save-excursion
			      (set-buffer full-buf)
			      mime-preview-buffer)))
		  (set-window-buffer pwin pbuf)
		  (select-window pwin)
		  )))))
      )))


;;; @ message/external-body
;;;

(defvar mime-raw-dired-function
  (if (and (>= emacs-major-version 19) window-system)
      (function dired-other-frame)
    (function mime-raw-dired-function-for-one-frame)
    ))

(defun mime-raw-dired-function-for-one-frame (dir)
  (let ((win (or (get-buffer-window mime-preview-buffer)
		 (get-largest-window))))
    (select-window win)
    (dired dir)
    ))

(defun mime-method-to-display-message/external-ftp (beg end cal)
  (let* ((site (cdr (assoc "site" cal)))
	 (directory (cdr (assoc "directory" cal)))
	 (name (cdr (assoc "name" cal)))
	 ;;(mode (cdr (assoc "mode" cal)))
	 (pathname (concat "/anonymous@" site ":" directory))
	 )
    (message (concat "Accessing " (expand-file-name name pathname) "..."))
    (funcall mime-raw-dired-function pathname)
    (goto-char (point-min))
    (search-forward name)
    ))


;;; @ rot13-47
;;;

(defun mime-method-to-display-caesar (start end cal)
  "Internal method for mime-view to display ROT13-47-48 message."
  (let* ((entity (mime-raw-find-entity-from-point start))
	 (cnum (reverse (mime-entity-node-id entity)))
	 (new-name (format "%s-%s" (buffer-name) cnum))
	 (the-buf (current-buffer))
	 (mother mime-preview-buffer)
	 (charset (cdr (assoc "charset" cal)))
	 (encoding (cdr (assq 'encoding cal)))
	 (mode major-mode))
    (let ((pwin (or (get-buffer-window mother)
		    (get-largest-window)))
	  (buf (get-buffer-create new-name))
	  )
      (set-window-buffer pwin buf)
      (set-buffer buf)
      (select-window pwin)
      )
    (setq buffer-read-only nil)
    (erase-buffer)
    (mime-text-insert-decoded-body entity)
    (mule-caesar-region (point-min) (point-max))
    (set-buffer-modified-p nil)
    (set-buffer mother)
    (view-buffer new-name)
    ))


;;; @ end
;;;

(provide 'mime-play)

(let* ((file mime-acting-situation-examples-file)
       (buffer (get-buffer-create " *mime-example*")))
  (if (file-readable-p file)
      (unwind-protect
	  (save-excursion
	    (set-buffer buffer)
	    (erase-buffer)
	    (insert-file-contents file)
	    (eval-current-buffer)
	    ;; format check
	    (or (eq (car mime-acting-situation-examples) 'type)
		(setq mime-acting-situation-examples nil))
	    )
	(kill-buffer buffer))))

;;; mime-play.el ends here
