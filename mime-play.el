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

(eval-when-compile
  (require 'mime-text)
  (condition-case nil
      (require 'bbdb)
    (error (defvar bbdb-buffer-name nil)))
  )

(defvar mime-acting-situation-example-list nil)

(defvar mime-acting-situation-example-list-max-size 16)

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
	  (pp `(setq mime-acting-situation-example-list
		     ',mime-acting-situation-example-list)
	      (current-buffer))
	  (insert "\n;;; "
                  (file-name-nondirectory file)
                  " ends here.\n")
          (save-buffer))
      (kill-buffer buffer))))

(add-hook 'kill-emacs-hook 'mime-save-acting-situation-examples)

(defun mime-reduce-acting-situation-examples ()
  (let* ((rest mime-acting-situation-example-list)
	 (min-example (car rest))
	 (min-score (cdr min-example)))
    (while rest
      (let* ((example (car rest))
	     (score (cdr example)))
	(cond ((< score min-score)
	       (setq min-score score
		     min-example example)
	       )
	      ((= score min-score)
	       (if (<= (length (car example))(length (car min-example)))
		   (setq min-example example)
		 ))
	      ))
      (setq rest (cdr rest)))
    (setq mime-acting-situation-example-list
	  (delq min-example mime-acting-situation-example-list))
    (setq min-example (car min-example))
    (let ((examples mime-acting-situation-example-list)
	  (max-score 0)
	  max-examples)
      (while examples
	(let* ((ret (mime-compare-situation-with-example min-example
							 (caar examples)))
	       (ret-score (car ret)))
	  (cond ((> ret-score max-score)
		 (setq max-score ret-score
		       max-examples (list (cdr ret)))
		 )
		((= ret-score max-score)
		 (setq max-examples (cons (cdr ret) max-examples))
		 )))
	(setq examples (cdr examples)))
      (while max-examples
	(let* ((example (car max-examples))
	       (cell (assoc example mime-acting-situation-example-list)))
	  (if cell
	      (setcdr cell (1+ (cdr cell)))
	    (setq mime-acting-situation-example-list
		  (cons (cons example 0)
			mime-acting-situation-example-list))
	    ))
	(setq max-examples (cdr max-examples))
	))))


;;; @ content decoder
;;;

(defvar mime-preview-after-decoded-position nil)

(defun mime-preview-play-current-entity (&optional ignore-examples mode)
  "Play current entity.
It decodes current entity to call internal or external method.  The
method is selected from variable `mime-acting-condition'.
If IGNORE-EXAMPLES (C-u prefix) is specified, this function ignores
`mime-acting-situation-example-list'.
If MODE is specified, play as it.  Default MODE is \"play\"."
  (interactive "P")
  (let ((entity (get-text-property (point) 'mime-view-entity)))
    (if entity
	(let ((the-buf (current-buffer))
	      (raw-buffer (mime-entity-buffer entity)))
	  (setq mime-preview-after-decoded-position (point))
	  (set-buffer raw-buffer)
	  (mime-raw-play-entity entity (or mode "play") nil ignore-examples)
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
			   (method . 4)
			   (major-mode . 5)
			   (disposition-type . 6)
			   ))
		  a-order b-order)
	      (if (symbolp a-t)
		  (let ((ret (assq a-t order)))
		    (if ret
			(setq a-order (cdr ret))
		      (setq a-order 7)
		      ))
		(setq a-order 8)
		)
	      (if (symbolp b-t)
		  (let ((ret (assq b-t order)))
		    (if ret
			(setq b-order (cdr ret))
		      (setq b-order 7)
		      ))
		(setq b-order 8)
		)
	      (if (= a-order b-order)
		  (string< (format "%s" a-t)(format "%s" b-t))
		(< a-order b-order))
	      )))
  )

(defsubst mime-delq-null-situation (situations field
					       &optional ignored-value)
  (let (dest)
    (while situations
      (let* ((situation (car situations))
	     (cell (assq field situation)))
	(if cell
	    (or (eq (cdr cell) ignored-value)
		(setq dest (cons situation dest))
		)))
      (setq situations (cdr situations)))
    dest))

(defun mime-compare-situation-with-example (situation example)
  (let ((example (copy-alist example))
	(match 0))
    (while situation
      (let* ((cell (car situation))
	     (key (car cell))
	     (ecell (assoc key example)))
	(when ecell
	  (if (equal cell ecell)
	      (setq match (1+ match))
	    (setq example (delq ecell example))
	    ))
	)
      (setq situation (cdr situation))
      )
    (cons match example)
    ))

(defun mime-raw-play-entity (entity &optional mode situation ignore-examples
				    ignored-method)
  "Play entity specified by ENTITY.
It decodes the entity to call internal or external method.  The method
is selected from variable `mime-acting-condition'.  If MODE is
specified, play as it.  Default MODE is \"play\"."
  (let (method ret)
    (or situation
	(setq situation (mime-entity-situation entity)))
    (if mode
	(setq situation (cons (cons 'mode mode) situation))
      )
    (if ignore-examples
	(or (assq 'ignore-examples situation)
	    (setq situation
		  (cons (cons 'ignore-examples ignore-examples) situation)))
      )
    (setq ret
	  (mime-delq-null-situation
	   (ctree-find-calist mime-acting-condition situation
			      mime-view-find-every-acting-situation)
	   'method ignored-method))
    (or ignore-examples
	(if (cdr ret)
	    (let ((rest ret)
		  (max-score 0)
		  max-escore
		  max-examples
		  max-situations)
	      (while rest
		(let ((situation (car rest))
		      (examples mime-acting-situation-example-list))
		  (while examples
		    (let* ((ret
			    (mime-compare-situation-with-example
			     situation (caar examples)))
			   (ret-score (car ret)))
		      (cond ((> ret-score max-score)
			     (setq max-score ret-score
				   max-escore (cdar examples)
				   max-examples (list (cdr ret))
				   max-situations (list situation))
			     )
			    ((= ret-score max-score)
			     (cond ((> (cdar examples) max-escore)
				    (setq max-escore (cdar examples)
					  max-examples (list (cdr ret))
					  max-situations (list situation))
				    )
				   ((= (cdar examples) max-escore)
				    (setq max-examples
					  (cons (cdr ret) max-examples))
				    (or (member situation max-situations)
					(setq max-situations
					      (cons situation max-situations)))
				    )))))
		    (setq examples (cdr examples))))
		(setq rest (cdr rest)))
	      (when max-situations
		(setq ret max-situations)
		(while max-examples
		  (let* ((example (car max-examples))
			 (cell
			  (assoc example mime-acting-situation-example-list)))
		    (if cell
			(setcdr cell (1+ (cdr cell)))
		      (setq mime-acting-situation-example-list
			    (cons (cons example 0)
				  mime-acting-situation-example-list))
		      ))
		  (setq max-examples (cdr max-examples))
		  )))))
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
	   (add-to-list 'mime-acting-situation-example-list (cons ret 0))
	   )
	  (t
	   (setq ret (car ret))
	   ))
    (setq method (cdr (assq 'method ret)))
    (cond ((and (symbolp method)
		(fboundp method))
	   (funcall method entity ret)
	   )
	  ((stringp method)
	   (mime-activate-mailcap-method entity ret)
	   )
          ;; ((and (listp method)(stringp (car method)))
          ;;  (mime-activate-external-method entity ret)
          ;;  )
	  (t
	   (mime-show-echo-buffer "No method are specified for %s\n"
				  (mime-entity-type/subtype entity))
	   ))
    ))


;;; @ external decoder
;;;

(defvar mime-mailcap-method-filename-alist nil)

(defun mime-activate-mailcap-method (entity situation)
  (save-excursion
    (save-restriction
      (let ((start (mime-entity-point-min entity))
	    (end (mime-entity-point-max entity)))
	(narrow-to-region start end)
	(goto-char start)
	(let ((method (cdr (assoc 'method situation)))
	      (name (mime-entity-safe-filename entity)))
	  (setq name
		(if (and name (not (string= name "")))
		    (expand-file-name name mime-temp-directory)
		  (make-temp-name
		   (expand-file-name "EMI" mime-temp-directory))
		  ))
          (mime-write-entity-content entity name)
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
	  )))))

(defun mime-mailcap-method-sentinel (process event)
  (let ((file (cdr (assq process mime-mailcap-method-filename-alist))))
    (if (file-exists-p file)
	(delete-file file)
      ))
  (remove-alist 'mime-mailcap-method-filename-alist process)
  (message (format "%s %s" process event)))

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

(defun mime-entity-safe-filename (entity)
  (let ((filename
	 (or (mime-entity-filename entity)
	     (let ((subj
		    (or (mime-read-field 'Content-Description entity)
			(mime-read-field 'Subject entity))))
	       (if (and subj
			(or (string-match mime-view-file-name-regexp-1 subj)
			    (string-match mime-view-file-name-regexp-2 subj)))
		   (substring subj (match-beginning 0)(match-end 0))
		 )))))
    (if filename
	(replace-as-filename filename)
      )))


;;; @ file extraction
;;;

(defun mime-save-content (entity situation)
  (let* ((name (mime-entity-safe-filename entity))
	 (filename (if (and name (not (string-equal name "")))
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
    (mime-write-entity-content entity filename)
    ))


;;; @ file detection
;;;

(defvar mime-magic-type-alist
  '(("^\377\330\377[\340\356]..JFIF"	image jpeg)
    ("^\211PNG"				image png)
    ("^GIF8[79]"			image gif)
    ("^II\\*\000"			image tiff)
    ("^MM\000\\*"			image tiff)
    ("^MThd"				audio midi)
    ("^\000\000\001\263"		video mpeg)
    )
  "*Alist of regexp about magic-number vs. corresponding media-types.
Each element looks like (REGEXP TYPE SUBTYPE).
REGEXP is a regular expression to match against the beginning of the
content of entity.
TYPE is symbol to indicate primary type of media-type.
SUBTYPE is symbol to indicate subtype of media-type.")

(defun mime-detect-content (entity situation)
  (let (type subtype)
    (let ((mdata (save-excursion
		   ;;(set-buffer (mime-entity-buffer entity))
		   (let* ((start (mime-entity-body-start entity))
			  (end (progn
				 (goto-char start)
				 (end-of-line)
				 (point))))
		     (mime-decode-string (buffer-substring start end)
					 (mime-entity-encoding entity))
		     )))
	  (rest mime-magic-type-alist))
      (while (not (let ((cell (car rest)))
		    (if cell
			(if (string-match (car cell) mdata)
			    (setq type (nth 1 cell)
				  subtype (nth 2 cell))
			  )
		      t)))
	(setq rest (cdr rest))))
    (if type
	(mime-raw-play-entity
	 entity nil
	 (put-alist 'type type
		    (put-alist 'subtype subtype
			       (del-alist 'method
					  (copy-alist situation))))
	 (cdr (assq 'ignore-examples situation))
	 'mime-detect-content)
      ))
  )


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

(defun mime-view-message/rfc822 (entity situation)
  (let* ((new-name
	  (format "%s-%s" (buffer-name) (mime-entity-number entity)))
	 (mother mime-preview-buffer)
	 (children (car (mime-entity-children entity))))
    (set-buffer (get-buffer-create new-name))
    (erase-buffer)
    (insert-buffer-substring (mime-entity-buffer children)
			     (mime-entity-point-min children)
			     (mime-entity-point-max children))
    (setq mime-message-structure children)
    (setq major-mode 'mime-show-message-mode)
    (mime-view-buffer (current-buffer) nil mother
		      nil (if (mime-entity-cooked-p entity) 'cooked))
    ))


;;; @ message/partial
;;;

(defun mime-store-message/partial-piece (entity cal)
  (goto-char (mime-entity-point-min entity))
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
	    (mime-view-buffer (current-buffer) nil mother)
	    )
	  (set-window-buffer pwin
			     (save-excursion
			       (set-buffer full-buf)
			       mime-preview-buffer))
	  (select-window pwin)
	  )
      (setq file (concat root-dir "/" number))
      (mime-write-entity-body entity file)
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
		  (mime-view-buffer (current-buffer) nil mother)
		  )
		(let ((pwin (or (get-buffer-window mother)
				(get-largest-window)))
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

(defun mime-view-message/external-anon-ftp (entity cal)
  (let* ((site (cdr (assoc "site" cal)))
	 (directory (cdr (assoc "directory" cal)))
	 (name (cdr (assoc "name" cal)))
	 (pathname (concat "/anonymous@" site ":" directory)))
    (message (concat "Accessing " (expand-file-name name pathname) " ..."))
    (funcall mime-raw-dired-function pathname)
    (goto-char (point-min))
    (search-forward name)
    ))

(defvar mime-raw-browse-url-function (function mime-browse-url))

(defun mime-view-message/external-url (entity cal)
  (let ((url (cdr (assoc "url" cal))))
    (message (concat "Accessing " url " ..."))
    (funcall mime-raw-browse-url-function url)))


;;; @ rot13-47
;;;

(defun mime-view-caesar (entity situation)
  "Internal method for mime-view to display ROT13-47-48 message."
  (let* ((new-name (format "%s-%s" (buffer-name)
			   (mime-entity-number entity)))
	 (mother mime-preview-buffer))
    (let ((pwin (or (get-buffer-window mother)
		    (get-largest-window)))
	  (buf (get-buffer-create new-name)))
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
	    (eval-buffer)
	    ;; format check
	    (condition-case nil
		(let ((i 0))
		  (while (and (> (length mime-acting-situation-example-list)
				 mime-acting-situation-example-list-max-size)
			      (< i 16))
		    (mime-reduce-acting-situation-examples)
		    (setq i (1+ i))
		    ))
	      (error (setq mime-acting-situation-example-list nil)))
	    )
	(kill-buffer buffer))))

;;; mime-play.el ends here
