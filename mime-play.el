;;; mime-play.el --- Playback processing module for mime-view.el

;; Copyright (C) 1994,1995,1996,1997,1998,1999 Free Software Foundation, Inc.

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
  (condition-case nil
      (require 'bbdb)
    (error (defvar bbdb-buffer-name nil)))
  )

(defcustom mime-save-directory "~/"
  "*Name of the directory where MIME entity will be saved in.
If t, it means current directory."
  :group 'mime-view
  :type '(choice (const :tag "Current directory" t)
		 (directory)))

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
                  mime-view-version "\n\n")
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
  (let ((len (length mime-acting-situation-example-list))
	i ir ic j jr jc ret
	dest d-i d-j
	(max-sim 0) sim
	min-det-ret det-ret
	min-det-org det-org
	min-freq freq)
    (setq i 0
	  ir mime-acting-situation-example-list)
    (while (< i len)
      (setq ic (car ir)
	    j 0
	    jr mime-acting-situation-example-list)
      (while (< j len)
	(unless (= i j)
	  (setq jc (car jr))
	  (setq ret (mime-compare-situation-with-example (car ic)(car jc))
		sim (car ret)
		det-ret (+ (length (car ic))(length (car jc)))
		det-org (length (cdr ret))
		freq (+ (cdr ic)(cdr jc)))
	  (cond ((< max-sim sim)
		 (setq max-sim sim
		       min-det-ret det-ret
		       min-det-org det-org
		       min-freq freq
		       d-i i
		       d-j j
		       dest (cons (cdr ret) freq))
		 )
		((= max-sim sim)
		 (cond ((> min-det-ret det-ret)
			(setq min-det-ret det-ret
			      min-det-org det-org
			      min-freq freq
			      d-i i
			      d-j j
			      dest (cons (cdr ret) freq))
			)
		       ((= min-det-ret det-ret)
			(cond ((> min-det-org det-org)
			       (setq min-det-org det-org
				     min-freq freq
				     d-i i
				     d-j j
				     dest (cons (cdr ret) freq))
			       )
			      ((= min-det-org det-org)
			       (cond ((> min-freq freq)
				      (setq min-freq freq
					    d-i i
					    d-j j
					    dest (cons (cdr ret) freq))
				      ))
			       ))
			))
		 ))
	  )
	(setq jr (cdr jr)
	      j (1+ j)))
      (setq ir (cdr ir)
	    i (1+ i)))
    (if (> d-i d-j)
	(setq i d-i
	      d-i d-j
	      d-j i))
    (setq jr (nthcdr (1- d-j) mime-acting-situation-example-list))
    (setcdr jr (cddr jr))
    (if (= d-i 0)
	(setq mime-acting-situation-example-list
	      (cdr mime-acting-situation-example-list))
      (setq ir (nthcdr (1- d-i) mime-acting-situation-example-list))
      (setcdr ir (cddr ir))
      )
    (if (setq ir (assoc (car dest) mime-acting-situation-example-list))
	(setcdr ir (+ (cdr ir)(cdr dest)))
      (setq mime-acting-situation-example-list
	    (cons dest mime-acting-situation-example-list))
      )))


;;; @ content decoder
;;;

;;;###autoload
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
	(let ((situation
	       (get-text-property (point) 'mime-view-situation)))
	  (or mode
	      (setq mode "play"))
	  (setq situation 
		(if (assq 'mode situation)
		    (put-alist 'mode mode (copy-alist situation))
		  (cons (cons 'mode mode)
			situation)))
	  (if ignore-examples
	      (setq situation
		    (cons (cons 'ignore-examples ignore-examples)
			  situation)))
	  (mime-play-entity entity situation)
	  ))))

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

;;;###autoload
(defun mime-play-entity (entity &optional situation ignored-method)
  "Play entity specified by ENTITY.
It decodes the entity to call internal or external method.  The method
is selected from variable `mime-acting-condition'.  If MODE is
specified, play as it.  Default MODE is \"play\"."
  (let (method ret)
    (in-calist-package 'mime-view)
    (setq ret
	  (mime-delq-null-situation
	   (ctree-find-calist mime-acting-condition
			      (mime-entity-situation entity situation)
			      mime-view-find-every-acting-situation)
	   'method ignored-method))
    (or (assq 'ignore-examples situation)
	(if (cdr ret)
	    (let ((rest ret)
		  (max-score 0)
		  (max-escore 0)
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
				  (mime-type/subtype-string
				   (cdr (assq 'type situation))
				   (cdr (assq 'subtype situation))))
	   (if (y-or-n-p "Do you want to save current entity to disk?")
	       (mime-save-content entity situation))
	   ))
    ))


;;; @ external decoder
;;;

(defvar mime-mailcap-method-filename-alist nil)

(defun mime-activate-mailcap-method (entity situation)
  (let ((method (cdr (assoc 'method situation)))
	(name (mime-entity-safe-filename entity)))
    (setq name
	  (if (and name (not (string= name "")))
	      (expand-file-name name temporary-file-directory)
	    (make-temp-name
	     (expand-file-name "EMI" temporary-file-directory))
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
    ))

(defun mime-mailcap-method-sentinel (process event)
  (let ((file (cdr (assq process mime-mailcap-method-filename-alist))))
    (if (file-exists-p file)
	(delete-file file)
      ))
  (remove-alist 'mime-mailcap-method-filename-alist process)
  (message (format "%s %s" process event)))

(defvar mime-echo-window-is-shared-with-bbdb
  (module-installed-p 'bbdb)
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
  "Show mime-echo buffer to display MIME-playing information.
It returns the list of window, start and end positions of inserted text.
A window height of the buffer `mime-echo-buffer-name' will be determined
by `mime-echo-window-height' (its value or its return value) whenever
this function is called."
  (get-buffer-create mime-echo-buffer-name)
  (let ((the-win (selected-window))
	(win (get-buffer-window mime-echo-buffer-name))
	(height (if (functionp mime-echo-window-height)
		    (funcall mime-echo-window-height)
		  mime-echo-window-height))
	(window-min-height 1)
	start)
    (if win
	(progn
	  (select-window win)
	  (enlarge-window (max 0 (- height (window-height))))
	  )
      (unless (and mime-echo-window-is-shared-with-bbdb
		   (condition-case nil
		       (select-window
			(setq win (get-buffer-window bbdb-buffer-name))
			)
		     (error nil)))
	(select-window (get-buffer-window (or mime-preview-buffer
					      (current-buffer))))
	(setq win (split-window-vertically (- (window-height) height)))
	(set-window-buffer win mime-echo-buffer-name)
	(select-window win)
	))
    (goto-char (setq start (point-max)))
    (if forms
	(let ((buffer-read-only nil))
	  (insert (apply (function format) forms))
	  ))
    (prog1
	(list win start (point))
      (select-window the-win)
      )))


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
		    (or (mime-entity-read-field entity 'Content-Description)
			(mime-entity-read-field entity 'Subject))))
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
  (let ((name (or (mime-entity-safe-filename entity)
		  (format "%s" (mime-entity-media-type entity))))
	(dir (if (eq t mime-save-directory)
		 default-directory
	       mime-save-directory))
	filename)
    (setq filename (read-file-name
		    (concat "File name: (default "
			    (file-name-nondirectory name) ") ")
		    dir
		    (concat (file-name-as-directory dir)
			    (file-name-nondirectory name))))
    (if (file-directory-p filename)
	(setq filename (concat (file-name-as-directory filename)
			       (file-name-nondirectory name))))
    (if (file-exists-p filename)
	(or (yes-or-no-p (format "File %s exists. Save anyway? " filename))
	    (error "")))
    (mime-write-entity-content entity (expand-file-name filename))
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
    (let ((mdata (mime-entity-content entity))
	  (rest mime-magic-type-alist))
      (while (not (let ((cell (car rest)))
		    (if cell
			(if (string-match (car cell) mdata)
			    (setq type (nth 1 cell)
				  subtype (nth 2 cell))
			  )
		      t)))
	(setq rest (cdr rest))))
    (setq situation (del-alist 'method (copy-alist situation)))
    (mime-play-entity entity
		      (if type
			  (put-alist 'type type
				     (put-alist 'subtype subtype
						situation))
			situation)
		      'mime-detect-content)))


;;; @ mail/news message
;;;

(defun mime-preview-quitting-method-for-mime-show-message-mode ()
  "Quitting method for mime-view.
It is registered to variable `mime-preview-quitting-method-alist'."
  (let ((raw-buffer (mime-entity-buffer
		     (get-text-property (point-min) 'mime-view-entity)))
	(mother mime-mother-buffer)
	(win-conf mime-preview-original-window-configuration))
    (kill-buffer raw-buffer)
    (mime-preview-kill-buffer)
    (set-window-configuration win-conf)
    (pop-to-buffer mother)
    ))

(defun mime-view-message/rfc822 (entity situation)
  (let* ((new-name
	  (format "%s-%s" (buffer-name) (mime-entity-number entity)))
	 (mother (current-buffer))
	 (children (car (mime-entity-children entity)))
	 (preview-buffer
	  (mime-display-message
	   children new-name mother nil
	   (cdr (assq 'major-mode
		      (get-text-property (point) 'mime-view-situation))))))
    (or (get-buffer-window preview-buffer)
	(let ((m-win (get-buffer-window mother)))
	  (if m-win
	      (set-window-buffer m-win preview-buffer)
	    (switch-to-buffer preview-buffer)
	    )))))


;;; @ message/partial
;;;

(defun mime-store-message/partial-piece (entity cal)
  (let* ((root-dir
	  (expand-file-name
	   (concat "m-prts-" (user-login-name)) temporary-file-directory))
	 (id (cdr (assoc "id" cal)))
	 (number (cdr (assoc "number" cal)))
	 (total (cdr (assoc "total" cal)))
	 file
	 (mother (current-buffer)))
    (or (file-exists-p root-dir)
	(make-directory root-dir))
    (setq id (replace-as-filename id))
    (setq root-dir (concat root-dir "/" id))
    (or (file-exists-p root-dir)
	(make-directory root-dir))
    (setq file (concat root-dir "/FULL"))
    (if (file-exists-p file)
	(let ((full-buf (get-buffer-create "FULL"))
	      (pwin (or (get-buffer-window mother)
			(get-largest-window)))
	      pbuf)
	  (save-window-excursion
	    (set-buffer full-buf)
	    (erase-buffer)
	    (as-binary-input-file (insert-file-contents file))
	    (setq major-mode 'mime-show-message-mode)
	    (mime-view-buffer (current-buffer) nil mother)
	    (setq pbuf (current-buffer))
	    )
	  (set-window-buffer pwin pbuf)
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
      (if (and total (> total 0)
	       (>= (length (directory-files root-dir nil "^[0-9]+$" t))
		   total))
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
		(let ((pwin (or (get-buffer-window mother)
				(get-largest-window)))
		      (pbuf (mime-display-message
			     (mime-open-entity 'buffer (current-buffer))
			     nil mother nil 'mime-show-message-mode)))
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

(defvar mime-raw-browse-url-function mime-browse-url-function)

(defun mime-view-message/external-url (entity cal)
  (let ((url (cdr (assoc "url" cal))))
    (message (concat "Accessing " url " ..."))
    (funcall mime-raw-browse-url-function url)))


;;; @ rot13-47
;;;

(defun mime-view-caesar (entity situation)
  "Internal method for mime-view to display ROT13-47-48 message."
  (let ((buf (get-buffer-create
	      (format "%s-%s" (buffer-name) (mime-entity-number entity)))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (mime-insert-text-content entity)
      (mule-caesar-region (point-min) (point-max))
      (set-buffer-modified-p nil)
      )
    (let ((win (get-buffer-window (current-buffer))))
      (or (eq (selected-window) win)
	  (select-window (or win (get-largest-window)))
	  ))
    (view-buffer buf)
    (goto-char (point-min))
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
