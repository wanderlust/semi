;;; mime-tar.el --- mime-view internal method for tar or tar+gzip format

;; Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.

;; Author: Hiroshi Ueno <zodiac@ibm.net>
;;	modified by MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Renamed: 1997/2/26 from tm-tar.el
;; Version: $Id$
;; Keywords: tar, tar+gzip, MIME, multimedia, mail, news

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

;;; Commentary:

;; Internal viewer for
;;    - application/x-tar
;;    - application/x-gzip, type="tar"
;;    - aplication/octet-stream, type="tar"
;;    - aplication/octet-stream, type="tar+gzip"

;;; Code:

(require 'mime-view)


;;; @ constants
;;;

(defconst mime-tar-list-buffer "*mime-tar-List*")
(defconst mime-tar-view-buffer "*mime-tar-View*")
(defconst mime-tar-file-search-regexp "[0-9]+\:[0-9\:]+[ ]+[0-9]+[ ]+")
(defconst mime-tar-popup-menu-title "Action Menu")


;;; @ variables
;;;

(defvar mime-tar-program "gtar")
(defvar mime-tar-decompress-arg '("-z"))
(defvar mime-tar-gzip-program "gzip")
(defvar mime-tar-mmencode-program "mmencode")
(defvar mime-tar-uudecode-program "uudecode")

(defvar mime-tar-popup-menu-items
  '(("View File"		. mime-tar-view-file)
    ("Key Help"			. mime-tar-helpful-message)
    ("Quit mime-tar Mode"	. exit-recursive-edit)
    ))

(cond ((string-match "XEmacs\\|Lucid" emacs-version)
       (defvar mime-tar-popup-menu
	 (cons mime-tar-popup-menu-title
	       (mapcar (function
			(lambda (item)
			  (vector (car item)(cdr item) t)
			  ))
		       mime-tar-popup-menu-items)))
       
       (defun mime-tar-mouse-button-2 (event)
	 (popup-menu mime-tar-popup-menu)
	 )
       )
      ((>= emacs-major-version 19)
       (defun mime-tar-mouse-button-2 (event)
	 (let ((menu
		(cons mime-tar-popup-menu-title
		      (list (cons "Menu Items" mime-tar-popup-menu-items))
		      )))
	   (let ((func (x-popup-menu event menu)))
	     (if func
		 (funcall func)
	       ))
	   ))
       ))

(defvar mime-tar-mode-map nil)
(if mime-tar-mode-map
    nil
  (setq mime-tar-mode-map (make-keymap))
  (suppress-keymap mime-tar-mode-map)
  (define-key mime-tar-mode-map "\C-c"    'exit-recursive-edit)
  (define-key mime-tar-mode-map "q"       'exit-recursive-edit)
  (define-key mime-tar-mode-map "n"       'mime-tar-next-line)
  (define-key mime-tar-mode-map " "       'mime-tar-next-line)
  (define-key mime-tar-mode-map "\C-m"    'mime-tar-next-line)
  (define-key mime-tar-mode-map "p"       'mime-tar-previous-line)
  (define-key mime-tar-mode-map "\177"    'mime-tar-previous-line)
  (define-key mime-tar-mode-map "\C-\M-m" 'mime-tar-previous-line)
  (define-key mime-tar-mode-map "v"       'mime-tar-view-file)
  (define-key mime-tar-mode-map "\C-h"    'Helper-help)
  (define-key mime-tar-mode-map "?"       'mime-tar-helpful-message)
  (if mouse-button-2
      (define-key mime-tar-mode-map
	mouse-button-2 'mime-button-dispatcher))
  )


;;; @@ mime-tar mode functions
;;;

(defun mime-tar-mode (&optional prev-buf)
  "Major mode for listing the contents of a tar archive file."
  (unwind-protect
      (let ((buffer-read-only t)
	    (mode-name "mime-tar")
	    (mode-line-buffer-identification '("%17b"))
	    )
	(goto-char (point-min))
	(mime-tar-move-to-filename)
	(catch 'mime-tar-mode (mime-tar-command-loop))
	)
    (if prev-buf
	(switch-to-buffer prev-buf)
      )
    ))

(defun mime-tar-command-loop ()
  (let ((old-local-map (current-local-map)))
    (unwind-protect
	(progn
	  (use-local-map mime-tar-mode-map)
	  (mime-tar-helpful-message)
	  (recursive-edit)
	  )
      (save-excursion
	(use-local-map old-local-map)
	))
    ))

(defun mime-tar-next-line ()
  (interactive)
  (next-line 1)
  (mime-tar-move-to-filename)
  )

(defun mime-tar-previous-line ()
  (interactive)
  (previous-line 1)
  (mime-tar-move-to-filename)
  )

(defun mime-tar-view-file ()
  (interactive)
  (let ((name (mime-tar-get-filename))
	)
    (save-excursion
      (switch-to-buffer mime-tar-view-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (message "Reading a file from an archive. Please wait...")
      (apply 'call-process mime-tar-program
	     nil t nil (append mime-tar-view-args (list name)))
      (goto-char (point-min))
      )
    (view-buffer mime-tar-view-buffer)
    ))

(defun mime-tar-get-filename ()
  (let (eol)
    (save-excursion
      (end-of-line)
      (setq eol (point))
      (beginning-of-line)
      (save-excursion
	(if (re-search-forward "^d" eol t)
	    (error "Cannot view a directory"))
	)
      (if (re-search-forward mime-tar-file-search-regexp eol t)
	  (let ((beg (point)))
	    (skip-chars-forward "^ \n")
	    (buffer-substring beg (point))
	    )
	(error "No file on this line")
	))
    ))

(defun mime-tar-move-to-filename ()
  (let ((eol (progn (end-of-line) (point))))
    (beginning-of-line)
    (re-search-forward mime-tar-file-search-regexp eol t)
    ))

(defun mime-tar-set-properties ()
  (if mouse-button-2
      (let ((beg (point-min))
	    (end (point-max))
	    )
	(goto-char beg)
	(save-excursion
	  (while (re-search-forward mime-tar-file-search-regexp end t)
	    (mime-add-button (point)
			     (progn
			       (end-of-line)
			       (point))
			     'mime-tar-view-file)
	    ))
	)))

(defun mime-tar-helpful-message ()
  (interactive)
  (message "Type %s, %s, %s, %s, %s, %s."
	   (substitute-command-keys "\\[Helper-help] for help")
	   (substitute-command-keys "\\[mime-tar-helpful-message] for keys")
	   (substitute-command-keys "\\[mime-tar-next-line] to next")
	   (substitute-command-keys "\\[mime-tar-previous-line] to prev")
	   (substitute-command-keys "\\[mime-tar-view-file] to view")
	   (substitute-command-keys "\\[exit-recursive-edit] to quit")
	   ))

(defun mime-tar-y-or-n-p (prompt)
  (prog1
      (y-or-n-p prompt)
    (message "")
    ))

;;; @@ tar message decoder
;;

(defun mime-decode-message/tar (beg end cal)
  (if (mime-tar-y-or-n-p "Do you want to enter mime-tar mode? ")
      (let ((coding (cdr (assoc 'encoding cal)))
	    (cur-buf (current-buffer))
	    (mime-tar-file-name
	     (expand-file-name
	      (concat (make-temp-name
		       (expand-file-name "tm" mime-temp-directory)) ".tar")))
	    (mime-tar-tmp-file-name
	     (expand-file-name
	      (make-temp-name (expand-file-name "tm" mime-temp-directory))))
	    new-buf)
	(find-file mime-tar-tmp-file-name)
	(setq new-buf (current-buffer))
	(setq buffer-read-only nil)
	(erase-buffer)
	(save-excursion
	  (set-buffer cur-buf)
	  (goto-char beg)
	  (re-search-forward "^$")
	  (append-to-buffer new-buf (+ (match-end 0) 1) end)
	  )
	(if (member coding mime-view-uuencode-encoding-name-list)
	    (progn
	      (goto-char (point-min))
	      (if (re-search-forward "^begin [0-9]+ " nil t)
		  (progn
		    (kill-line)
		    (insert mime-tar-file-name)
		    )
		(progn
		  (set-buffer-modified-p nil)
		  (kill-buffer new-buf)
		  (error "uuencode file signature was not found")
		  ))))
	(save-buffer)
	(kill-buffer new-buf)
	(message "Listing the contents of an archive.  Please wait...")
	(cond ((string-equal coding "base64")
	       (call-process mime-tar-mmencode-program nil nil nil "-u"
			     "-o" mime-tar-file-name mime-tar-tmp-file-name)
	       )
	      ((string-equal coding "quoted-printable")
	       (call-process mime-tar-mmencode-program nil nil nil "-u" "-q"
			     "-o" mime-tar-file-name mime-tar-tmp-file-name)
	       )
	      ((member coding mime-view-uuencode-encoding-name-list)
	       (call-process mime-tar-uudecode-program nil nil nil
			     mime-tar-tmp-file-name)
	       )
	      (t
	       (copy-file mime-tar-tmp-file-name mime-tar-file-name t)
	       ))
	(delete-file mime-tar-tmp-file-name)
	(setq mime-tar-list-args (list "-tvf" mime-tar-file-name))
	(setq mime-tar-view-args (list "-xOf" mime-tar-file-name))
	(if (eq 0 (call-process mime-tar-gzip-program
				nil nil nil "-t" mime-tar-file-name))
	    (progn
	      (setq mime-tar-list-args
		    (append mime-tar-decompress-arg mime-tar-list-args))
	      (setq mime-tar-view-args
		    (append mime-tar-decompress-arg mime-tar-view-args))
	      ))
	(switch-to-buffer mime-tar-view-buffer)
	(switch-to-buffer mime-tar-list-buffer)
	(setq buffer-read-only nil)
	(erase-buffer)
	(apply 'call-process mime-tar-program
	       nil t nil mime-tar-list-args)
	(if mouse-button-2
	    (progn
	      (make-local-variable 'mime-button-mother-dispatcher)
	      (setq mime-button-mother-dispatcher 'mime-tar-mouse-button-2)
	      ))
	(mime-tar-set-properties)
	(mime-tar-mode mime-view-buffer)
	(kill-buffer mime-tar-view-buffer)
	(kill-buffer mime-tar-list-buffer)
	(delete-file mime-tar-file-name)
	)
    ))

;;; @@ program/buffer coding system
;;;

(cond ((boundp 'MULE)
       (define-program-coding-system mime-tar-view-buffer nil '*autoconv*)
       )
      ((boundp 'NEMACS)
       (define-program-kanji-code mime-tar-view-buffer nil 1)
       ))

;;; @@ message types to use mime-tar
;;;

(set-atype 'mime/content-decoding-condition
	   '((type . "application/octet-stream")
	     (method . mime-decode-message/tar)
	     (mode . "play") ("type" . "tar")
	     ))

(set-atype 'mime/content-decoding-condition
	   '((type . "application/octet-stream")
	     (method . mime-decode-message/tar)
	     (mode . "play") ("type" . "tar+gzip")
	     ))

(set-atype 'mime/content-decoding-condition
	   '((type . "application/x-gzip")
	     (method . mime-decode-message/tar)
	     (mode . "play") ("type" . "tar")
	     ))

(set-atype 'mime/content-decoding-condition
	   '((type . "application/x-tar")
	     (method . mime-decode-message/tar)
	     (mode . "play")
	     ))

;;; @ end
;;;

(provide 'mime-tar)

;;; mime-tar.el ends here
