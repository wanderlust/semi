;;; mime-image.el --- mime-view filter to display images

;; Copyright (C) 1995,1996,1997,1998 MORIOKA Tomohiko
;; Copyright (C) 1996 Dan Rich

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         Dan Rich <drich@morpheus.corp.sgi.com>
;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1995/12/15
;;	Renamed: 1997/2/21 from tm-image.el

;; Keywords: image, picture, X-Face, MIME, multimedia, mail, news

;; This file is part of SEMI (Showy Emacs MIME Interfaces).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;	If you use this program with MULE, please install
;;	etl8x16-bitmap.bdf font included in tl package.

;;; Code:

(require 'mime-view)
(require 'alist)
(require 'path-util)

(cond 
 ((featurep 'xemacs) 

  (require 'images)

  (defun-maybe image-inline-p (format)
    (or (memq format image-native-formats)
	(find-if (function
		  (lambda (native)
		    (image-converter-chain format native)
		    ))
		 image-native-formats)
	))
       
  (image-register-netpbm-utilities)
  (image-register-converter 'pic 'ppm "pictoppm")
  (image-register-converter 'mag 'ppm "magtoppm")
       
  (defun image-insert-at-point (image)
    (let ((e (make-extent (point) (point))))
      (set-extent-end-glyph e (make-glyph image))))

  (defsubst-maybe image-invalid-glyph-p (glyph)
    (or (null (aref glyph 0))
	(null (aref glyph 2))
	(equal (aref glyph 2) "")
	))
  )
 ((featurep 'mule) 

  (eval-when-compile (require 'static))

  (eval-and-compile
    (autoload 'bitmap-insert-xbm-buffer "bitmap")
    )

  (static-if (fboundp 'image-type-available-p)
      (defalias-maybe 'image-inline-p 'image-type-available-p)
    (defvar image-native-formats '(xbm))
    (defun-maybe image-inline-p (format)
      (memq format image-native-formats)))

  (defun-maybe image-normalize (format data)
    (if (memq format '(xbm xpm))
	(list 'image ':type format ':data data)
      (let ((image-file 
	     (make-temp-name 
	      (expand-file-name "tm" temporary-file-directory))))
	(with-temp-buffer
	  (insert data)
	  (write-region-as-binary (point-min)(point-max) image-file))
	(list 'image ':type format ':file image-file)
	)))

  (defun image-insert-at-point (image)
    (static-if (fboundp 'insert-image)
	(unwind-protect
	    (save-excursion
	      (static-if (condition-case nil
			     (progn (insert-image '(image)) nil)
			   (wrong-number-of-arguments t))
		  (insert-image image "x")
		(insert-image image))
	      (insert "\n")
	      (save-window-excursion
		(set-window-buffer (selected-window)(current-buffer))
		(sit-for 0)))
	  (let ((file (plist-get (cdr image) ':file)))
	    (and file (file-exists-p file)
		 (delete-file file)
		 )))
      (when (eq (plist-get (cdr image) ':type) 'xbm)
	(save-restriction
	  (narrow-to-region (point)(point))
	  (insert (plist-get (cdr image) ':data))
	  (let ((mark (set-marker (make-marker) (point))))
	    (bitmap-insert-xbm-buffer (current-buffer))
	    (delete-region (point-min) mark))
	  ))))

  (defsubst-maybe image-invalid-glyph-p (glyph)
    (not (eq 'image (nth 0 glyph))))
  ))

;;
;; X-Face
;;

(cond
 ((module-installed-p 'highlight-headers)
  (eval-and-compile
    (autoload 'highlight-headers "highlight-headers"))
 
  (defun mime-preview-x-face-function-use-highlight-headers ()
    (highlight-headers (point-min) (re-search-forward "^$" nil t) t)
    )
  (add-hook 'mime-display-header-hook
	    'mime-preview-x-face-function-use-highlight-headers)
  )
 ((featurep 'mule)
  (require 'x-face-mule)
  (when (exec-installed-p uncompface-program exec-path)
    (add-hook 'mime-display-header-hook
	      'x-face-decode-message-header))
  ))

(defvar mime-image-format-alist
  '((image jpeg		jpeg)
    (image gif		gif)
    (image tiff		tiff)
    (image x-tiff	tiff)
    (image xbm		xbm)
    (image x-xbm	xbm)
    (image x-xpixmap	xpm)
    (image x-pic	pic)
    (image x-mag	mag)
    (image png		png)))

(dolist (rule mime-image-format-alist)
  (let ((type    (car rule))
	(subtype (nth 1 rule))
	(format  (nth 2 rule)))
    (when (image-inline-p format)
      (ctree-set-calist-strictly
       'mime-preview-condition
       (list (cons 'type type)(cons 'subtype subtype)
	     '(body . visible)
	     (cons 'body-presentation-method #'mime-display-image)
	     (cons 'image-format format))
       ))))


;;; @ content filter for images
;;;
;;    (for XEmacs 19.12 or later)

(defun mime-display-image (entity situation)
  (message "Decoding image...")
  (let ((image (image-normalize (cdr (assq 'image-format situation))
				(mime-entity-content entity))))
    (if (image-invalid-glyph-p image)
	(message "Invalid glyph!")
      (image-insert-at-point image)
      (message "Decoding image... done")))
  (insert "\n")
  )


;;; @ end
;;;

(provide 'mime-image)

;;; mime-image.el ends here
