;;; semi-setup.el --- setup file for MIME-View.

;; Copyright (C) 1994,1995,1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id$
;; Keywords: mail, news, MIME, multimedia, multilingual, encoded-word

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

(eval-when-compile
  (require 'mime-view)
  )


;;; @ Utility
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


;;; @ for mime-view
;;;

(call-after-loaded
 'mime-view
 (function
  (lambda ()
    ;; for message/partial
    (require 'mime-partial)
    
    ;; for anonymous ftp
    (set-atype 'mime/content-decoding-condition
	       '((type . "message/external-body")
		 ("access-type" . "anon-ftp")
		 (method . mime-article/decode-message/external-ftp)
		 ))
    (autoload 'mime-article/decode-message/external-ftp "tm-ftp")
    
    ;; for LaTeX
    (set-atype 'mime/content-decoding-condition
	       '((type . "text/x-latex")
		 (method . mime/decode-text/latex)
		 ))
    (set-atype 'mime/content-decoding-condition
	       '((type . "application/x-latex")
		 (method . mime/decode-text/latex)
		 ))
    ;;(set-atype 'mime/content-decoding-condition
    ;; 	'((type . "application/octet-stream")
    ;;		  ("type" . "latex")
    ;;		  (method . mime/decode-text/latex)
    ;;		  ))
    (autoload 'mime/decode-text/latex "tm-latex")
    )))


(defvar running-xemacs (string-match "XEmacs" emacs-version))

;; for image/* and X-Face
(defvar mime-setup-enable-inline-image
  (and window-system
       (or running-xemacs
	   (and (featurep 'mule)(module-installed-p 'bitmap))
	   ))
  "*If it is non-nil, semi-setup sets up to use mime-image.")

(if mime-setup-enable-inline-image
    (call-after-loaded 'mime-view
		       (function
			(lambda ()
			  (require 'mime-image)
			  )))
  )


(defvar mime-setup-enable-pgp
  (module-installed-p 'mailcrypt)
  "*If it is non-nil, semi-setup sets uf to use tm-pgp.")

;; for PGP
(if mime-setup-enable-pgp
    (call-after-loaded 'mime-view
		       (function
			(lambda ()
			  (require 'tm-pgp)
			  )))
  )


;;; @ for mh-e
;;;

(defun semi-setup-load-emh ()
  (require 'emh)
  )

(call-after-loaded 'mh-e 'semi-setup-load-emh 'mh-folder-mode-hook)
(or (featurep 'mh-e)
    (add-hook 'mh-letter-mode-hook 'semi-setup-load-emh)
    )


;;; @ for Gnus
;;;
  
(defun semi-setup-load-gnus ()
  (let (gnus-load-hook)
    (require 'gnus-mime)
    ))

(add-hook 'gnus-load-hook 'semi-setup-load-gnus)


;;; @ end
;;;

(provide 'semi-setup)

;;; semi-setup.el ends here
