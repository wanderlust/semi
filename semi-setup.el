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

(require 'mime-def)
(require 'file-detect)


;;; @ for mime-view
;;;

(call-after-loaded
 'mime-view
 (function
  (lambda ()
    ;; for message/partial
    (require 'mime-partial)
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
  "*If it is non-nil, semi-setup sets uf to use mime-pgp.")

;; for PGP
(if mime-setup-enable-pgp
    (call-after-loaded 'mime-view
		       (function
			(lambda ()
			  (require 'mime-pgp)
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
