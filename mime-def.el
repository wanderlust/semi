;;; mime-def.el --- definition module for SEMI

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

(require 'custom)

(defgroup mime nil
  "Emacs MIME Interfaces"
  :group 'news
  :group 'mail)

(custom-handle-keyword 'default-mime-charset :group 'mime
		       'custom-variable)

(unless (fboundp 'butlast)
  (defun butlast (x &optional n)
    "Returns a copy of LIST with the last N elements removed."
    (if (and n (<= n 0)) x
      (nbutlast (copy-sequence x) n)))
  
  (defun nbutlast (x &optional n)
    "Modifies LIST to remove the last N elements."
    (let ((m (length x)))
      (or n (setq n 1))
      (and (< n m)
	   (progn
	     (if (> n 0) (setcdr (nthcdr (- (1- m) n) x) nil))
	     x))))
  )


;;; @ definitions about MIME
;;;

(defconst mime-tspecials "][()<>@,\;:\\\"/?=")
(defconst mime-token-regexp (concat "[^" mime-tspecials "\000-\040]+"))
(defconst mime-charset-regexp mime-token-regexp)

(defconst mime-media-type/subtype-regexp
  (concat mime-token-regexp "/" mime-token-regexp))


;;; @ end
;;;

(provide 'mime-def)

;;; mime-def.el ends here
