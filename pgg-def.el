;;; pgg-def.el --- functions/macros for defining PGG functions

;; Copyright (C) 1999 Daiki Ueno

;; Author: Daiki Ueno <ueno@ueda.info.waseda.ac.jp>
;; Created: 1999/11/02
;; Keywords: PGP, OpenPGP, GnuPG

;; This file is part of SEMI (Secure Emacs MIME Interface).

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

(require 'pcustom)

(defgroup pgg ()
  "Glue for the various PGP implementations."
  :group 'mime)

(defcustom pgg-default-scheme 'gpg
  "Default PGP scheme"
  :group 'symbol
  :type 'string)

(defcustom pgg-default-user-id (user-login-name)
  "User ID of your default identity."
  :group 'pgg
  :type 'string)

(defcustom pgg-default-keyserver-address "pgp.nic.ad.jp"
  "Host name of keyserver."
  :group 'pgg
  :type 'string)

(defvar pgg-status-buffer " *PGG status*")
(defvar pgg-errors-buffer " *PGG errors*")
(defvar pgg-output-buffer " *PGG output*")

(defvar pgg-scheme nil
  "Current scheme of PGP implementation")

(defmacro pgg-truncate-key-identifier (key)
  `(if (> (length ,key) 8) (substring ,key 8) ,key))

(provide 'pgg-def)

;;; pgg-def.el ends here
