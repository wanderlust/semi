;;; mime-edit.el --- Simple MIME Composer for GNU Emacs

;; Copyright (C) 1993,1994,1995,1996,1997 Free Software Foundation, Inc.

;; Author: UMEDA Masanobu <umerin@mse.kyutech.ac.jp>
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1994/08/21 renamed from mime.el
;;	Renamed: 1997/2/21 from tm-edit.el
;; Version: $Revision$
;; Keywords: MIME, multimedia, multilingual, mail, news

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

;; This is an Emacs minor mode for editing Internet multimedia
;; messages formatted in MIME (RFC 2045, 2046, 2047, 2048 and 2049).
;; All messages in this mode are composed in the tagged MIME format,
;; that are described in the following examples.  The messages
;; composed in the tagged MIME format are automatically translated
;; into a MIME compliant message when exiting the mode.

;; Mule (a multilingual extension to Emacs 18 and 19) has a capability
;; of handling multilingual text in limited ISO-2022 manner that is
;; based on early experiences in Japanese Internet community and
;; resulted in RFC 1468 (ISO-2022-JP charset for MIME).  In order to
;; enable multilingual capability in single text message in MIME,
;; charset of multilingual text written in Mule is declared as either
;; `ISO-2022-JP-2' [RFC 1554].  Mule is required for reading the such
;; messages.

;; This MIME composer can work with Mail mode, mh-e letter Mode, and
;; News mode.  First of all, you need the following autoload
;; definition to load mime-edit-mode automatically:
;;
;; (autoload 'mime-edit-mode "mime-edit"
;;           "Minor mode for editing MIME message." t)
;;
;; In case of Mail mode (includes VM mode), you need the following
;; hook definition:
;;
;; (add-hook 'mail-mode-hook 'mime-edit-mode)
;; (add-hook 'mail-send-hook 'mime-edit-maybe-translate)
;;
;; In case of MH-E, you need the following hook definition:
;;
;; (add-hook 'mh-letter-mode-hook
;;           (function
;;            (lambda ()
;;              (mime-edit-mode)
;;              (make-local-variable 'mail-header-separator)
;;              (setq mail-header-separator "--------")
;;              ))))
;; (add-hook 'mh-before-send-letter-hook 'mime-edit-maybe-translate)
;;
;; In case of News mode, you need the following hook definition:
;;
;; (add-hook 'news-reply-mode-hook 'mime-edit-mode)
;; (add-hook 'news-inews-hook 'mime-edit-maybe-translate)
;;
;; In case of Emacs 19, it is possible to emphasize the message tags
;; using font-lock mode as follows:
;;
;; (add-hook 'mime-edit-mode-hook
;;           (function
;;            (lambda ()
;;              (font-lock-mode 1)
;;              (setq font-lock-keywords (list mime-edit-tag-regexp))
;;              ))))

;; The message tag looks like:
;;
;;	--[[TYPE/SUBTYPE;PARAMETERS][ENCODING]]
;;
;; The tagged MIME message examples:
;;
;; This is a conventional plain text.  It should be translated into
;; text/plain.
;; 
;;--[[text/plain]]
;; This is also a plain text.  But, it is explicitly specified as is.
;;--[[text/plain; charset=ISO-8859-1]]
;; This is also a plain text.  But charset is specified as iso-8859-1.
;;
;; ¡Hola!  Buenos días.  ¿Cómo está usted?
;;--[[text/enriched]]
;; <center>This is a richtext.</center>
;;
;;--[[image/gif][base64]]^M...image encoded in base64 comes here...
;;
;;--[[audio/basic][base64]]^M...audio encoded in base64 comes here...

;;; Code:

(require 'emu)
(require 'sendmail)
(require 'mail-utils)
(require 'mel)
(require 'mime-view)
(require 'eword-encode)
(require 'signature)
(require 'alist)


;;; @ version
;;;

(defconst mime-edit-RCS-ID
  "$Id$")

(defconst mime-edit-version (get-version-string mime-edit-RCS-ID))

(defconst mime-edit-version-name
  (concat "SEMI MIME-Edit " mime-edit-version))


;;; @ variables
;;;

(defvar mime-ignore-preceding-spaces nil
  "*Ignore preceding white spaces if non-nil.")

(defvar mime-ignore-trailing-spaces nil
  "*Ignore trailing white spaces if non-nil.")

(defvar mime-ignore-same-text-tag t
  "*Ignore preceding text content-type tag that is same with new one.
If non-nil, the text tag is not inserted unless something different.")

(defvar mime-auto-hide-body t
  "*Hide non-textual body encoded in base64 after insertion if non-nil.")

(defvar mime-edit-voice-recorder
  (function mime-edit-voice-recorder-for-sun)
  "*Function to record a voice message and encode it. [mime-edit.el]")

(defvar mime-edit-mode-hook nil
  "*Hook called when enter MIME mode.")

(defvar mime-edit-translate-hook nil
  "*Hook called before translating into a MIME compliant message.
To insert a signature file automatically, call the function
`mime-edit-insert-signature' from this hook.")

(defvar mime-edit-exit-hook nil
  "*Hook called when exit MIME mode.")

(defvar mime-content-types
  '(("text"
     ;; Charset parameter need not to be specified, since it is
     ;; defined automatically while translation.
     ("plain"
      ;;("charset" "" "ISO-2022-JP" "US-ASCII" "ISO-8859-1" "ISO-8859-8")
      )
     ("richtext"
      ;;("charset" "" "ISO-2022-JP" "US-ASCII" "ISO-8859-1" "ISO-8859-8")
      )
     ("enriched"
      ;;("charset" "" "ISO-2022-JP" "US-ASCII" "ISO-8859-1" "ISO-8859-8")
      )
     ("x-latex"
      ;;("charset" "" "ISO-2022-JP" "US-ASCII" "ISO-8859-1" "ISO-8859-8")
      )
     ("html"
      ;;("charset" "" "ISO-2022-JP" "US-ASCII" "ISO-8859-1" "ISO-8859-8")
      )
     ("x-rot13-47")
     )
    ("message"
     ("external-body"
      ("access-type"
       ("anon-ftp"
	("site" "ftp.jaist.ac.jp" "wnoc-fuk.wide.ad.jp" "nic.karrn.ad.jp")
	("directory" "/pub/GNU/elisp/mime")
	("name")
	("mode" "image" "ascii" "local8"))
       ("ftp"
	("site")
	("directory")
	("name")
	("mode" "image" "ascii" "local8"))
       ("tftp"        ("site") ("name"))
       ("afs"         ("site") ("name"))
       ("local-file"  ("site") ("name"))
       ("mail-server" ("server" "ftpmail@nic.karrn.ad.jp"))
       ))
     ("rfc822")
     )
    ("application"
     ("octet-stream" ("type" "" "tar" "shar"))
     ("postscript")
     ("x-kiss" ("x-cnf")))
    ("image"
     ("gif")
     ("jpeg")
     ("tiff")
     ("x-pic")
     ("x-mag")
     ("x-xwd")
     ("x-xbm")
     )
    ("audio" ("basic"))
    ("video" ("mpeg"))
    )
  "*Alist of content-type, subtype, parameters and its values.")

(defvar mime-file-types
  '(("\\.rtf$"
     "text"	"richtext"	nil
     nil
     nil		nil)
    ("\\.html$"
     "text"	"html"		nil
     nil
     nil		nil)
    ("\\.ps$"
     "application" "postscript"	nil
     "quoted-printable"
     "attachment"	(("filename" . file))
     )
    ("\\.jpg$"
     "image"	"jpeg"		nil
     "base64"
     "inline"		(("filename" . file))
     )
    ("\\.gif$"
     "image"	"gif"		nil
     "base64"
     "inline"		(("filename" . file))
     )
    ("\\.tiff$"
     "image"	"tiff"		nil
     "base64"
     "inline"		(("filename" . file))
     )
    ("\\.pic$"
     "image"	"x-pic"		nil
     "base64"
     "inline"		(("filename" . file))
     )
    ("\\.mag$"
     "image"	"x-mag"		nil
     "base64"
     "inline"		(("filename" . file))
     )
    ("\\.xbm$"
     "image"	"x-xbm"		nil
     "base64"
     "inline"		(("filename" . file))
     )
    ("\\.xwd$"
     "image"	"x-xwd"		nil
     "base64"
     "inline"		(("filename" . file))
     )
    ("\\.au$"
     "audio"	"basic"		nil
     "base64"
     "attachment"		(("filename" . file))
     )
    ("\\.mpg$"
     "video"	"mpeg"		nil
     "base64"
     "attachment"	(("filename" . file))
     )
    ("\\.el$"
     "application" "octet-stream" (("type" . "emacs-lisp"))
     "7bit"
     "attachment"	(("filename" . file))
     )
    ("\\.lsp$"
     "application" "octet-stream" (("type" . "common-lisp"))
     "7bit"
     "attachment"	(("filename" . file))
     )
    ("\\.tar\\.gz$"
     "application" "octet-stream" (("type" . "tar+gzip"))
     "base64"
     "attachment"	(("filename" . file))
     )
    ("\\.tgz$"
     "application" "octet-stream" (("type" . "tar+gzip"))
     "base64"
     "attachment"	(("filename" . file))
     )
    ("\\.tar\\.Z$"
     "application" "octet-stream" (("type" . "tar+compress"))
     "base64"
     "attachment"	(("filename" . file))
     )
    ("\\.taz$"
     "application" "octet-stream" (("type" . "tar+compress"))
     "base64"
     "attachment"	(("filename" . file))
     )
    ("\\.gz$"
     "application" "octet-stream" (("type" . "gzip"))
     "base64"
     "attachment"	(("filename" . file))
     )
    ("\\.Z$"
     "application" "octet-stream" (("type" . "compress"))
     "base64"
     "attachment"	(("filename" . file))
     )
    ("\\.lzh$"
     "application" "octet-stream" (("type" . "lha"))
     "base64"
     "attachment"	(("filename" . file))
     )
    ("\\.zip$"
     "application" "zip" nil
     "base64"
     "attachment"	(("filename" . file))
     )
    ("\\.diff$"
     "application" "octet-stream" (("type" . "patch"))
     nil
     "attachment"	(("filename" . file))
     )
    ("\\.patch$"
     "application" "octet-stream" (("type" . "patch"))
     nil
     "attachment"	(("filename" . file))
     )
    ("\\.signature"
     "text"	"plain"		nil	nil)
    (".*"
     "application" "octet-stream" nil
     nil
     "attachment"	(("filename" . file))
     )
    )
  "*Alist of file name, types, parameters, and default encoding.
If encoding is nil, it is determined from its contents.")


;;; @@ about charset, encoding and transfer-level
;;;

(defvar mime-charset-type-list
  '((us-ascii		7 nil)
    (iso-8859-1		8 "quoted-printable")
    (iso-8859-2		8 "quoted-printable")
    (iso-8859-3		8 "quoted-printable")
    (iso-8859-4		8 "quoted-printable")
    (iso-8859-5		8 "quoted-printable")
    (koi8-r		8 "quoted-printable")
    (iso-8859-7		8 "quoted-printable")
    (iso-8859-8		8 "quoted-printable")
    (iso-8859-9		8 "quoted-printable")
    (iso-2022-jp	7 "base64")
    (iso-2022-kr	7 "base64")
    (euc-kr		8 "base64")
    (gb2312		8 "quoted-printable")
    (big5		8 "base64")
    (iso-2022-jp-2	7 "base64")
    (iso-2022-int-1	7 "base64")
    ))

(defvar mime-transfer-level 7
  "*A number of network transfer level.  It should be bigger than 7.")
(make-variable-buffer-local 'mime-transfer-level)

(defsubst mime-encoding-name (transfer-level &optional not-omit)
  (cond ((> transfer-level 8) "binary")
	((= transfer-level 8) "8bit")
	(not-omit "7bit")
	))

(defvar mime-transfer-level-string
  (mime-encoding-name mime-transfer-level 'not-omit)
  "A string formatted version of mime-transfer-level")
(make-variable-buffer-local 'mime-transfer-level-string)

(defun mime-make-charset-default-encoding-alist (transfer-level)
  (mapcar (function
	   (lambda (charset-type)
	     (let ((charset  (car charset-type))
		   (type     (nth 1 charset-type))
		   (encoding (nth 2 charset-type))
		   )
	       (if (<= type transfer-level)
		   (cons charset (mime-encoding-name type))
		 (cons charset encoding)
		 ))))
	  mime-charset-type-list))

(defvar mime-edit-charset-default-encoding-alist
  (mime-make-charset-default-encoding-alist mime-transfer-level))
(make-variable-buffer-local 'mime-edit-charset-default-encoding-alist)


;;; @@ about message inserting
;;;

(defvar mime-edit-yank-ignored-field-list
  '("Received" "Approved" "Path" "Replied" "Status"
    "Xref" "X-UIDL" "X-Filter" "X-Gnus-.*" "X-VM-.*")
  "Delete these fields from original message when it is inserted
as message/rfc822 part.
Each elements are regexp of field-name. [mime-edit.el]")

(defvar mime-edit-yank-ignored-field-regexp
  (concat "^"
	  (apply (function regexp-or) mime-edit-yank-ignored-field-list)
	  ":"))

(defvar mime-edit-message-inserter-alist nil)
(defvar mime-edit-mail-inserter-alist nil)


;;; @@ about message splitting
;;;

(defvar mime-edit-split-message t
  "*Split large message if it is non-nil. [mime-edit.el]")

(defvar mime-edit-message-default-max-lines 1000
  "*Default maximum lines of a message. [mime-edit.el]")

(defvar mime-edit-message-max-lines-alist
  '((news-reply-mode . 500))
  "Alist of major-mode vs maximum lines of a message.
If it is not specified for a major-mode,
`mime-edit-message-default-max-lines' is used. [mime-edit.el]")

(defconst mime-edit-split-ignored-field-regexp
  "\\(^Content-\\|^Subject:\\|^Mime-Version:\\)")

(defvar mime-edit-split-blind-field-regexp
  "\\(^[BDFbdf]cc:\\|^cc:[ \t]*$\\)")

(defvar mime-edit-split-message-sender-alist nil)

(defvar mime-edit-news-reply-mode-server-running nil)


;;; @@ about PGP
;;;

(defvar mime-edit-signing-type 'pgp-elkins
  "*PGP signing type (pgp-elkins, pgp-kazu or nil). [mime-edit.el]")

(defvar mime-edit-encrypting-type 'pgp-elkins
  "*PGP encrypting type (pgp-elkins, pgp-kazu or nil). [mime-edit.el]")


;;; @@ about tag
;;;

(defconst mime-edit-single-part-tag-regexp
  "--[[][[]\\([^]]*\\)]\\([[]\\([^]]*\\)]\\|\\)]"
  "*Regexp of MIME tag in the form of [[CONTENT-TYPE][ENCODING]].")

(defconst mime-edit-quoted-single-part-tag-regexp
  (concat "- " (substring mime-edit-single-part-tag-regexp 1)))

(defconst mime-edit-multipart-beginning-regexp "--<<\\([^<>]+\\)>>-{\n")

(defconst mime-edit-multipart-end-regexp "--}-<<\\([^<>]+\\)>>\n")

(defconst mime-edit-beginning-tag-regexp
  (regexp-or mime-edit-single-part-tag-regexp
	     mime-edit-multipart-beginning-regexp))

(defconst mime-edit-end-tag-regexp
  (regexp-or mime-edit-single-part-tag-regexp
	     mime-edit-multipart-end-regexp))

(defconst mime-edit-tag-regexp
  (regexp-or mime-edit-single-part-tag-regexp
	     mime-edit-multipart-beginning-regexp
	     mime-edit-multipart-end-regexp))

(defvar mime-tag-format "--[[%s]]"
  "*Control-string making a MIME tag.")

(defvar mime-tag-format-with-encoding "--[[%s][%s]]"
  "*Control-string making a MIME tag with encoding.")


;;; @@ multipart boundary
;;;

(defvar mime-multipart-boundary "Multipart"
  "*Boundary of a multipart message.")


;;; @@ optional header fields
;;;

(defvar mime-edit-insert-x-emacs-field t
  "*If non-nil, insert X-Emacs header field.")

(defvar mime-edit-x-emacs-value
  (if running-xemacs
      (concat emacs-version
	      (if (featurep 'mule)
		  " with mule"
		" without mule"))
    (let ((ver (if (string-match "\\.[0-9]+$" emacs-version)
		   (substring emacs-version 0 (match-beginning 0))
		 emacs-version)))
      (if (featurep 'mule)
	  (concat "Emacs " ver ", MULE " mule-version)
	ver))))


;;; @ constants
;;;

(defconst mime-tspecials-regexp "[][()<>@,;:\\\"/?.= \t]"
  "*Specify MIME tspecials.
Tspecials means any character that matches with it in header must be quoted.")

(defconst mime-edit-mime-version-value
  (concat "1.0 (generated by " mime-edit-version-name ")")
  "MIME version number.")


;;; @ keymap and menu
;;;

(defvar mime-edit-mode-flag nil)
(make-variable-buffer-local 'mime-edit-mode-flag)

(defvar mime-edit-prefix "\C-c\C-x"
  "*Keymap prefix for MIME-Edit commands.")

(defvar mime-edit-map (make-sparse-keymap)
  "Keymap for MIME commands.")

(define-key mime-edit-map "\C-t"	'mime-edit-insert-text)
(define-key mime-edit-map "\C-i"	'mime-edit-insert-file)
(define-key mime-edit-map "\C-e"	'mime-edit-insert-external)
(define-key mime-edit-map "\C-v"	'mime-edit-insert-voice)
(define-key mime-edit-map "\C-y"	'mime-edit-insert-message)
(define-key mime-edit-map "\C-m"	'mime-edit-insert-mail)
(define-key mime-edit-map "\C-w"	'mime-edit-insert-signature)
(define-key mime-edit-map "\C-s"	'mime-edit-insert-signature)
(define-key mime-edit-map "\C-k"	'mime-edit-insert-key)
(define-key mime-edit-map "t"		'mime-edit-insert-tag)
(define-key mime-edit-map "a"		'mime-edit-enclose-alternative-region)
(define-key mime-edit-map "p"		'mime-edit-enclose-parallel-region)
(define-key mime-edit-map "m"		'mime-edit-enclose-mixed-region)
(define-key mime-edit-map "d"		'mime-edit-enclose-digest-region)
(define-key mime-edit-map "s"		'mime-edit-enclose-signed-region)
(define-key mime-edit-map "e"		'mime-edit-enclose-encrypted-region)
(define-key mime-edit-map "q"		'mime-edit-enclose-quote-region)
(define-key mime-edit-map "7"		'mime-edit-set-transfer-level-7bit)
(define-key mime-edit-map "8"		'mime-edit-set-transfer-level-8bit)
(define-key mime-edit-map "/"		'mime-edit-set-split)
(define-key mime-edit-map "v"		'mime-edit-set-sign)
(define-key mime-edit-map "h"		'mime-edit-set-encrypt)
(define-key mime-edit-map "\C-p"	'mime-edit-preview-message)
(define-key mime-edit-map "\C-z"	'mime-edit-exit)
(define-key mime-edit-map "?"		'mime-edit-help)

(defconst mime-edit-menu-title "MIME-Edit")

(defconst mime-edit-menu-list
  '((mime-help	"Describe MIME editor mode" mime-edit-help)
    (file	"Insert File"		mime-edit-insert-file)
    (external	"Insert External"	mime-edit-insert-external)
    (voice	"Insert Voice"		mime-edit-insert-voice)
    (message	"Insert Message"	mime-edit-insert-message)
    (mail	"Insert Mail"		mime-edit-insert-mail)
    (signature	"Insert Signature"	mime-edit-insert-signature)
    (text	"Insert Text"		mime-edit-insert-text)
    (tag	"Insert Tag"		mime-edit-insert-tag)
    (alternative "Enclose as alternative"
		 mime-edit-enclose-alternative-region)
    (parallel	"Enclose as parallel"	mime-edit-enclose-parallel-region)
    (mixed	"Enclose as serial"	mime-edit-enclose-mixed-region)
    (digest	"Enclose as digest"	mime-edit-enclose-digest-region)
    (signed	"Enclose as signed"	mime-edit-enclose-signed-region)
    (encrypted	"Enclose as encrypted"	mime-edit-enclose-encrypted-region)
    (quote	"Verbatim region"	mime-edit-enclose-quote-region)
    (key	"Insert Public Key"	mime-edit-insert-key)
    (split	"About split"           mime-edit-set-split)
    (sign	"About sign"		mime-edit-set-sign)
    (encrypt	"About encryption"	mime-edit-set-encrypt)
    (preview	"Preview Message"	mime-edit-preview-message)
    (level	"Toggle transfer-level"	mime-edit-toggle-transfer-level)
    )
  "MIME-edit menubar entry.")

(defvar mime-edit-mode-map (make-sparse-keymap)
  "Keymap for MIME-Edit mode commands.")
(define-key mime-edit-mode-map mime-edit-prefix mime-edit-map)

(cond (running-xemacs
       ;; modified by Pekka Marjola <pema@iki.fi>
       ;;	1995/9/5 (c.f. [tm-en:69])
       (defun mime-edit-define-menu-for-xemacs ()
	 "Define menu for Emacs 19."
	 (cond ((featurep 'menubar)
		(make-local-variable 'current-menubar)
		(set-buffer-menubar current-menubar)
		(add-submenu
		 nil
		 (cons mime-edit-menu-title
		       (mapcar (function
				(lambda (item)
				  (vector (nth 1 item)(nth 2 item)
					  mime-edit-mode-flag)
				  ))
			       mime-edit-menu-list)))
		)))
       
       ;; modified by Steven L. Baur <steve@miranova.com>
       ;;	1995/12/6 (c.f. [tm-en:209])
       (or (boundp 'mime-edit-popup-menu-for-xemacs)
	   (setq mime-edit-popup-menu-for-xemacs
		 (append '("MIME Commands" "---")
			 (mapcar (function (lambda (item)
					     (vector (nth 1 item)
						     (nth 2 item)
						     t)))
				 mime-edit-menu-list)))
	   )
       )
      ((>= emacs-major-version 19)
       (define-key mime-edit-mode-map [menu-bar mime-edit]
	 (cons mime-edit-menu-title
	       (make-sparse-keymap mime-edit-menu-title)))
       (mapcar (function
		(lambda (item)
		  (define-key mime-edit-mode-map
		    (vector 'menu-bar 'mime-edit (car item))
		    (cons (nth 1 item)(nth 2 item))
		    )
		  ))
	       (reverse mime-edit-menu-list)
	       )
       ))

(defun mime-edit-toggle-mode ()
  (interactive)
  (if mime-edit-mode-flag
      (mime-edit-exit 'nomime)
    (mime-edit-mode)
    ))

(cond (running-xemacs
       (add-minor-mode 'mime-edit-mode-flag
		       '((" MIME-Edit "  mime-transfer-level-string))
		       mime-edit-mode-map
		       nil
		       'mime-edit-toggle-mode)
       )
      (t
       (set-alist 'minor-mode-alist
		  'mime-edit-mode-flag
		  '((" MIME-Edit "  mime-transfer-level-string)))
       (set-alist 'minor-mode-map-alist
		  'mime-edit-mode-flag
		  mime-edit-mode-map)
       ))

;;(defvar mime-edit-mode-old-local-map nil) ; buffer local variable


;;; @ functions
;;;

;;;###autoload
(defun mime-edit-mode ()
  "MIME minor mode for editing the tagged MIME message.

In this mode, basically, the message is composed in the tagged MIME
format. The message tag looks like:

	--[[text/plain; charset=ISO-2022-JP][7bit]]

The tag specifies the MIME content type, subtype, optional parameters
and transfer encoding of the message following the tag. Messages
without any tag are treated as `text/plain' by default. Charset and
transfer encoding are automatically defined unless explicitly
specified. Binary messages such as audio and image are usually hidden.
The messages in the tagged MIME format are automatically translated
into a MIME compliant message when exiting this mode.

Available charsets depend on Emacs version being used. The following
lists the available charsets of each emacs.

EMACS 18:	US-ASCII is only available.
NEmacs:		US-ASCII and ISO-2022-JP are available.
EMACS 19:	US-ASCII and ISO-8859-1 (or other charset) are available.
XEmacs 19:	US-ASCII and ISO-8859-1 (or other charset) are available.
Mule:		US-ASCII, ISO-8859-* (except for ISO-8859-5), KOI8-R,
		ISO-2022-JP, ISO-2022-JP-2, ISO-2022-KR, BIG5 and
		ISO-2022-INT-1 are available.

ISO-2022-JP-2 and ISO-2022-INT-1 charsets used in mule is expected to
be used to represent multilingual text in intermixed manner. Any
languages that has no registered charset are represented as either
ISO-2022-JP-2 or ISO-2022-INT-1 in mule.

If you want to use non-ISO-8859-1 charset in EMACS 19 or XEmacs 19,
please set variable `default-mime-charset'. This variable must be
symbol of which name is a MIME charset.

If you want to add more charsets in mule, please set variable
`charsets-mime-charset-alist'. This variable must be alist of which
key is list of leading-char/charset and value is symbol of MIME
charset. (leading-char is a term of MULE 1.* and 2.*. charset is a
term of XEmacs/mule, mule merged EMACS and MULE 3.*) If name of
coding-system is different as MIME charset, please set variable
`mime-charset-coding-system-alist'. This variable must be alist of
which key is MIME charset and value is coding-system.

Following commands are available in addition to major mode commands:

\[make single part\]
\\[mime-edit-insert-text]	insert a text message.
\\[mime-edit-insert-file]	insert a (binary) file.
\\[mime-edit-insert-external]	insert a reference to external body.
\\[mime-edit-insert-voice]	insert a voice message.
\\[mime-edit-insert-message]	insert a mail or news message.
\\[mime-edit-insert-mail]	insert a mail message.
\\[mime-edit-insert-signature]	insert a signature file at end.
\\[mime-edit-insert-key]	insert PGP public key.
\\[mime-edit-insert-tag]	insert a new MIME tag.

\[make enclosure (maybe multipart)\]
\\[mime-edit-enclose-alternative-region]	enclose as multipart/alternative.
\\[mime-edit-enclose-parallel-region]	enclose as multipart/parallel.
\\[mime-edit-enclose-mixed-region]	enclose as multipart/mixed.
\\[mime-edit-enclose-digest-region]	enclose as multipart/digest.
\\[mime-edit-enclose-signed-region]	enclose as PGP signed.
\\[mime-edit-enclose-encrypted-region]	enclose as PGP encrypted.
\\[mime-edit-enclose-quote-region]	enclose as verbose mode (to avoid to expand tags)

\[other commands\]
\\[mime-edit-set-transfer-level-7bit]	set transfer-level as 7.
\\[mime-edit-set-transfer-level-8bit]	set transfer-level as 8.
\\[mime-edit-set-split]	set message splitting mode.
\\[mime-edit-set-sign]	set PGP-sign mode.
\\[mime-edit-set-encrypt]	set PGP-encryption mode.
\\[mime-edit-preview-message]	preview editing MIME message.
\\[mime-edit-exit]	exit and translate into a MIME compliant message.
\\[mime-edit-help]	show this help.
\\[mime-edit-maybe-translate]	exit and translate if in MIME mode, then split.

Additional commands are available in some major modes:
C-c C-c		exit, translate and run the original command.
C-c C-s		exit, translate and run the original command.

The following is a message example written in the tagged MIME format.
TABs at the beginning of the line are not a part of the message:

	This is a conventional plain text.  It should be translated
	into text/plain.
	--[[text/plain]]
	This is also a plain text.  But, it is explicitly specified as
	is.
	--[[text/plain; charset=ISO-8859-1]]
	This is also a plain text.  But charset is specified as
	iso-8859-1.

	¡Hola!  Buenos días.  ¿Cómo está usted?
	--[[text/enriched]]
	This is a <bold>enriched text</bold>.
	--[[image/gif][base64]]...image encoded in base64 here...
	--[[audio/basic][base64]]...audio encoded in base64 here...

User customizable variables (not documented all of them):
 mime-edit-prefix
    Specifies a key prefix for MIME minor mode commands.

 mime-ignore-preceding-spaces
    Preceding white spaces in a message body are ignored if non-nil.

 mime-ignore-trailing-spaces
    Trailing white spaces in a message body are ignored if non-nil.

 mime-auto-hide-body
    Hide a non-textual body message encoded in base64 after insertion
    if non-nil.

 mime-transfer-level
    A number of network transfer level.  It should be bigger than 7.
    If you are in 8bit-through environment, please set 8.

 mime-edit-voice-recorder
    Specifies a function to record a voice message and encode it.
    The function `mime-edit-voice-recorder-for-sun' is for Sun
    SparcStations.

 mime-edit-mode-hook
    Turning on MIME mode calls the value of mime-edit-mode-hook, if
    it is non-nil.

 mime-edit-translate-hook
    The value of mime-edit-translate-hook is called just before translating
    the tagged MIME format into a MIME compliant message if it is
    non-nil.  If the hook call the function mime-edit-insert-signature,
    the signature file will be inserted automatically.

 mime-edit-exit-hook
    Turning off MIME mode calls the value of mime-edit-exit-hook, if it is
    non-nil."
  (interactive)
  (if mime-edit-mode-flag
      (error "You are already editing a MIME message.")
    (setq mime-edit-mode-flag t)
    ;; Remember old key bindings.
    ;; (if running-xemacs
    ;;     (use-local-map (or (current-local-map) (make-sparse-keymap)))
    ;;   (make-local-variable 'mime-edit-mode-old-local-map)
    ;;   (setq mime-edit-mode-old-local-map (current-local-map))
    ;;   ;; Add MIME commands to current local map.
    ;;   (use-local-map (copy-keymap (or (current-local-map)
    ;;                                   (make-sparse-keymap))))
    ;;   )
    ;; (if (not (lookup-key (current-local-map) mime-edit-prefix))
    ;;     (define-key (current-local-map) mime-edit-prefix mime-edit-map))

    ;; Set transfer level into mode line
    ;;
    (setq mime-transfer-level-string
 	  (mime-encoding-name mime-transfer-level 'not-omit))
    (force-mode-line-update)
    
    ;; Define menu.  Menus for other emacs implementations are
    ;; welcome.
    (cond (running-xemacs
	   (mime-edit-define-menu-for-xemacs))
          ;; ((>= emacs-major-version 19)
          ;;  (mime-edit-define-menu-for-emacs19)
          ;;  )
	  )
    ;; end
    
    (enable-invisible)
    
    ;; I don't care about saving these.
    (setq paragraph-start
	  (regexp-or mime-edit-single-part-tag-regexp
		     paragraph-start))
    (setq paragraph-separate
	  (regexp-or mime-edit-single-part-tag-regexp
		     paragraph-separate))
    (run-hooks 'mime-edit-mode-hook)
    (message
     (substitute-command-keys
      "Type \\[mime-edit-exit] to exit MIME mode, and type \\[mime-edit-help] to get help."))
    ))

;;;###autoload
(defalias 'edit-mime 'mime-edit-mode)		; for convenience
(defalias 'mime-mode 'mime-edit-mode)		; for convenience

(defun mime-edit-exit (&optional nomime no-error)
  "Translate the tagged MIME message into a MIME compliant message.
With no argument encode a message in the buffer into MIME, otherwise
just return to previous mode."
  (interactive "P")
  (if (not mime-edit-mode-flag)
      (if (null no-error)
	  (error "You aren't editing a MIME message.")
	)
    (if (not nomime)
	(progn
	  (run-hooks 'mime-edit-translate-hook)
	  (mime-edit-translate-buffer)))
    ;; Restore previous state.
    (setq mime-edit-mode-flag nil)
    (cond (running-xemacs
	   (if (featurep 'menubar) 
	       (delete-menu-item (list mime-edit-menu-title))))
          ;; (t
          ;;  (use-local-map mime-edit-mode-old-local-map)
          ;;  )
	  )
    
    (end-of-invisible)
    (set-buffer-modified-p (buffer-modified-p))
    (run-hooks 'mime-edit-exit-hook)
    (message "Exit MIME editor mode.")
    ))

(defun mime-edit-maybe-translate ()
  (interactive)
  (mime-edit-exit nil t)
  (call-interactively 'mime-edit-maybe-split-and-send)
  )

(defun mime-edit-help ()
  "Show help message about MIME mode."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "MIME editor mode:\n")
    (princ (documentation 'mime-edit-mode))
    (print-help-return-message)))

(defun mime-edit-insert-text ()
  "Insert a text message.
Charset is automatically obtained from the `charsets-mime-charset-alist'."
  (interactive)
  (let ((ret (mime-edit-insert-tag "text" nil nil)))
  (if ret
      (progn
	(if (looking-at mime-edit-single-part-tag-regexp)
	    (progn
	      ;; Make a space between the following message.
	      (insert "\n")
	      (forward-char -1)
	      ))
	(if (and (member (second ret) '("enriched" "richtext"))
		 (fboundp 'enriched-mode)
		 )
	    (enriched-mode t)
	  (if (boundp 'enriched-mode)
	      (enriched-mode nil)
	    ))))))

(defun mime-edit-insert-file (file &optional verbose)
  "Insert a message from a file."
  (interactive "fInsert file as MIME message: \nP")
  (let*  ((guess (mime-find-file-type file))
	  (type (nth 0 guess))
	  (subtype (nth 1 guess))
	  (parameters (nth 2 guess))
	  (encoding (nth 3 guess))
	  (disposition-type (nth 4 guess))
	  (disposition-params (nth 5 guess))
	  )
    (if verbose
	(setq type    (mime-prompt-for-type type)
	      subtype (mime-prompt-for-subtype type subtype)
	      ))
    (if (or (interactive-p) verbose)
	(setq encoding (mime-prompt-for-encoding encoding))
      )
    (if (or (consp parameters) (stringp disposition-type))
	(let ((rest parameters) cell attribute value)
	  (setq parameters "")
	  (while rest
	    (setq cell (car rest))
	    (setq attribute (car cell))
	    (setq value (cdr cell))
	    (if (eq value 'file)
		(setq value (std11-wrap-as-quoted-string
			     (file-name-nondirectory file)))
	      )
	    (setq parameters (concat parameters "; " attribute "=" value))
	    (setq rest (cdr rest))
	    )
	  (if disposition-type
	      (progn
		(setq parameters
		      (concat parameters "\n"
			      "Content-Disposition: " disposition-type))
		(setq rest disposition-params)
		(while rest
		  (setq cell (car rest))
		  (setq attribute (car cell))
		  (setq value (cdr cell))
		  (if (eq value 'file)
		      (setq value (std11-wrap-as-quoted-string
				   (file-name-nondirectory file)))
		    )
		  (setq parameters
			(concat parameters "; " attribute "=" value))
		  (setq rest (cdr rest))
		  )
		))
	  ))
    (mime-edit-insert-tag type subtype parameters)
    (mime-edit-insert-binary-file file encoding)
    ))

(defun mime-edit-insert-external ()
  "Insert a reference to external body."
  (interactive)
  (mime-edit-insert-tag "message" "external-body" nil ";\n\t")
  ;;(forward-char -1)
  ;;(insert "Content-Description: " (read-string "Content-Description: ") "\n")
  ;;(forward-line 1)
  (let* ((pritype (mime-prompt-for-type))
	 (subtype (mime-prompt-for-subtype pritype))
	 (parameters (mime-prompt-for-parameters pritype subtype ";\n\t")))
    (and pritype
	 subtype
	 (insert "Content-Type: "
		 pritype "/" subtype (or parameters "") "\n")))
  (if (and (not (eobp))
	   (not (looking-at mime-edit-single-part-tag-regexp)))
      (insert (mime-make-text-tag) "\n")))

(defun mime-edit-insert-voice ()
  "Insert a voice message."
  (interactive)
  (let ((encoding
	 (completing-read
	  "What transfer encoding: "
	  mime-file-encoding-method-alist nil t nil)))
    (mime-edit-insert-tag "audio" "basic" nil)
    (mime-edit-define-encoding encoding)
    (save-restriction
      (narrow-to-region (1- (point))(point))
      (unwind-protect
	  (funcall mime-edit-voice-recorder encoding)
	(progn
	  (insert "\n")
	  (invisible-region (point-min)(point-max))
	  (goto-char (point-max))
	  )))))

(defun mime-edit-insert-signature (&optional arg)
  "Insert a signature file."
  (interactive "P")
  (let ((signature-insert-hook
         (function
          (lambda ()
            (apply (function mime-edit-insert-tag)
                   (mime-find-file-type signature-file-name))
            )))
        )
    (insert-signature arg)
    ))


;; Insert a new tag around a point.

(defun mime-edit-insert-tag (&optional pritype subtype parameters delimiter)
  "Insert new MIME tag and return a list of PRITYPE, SUBTYPE, and PARAMETERS.
If nothing is inserted, return nil."
  (interactive)
  (let ((p (point)))
    (mime-edit-goto-tag)
    (if (and (re-search-forward mime-edit-tag-regexp nil t)
	     (< (match-beginning 0) p)
	     (< p (match-end 0))
	     )
	(goto-char (match-beginning 0))
      (goto-char p)
      ))
  (let ((oldtag nil)
	(newtag nil)
	(current (point))
	)
    (setq pritype
	  (or pritype
	      (mime-prompt-for-type)))
    (setq subtype
	  (or subtype
	      (mime-prompt-for-subtype pritype)))
    (setq parameters
	  (or parameters
	      (mime-prompt-for-parameters pritype subtype delimiter)))
    ;; Make a new MIME tag.
    (setq newtag (mime-make-tag pritype subtype parameters))
    ;; Find an current MIME tag.
    (setq oldtag
	  (save-excursion
	    (if (mime-edit-goto-tag)
		(buffer-substring (match-beginning 0) (match-end 0))
	      ;; Assume content type is 'text/plan'.
	      (mime-make-tag "text" "plain")
	      )))
    ;; We are only interested in TEXT.
    (if (and oldtag
	     (not (mime-test-content-type
		   (mime-edit-get-contype oldtag) "text")))
	(setq oldtag nil))
    ;; Make a new tag.
    (if (or (not oldtag)		;Not text
	    (or mime-ignore-same-text-tag
		(not (string-equal oldtag newtag))))
	(progn
	  ;; Mark the beginning of the tag for convenience.
	  (push-mark (point) 'nomsg)
	  (insert newtag "\n")
	  (list pritype subtype parameters) ;New tag is created.
	  )
      ;; Restore previous point.
      (goto-char current)
      nil				;Nothing is created.
      )
    ))

(defun mime-edit-insert-binary-file (file &optional encoding)
  "Insert binary FILE at point.
Optional argument ENCODING specifies an encoding method such as base64."
  (let* ((tagend (1- (point)))		;End of the tag
	 (hide-p (and mime-auto-hide-body
		      (stringp encoding)
		      (not
		       (let ((en (downcase encoding)))
			 (or (string-equal en "7bit")
			     (string-equal en "8bit")
			     (string-equal en "binary")
			     )))))
	 )
    (save-restriction
      (narrow-to-region tagend (point))
      (mime-insert-encoded-file file encoding)
      (if hide-p
	  (progn
	    (invisible-region (point-min) (point-max))
	    (goto-char (point-max))
	    )
	(goto-char (point-max))
	))
    (or hide-p
	(looking-at mime-edit-tag-regexp)
	(= (point)(point-max))
	(mime-edit-insert-tag "text" "plain")
	)
    ;; Define encoding even if it is 7bit.
    (if (stringp encoding)
	(save-excursion
	  (goto-char tagend) ; Make sure which line the tag is on.
	  (mime-edit-define-encoding encoding)
	  ))
    ))


;; Commands work on a current message flagment.

(defun mime-edit-goto-tag ()
  "Search for the beginning of the tagged MIME message."
  (let ((current (point)) multipart)
    (if (looking-at mime-edit-tag-regexp)
	t
      ;; At first, go to the end.
      (cond ((re-search-forward mime-edit-beginning-tag-regexp nil t)
	     (goto-char (1- (match-beginning 0))) ;For multiline tag
	     )
	    (t
	     (goto-char (point-max))
	     ))
      ;; Then search for the beginning. 
      (re-search-backward mime-edit-end-tag-regexp nil t)
      (or (looking-at mime-edit-beginning-tag-regexp)
	  ;; Restore previous point.
	  (progn
	    (goto-char current)
	    nil
	    ))
      )))

(defun mime-edit-content-beginning ()
  "Return the point of the beginning of content."
  (save-excursion
    (let ((beg (save-excursion
		 (beginning-of-line) (point))))
      (if (mime-edit-goto-tag)
	  (let ((top (point)))
	    (goto-char (match-end 0))
	    (if (and (= beg top)
		     (= (following-char) ?\^M))
		(point)
	      (forward-line 1)
	      (point)))
	;; Default text/plain tag.
	(goto-char (point-min))
	(re-search-forward
	 (concat "\n" (regexp-quote mail-header-separator)
		 (if mime-ignore-preceding-spaces
		     "[ \t\n]*\n" "\n")) nil 'move)
	(point))
      )))

(defun mime-edit-content-end ()
  "Return the point of the end of content."
  (save-excursion
    (let ((beg (point)))
      (if (mime-edit-goto-tag)
	  (let ((top (point)))
	    (goto-char (match-end 0))
	    (if (invisible-p (point))
		(next-visible-point (point))
	      ;; Move to the end of this text.
	      (if (re-search-forward mime-edit-tag-regexp nil 'move)
		  ;; Don't forget a multiline tag.
		  (goto-char (match-beginning 0))
		)
	      (point)
	      ))
	;; Assume the message begins with text/plain.
	(goto-char (mime-edit-content-beginning))
	(if (re-search-forward mime-edit-tag-regexp nil 'move)
	    ;; Don't forget a multiline tag.
	    (goto-char (match-beginning 0)))
	(point))
      )))

(defun mime-edit-define-charset (charset)
  "Set charset of current tag to CHARSET."
  (save-excursion
    (if (mime-edit-goto-tag)
	(let ((tag (buffer-substring (match-beginning 0) (match-end 0))))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert
	   (mime-create-tag
	    (mime-edit-set-parameter
	     (mime-edit-get-contype tag)
	     "charset" (upcase (symbol-name charset)))
	    (mime-edit-get-encoding tag)))
	  ))))

(defun mime-edit-define-encoding (encoding)
  "Set encoding of current tag to ENCODING."
  (save-excursion
    (if (mime-edit-goto-tag)
	(let ((tag (buffer-substring (match-beginning 0) (match-end 0))))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert (mime-create-tag (mime-edit-get-contype tag) encoding)))
      )))

(defun mime-edit-choose-charset ()
  "Choose charset of a text following current point."
  (detect-mime-charset-region (point) (mime-edit-content-end))
  )

(defun mime-make-text-tag (&optional subtype)
  "Make a tag for a text after current point.
Subtype of text type can be specified by an optional argument SUBTYPE.
Otherwise, it is obtained from mime-content-types."
  (let* ((pritype "text")
	 (subtype (or subtype
		      (car (car (cdr (assoc pritype mime-content-types)))))))
    ;; Charset should be defined later.
    (mime-make-tag pritype subtype)))


;; Tag handling functions

(defun mime-make-tag (pritype subtype &optional parameters encoding)
  "Make a tag of MIME message of PRITYPE, SUBTYPE and optional PARAMETERS."
  (mime-create-tag (concat (or pritype "") "/" (or subtype "")
			   (or parameters ""))
		   encoding))

(defun mime-create-tag (contype &optional encoding)
  "Make a tag with CONTENT-TYPE and optional ENCODING."
  (format (if encoding mime-tag-format-with-encoding mime-tag-format)
	  contype encoding))

(defun mime-edit-get-contype (tag)
  "Return Content-Type (including parameters) of TAG."
  (and (stringp tag)
       (or (string-match mime-edit-single-part-tag-regexp tag)
	   (string-match mime-edit-multipart-beginning-regexp tag)
	   (string-match mime-edit-multipart-end-regexp tag)
	   )
       (substring tag (match-beginning 1) (match-end 1))
       ))

(defun mime-edit-get-encoding (tag)
  "Return encoding of TAG."
  (and (stringp tag)
       (string-match mime-edit-single-part-tag-regexp tag)
       (match-beginning 3)
       (not (= (match-beginning 3) (match-end 3)))
       (substring tag (match-beginning 3) (match-end 3))))

(defun mime-get-parameter (contype parameter)
  "For given CONTYPE return value for PARAMETER.
Nil if no such parameter."
  (if (string-match
       (concat
	";[ \t\n]*"
	(regexp-quote parameter)
	"[ \t\n]*=[ \t\n]*\\([^\" \t\n;]*\\|\"[^\"]*\"\\)\\([ \t\n]*;\\|$\\)")
       contype)
      (substring contype (match-beginning 1) (match-end 1))
    nil					;No such parameter
    ))

(defun mime-edit-set-parameter (contype parameter value)
  "For given CONTYPE set PARAMETER to VALUE."
  (let (ctype opt-fields)
    (if (string-match "\n[^ \t\n\r]+:" contype)
	(setq ctype (substring contype 0 (match-beginning 0))
	      opt-fields (substring contype (match-beginning 0)))
      (setq ctype contype)
      )
    (if (string-match
	 (concat
	  ";[ \t\n]*\\("
	  (regexp-quote parameter)
	  "[ \t\n]*=[ \t\n]*\\([^\" \t\n;]*\\|\"[^\"]*\"\\)\\)[ \t\n]*\\(;\\|$\\)")
	 ctype)
	;; Change value
	(concat (substring ctype 0 (match-beginning 1))
		parameter "=" value
		(substring contype (match-end 1))
		opt-fields)
      (concat ctype "; " parameter "=" value opt-fields)
      )))

(defun mime-strip-parameters (contype)
  "Return primary content-type and subtype without parameters for CONTYPE."
  (if (string-match "^[ \t]*\\([^; \t\n]*\\)" contype)
      (substring contype (match-beginning 1) (match-end 1)) nil))

(defun mime-test-content-type (contype type &optional subtype)
  "Test if CONTYPE is a TYPE and an optional SUBTYPE."
  (and (stringp contype)
       (stringp type)
       (string-match
	(concat "^[ \t]*" (downcase type) "/" (downcase (or subtype "")))
	(downcase contype))))


;; Basic functions

(defun mime-find-file-type (file)
  "Guess Content-Type, subtype, and parameters from FILE."
  (let ((guess nil)
	(guesses mime-file-types))
    (while (and (not guess) guesses)
      (if (string-match (car (car guesses)) file)
	  (setq guess (cdr (car guesses))))
      (setq guesses (cdr guesses)))
    guess
    ))

(defun mime-prompt-for-type (&optional default)
  "Ask for Content-type."
  (let ((type ""))
    ;; Repeat until primary content type is specified.
    (while (string-equal type "")
      (setq type
	    (completing-read "What content type: "
			     mime-content-types
			     nil
			     'require-match ;Type must be specified.
			     default
			     ))
      (if (string-equal type "")
	  (progn
	    (message "Content type is required.")
	    (beep)
	    (sit-for 1)
	    ))
      )
    type))

(defun mime-prompt-for-subtype (type &optional default)
  "Ask for subtype of media-type TYPE."
  (let ((subtypes (cdr (assoc type mime-content-types))))
    (or (and default
	     (assoc default subtypes))
	(setq default (car (car subtypes)))
	))
  (let* ((answer
	  (completing-read
	   (if default
	       (concat
		"What content subtype: (default " default ") ")
	     "What content subtype: ")
	   (cdr (assoc type mime-content-types))
	   nil
	   'require-match		;Subtype must be specified.
	   nil
	   )))
    (if (string-equal answer "") default answer)))

(defun mime-prompt-for-parameters (pritype subtype &optional delimiter)
  "Ask for Content-type parameters of Content-Type PRITYPE and SUBTYPE.
Optional DELIMITER specifies parameter delimiter (';' by default)."
  (let* ((delimiter (or delimiter "; "))
	 (parameters
	  (mapconcat
	   (function identity)
	   (delq nil
		 (mime-prompt-for-parameters-1
		  (cdr (assoc subtype
			      (cdr (assoc pritype mime-content-types))))))
	   delimiter
	   )))
    (if (and (stringp parameters)
	     (not (string-equal parameters "")))
	(concat delimiter parameters)
      ""				;"" if no parameters
      )))

(defun mime-prompt-for-parameters-1 (optlist)
  (apply (function append)
	 (mapcar (function mime-prompt-for-parameter) optlist)))

(defun mime-prompt-for-parameter (parameter)
  "Ask for PARAMETER.
Parameter must be '(PROMPT CHOICE1 (CHOISE2 ...))."
  (let* ((prompt (car parameter))
	 (choices (mapcar (function
			   (lambda (e)
			     (if (consp e) e (list e))))
			  (cdr parameter)))
	 (default (car (car choices)))
	 (answer nil))
    (if choices
	(progn
	  (setq answer
		(completing-read
		 (concat "What " prompt
			 ": (default "
			 (if (string-equal default "") "\"\"" default)
			 ") ")
		 choices nil nil ""))
	  ;; If nothing is selected, use default.
	  (if (string-equal answer "")
	      (setq answer default)))
      (setq answer
	    (read-string (concat "What " prompt ": "))))
    (cons (if (and answer
		   (not (string-equal answer "")))
	      (concat prompt "="
		      ;; Note: control characters ignored!
		      (if (string-match mime-tspecials-regexp answer)
			  (concat "\"" answer "\"") answer)))
	  (mime-prompt-for-parameters-1 (cdr (assoc answer (cdr parameter)))))
    ))

(defun mime-prompt-for-encoding (default)
  "Ask for Content-Transfer-Encoding. [mime-edit.el]"
  (let (encoding)
    (while (string=
	    (setq encoding
		  (completing-read
		   "What transfer encoding: "
		   mime-file-encoding-method-alist nil t default)
		  )
	    ""))
    encoding))


;;; @ Translate the tagged MIME messages into a MIME compliant message.
;;;

(defvar mime-edit-translate-buffer-hook
  '(mime-edit-pgp-enclose-buffer
    mime-edit-translate-body
    mime-edit-translate-header))

(defun mime-edit-translate-header ()
  "Encode the message header into network representation."
  (eword-encode-header 'code-conversion)
  (run-hooks 'mime-edit-translate-header-hook)
  )

(defun mime-edit-translate-buffer ()
  "Encode the tagged MIME message in current buffer in MIME compliant message."
  (interactive)
  (if (catch 'mime-edit-error
	(save-excursion
	  (run-hooks 'mime-edit-translate-buffer-hook)
	  ))
      (progn
	(undo)
	(error "Translation error!")
	)))

(defun mime-edit-find-inmost ()
  (goto-char (point-min))
  (if (re-search-forward mime-edit-multipart-beginning-regexp nil t)
      (let ((bb (match-beginning 0))
	    (be (match-end 0))
	    (type (buffer-substring (match-beginning 1)(match-end 1)))
	    end-exp eb ee)
	(setq end-exp (format "--}-<<%s>>\n" type))
	(widen)
	(if (re-search-forward end-exp nil t)
	    (progn
	      (setq eb (match-beginning 0))
	      (setq ee (match-end 0))
	      )
	  (setq eb (point-max))
	  (setq ee (point-max))
	  )
	(narrow-to-region be eb)
	(goto-char be)
	(if (re-search-forward mime-edit-multipart-beginning-regexp nil t)
	    (let (ret)
	      (narrow-to-region (match-beginning 0)(point-max))
	      (mime-edit-find-inmost)
	      )
	  (widen)
	  (list type bb be eb)
	  ))))

(defun mime-edit-process-multipart-1 (boundary)
  (let ((ret (mime-edit-find-inmost)))
    (if ret
	(let ((type (car ret))
	      (bb (nth 1 ret))(be (nth 2 ret))
	      (eb (nth 3 ret))
	      )
	  (narrow-to-region bb eb)
	  (delete-region bb be)
	  (setq bb (point-min))
	  (setq eb (point-max))
	  (widen)
	  (goto-char eb)
	  (if (looking-at mime-edit-multipart-end-regexp)
	      (let ((beg (match-beginning 0))
		    (end (match-end 0))
		    )
		(delete-region beg end)
		(or (looking-at mime-edit-beginning-tag-regexp)
		    (eobp)
		    (insert (concat (mime-make-text-tag) "\n"))
		    )))
	  (cond ((string-equal type "quote")
		 (mime-edit-enquote-region bb eb)
		 )
		((string-equal type "signed")
		 (cond ((eq mime-edit-signing-type 'pgp-elkins)
			(mime-edit-sign-pgp-elkins bb eb boundary)
			)
		       ((eq mime-edit-signing-type 'pgp-kazu)
			(mime-edit-sign-pgp-kazu bb eb boundary)
			))
		 )
		((string-equal type "encrypted")
		 (cond ((eq mime-edit-encrypting-type 'pgp-elkins)
			(mime-edit-encrypt-pgp-elkins bb eb boundary)
			)
		       ((eq mime-edit-encrypting-type 'pgp-kazu)
			(mime-edit-encrypt-pgp-kazu bb eb boundary)
			)))
		(t
		 (setq boundary
		       (nth 2 (mime-edit-translate-region bb eb
							    boundary t)))
		 (goto-char bb)
		 (insert
		  (format "--[[multipart/%s;
 boundary=\"%s\"][7bit]]\n"
			  type boundary))
		 ))
	  boundary))))

(defun mime-edit-enquote-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (while (re-search-forward mime-edit-single-part-tag-regexp nil t)
	(let ((tag (buffer-substring (match-beginning 0)(match-end 0))))
	  (replace-match (concat "- " (substring tag 1)))
	  )))))

(defun mime-edit-dequote-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (while (re-search-forward
	      mime-edit-quoted-single-part-tag-regexp nil t)
	(let ((tag (buffer-substring (match-beginning 0)(match-end 0))))
	  (replace-match (concat "-" (substring tag 2)))
	  )))))

(defun mime-edit-sign-pgp-elkins (beg end boundary)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let* ((ret
	      (mime-edit-translate-region beg end boundary))
	     (ctype    (car ret))
	     (encoding (nth 1 ret))
	     (parts    (nth 3 ret))
	     (pgp-boundary (concat "pgp-sign-" boundary))
	     )
	(goto-char beg)
	(insert (format "Content-Type: %s\n" ctype))
	(if encoding
	    (insert (format "Content-Transfer-Encoding: %s\n" encoding))
	  )
	(insert "\n")
	(or (funcall (pgp-function 'mime-sign)
		     (point-min)(point-max) nil nil pgp-boundary)
	    (throw 'mime-edit-error 'pgp-error)
	    )
	))))

(defvar mime-edit-encrypt-recipient-fields-list '("To" "cc"))

(defun mime-edit-make-encrypt-recipient-header ()
  (let* ((names mime-edit-encrypt-recipient-fields-list)
	 (values
	  (std11-field-bodies (cons "From" names)
			      nil mail-header-separator))
	 (from (prog1
		   (car values)
		 (setq values (cdr values))))
	 (header (and (stringp from)
		      (if (string-equal from "")
			  ""
			(format "From: %s\n" from)
			)))
	 recipients)
    (while (and names values)
      (let ((name (car names))
	    (value (car values))
	    )
	(and (stringp value)
	     (or (string-equal value "")
		 (progn
		   (setq header (concat header name ": " value "\n")
			 recipients (if recipients
					(concat recipients " ," value)
				      value))
		   ))))
      (setq names (cdr names)
	    values (cdr values))
      )
    (vector from recipients header)
    ))

(defun mime-edit-encrypt-pgp-elkins (beg end boundary)
  (save-excursion
    (save-restriction
      (let (from recipients header)
	(let ((ret (mime-edit-make-encrypt-recipient-header)))
	  (setq from (aref ret 0)
		recipients (aref ret 1)
		header (aref ret 2))
	  )
	(narrow-to-region beg end)
	(let* ((ret
		(mime-edit-translate-region beg end boundary))
	       (ctype    (car ret))
	       (encoding (nth 1 ret))
	       (parts    (nth 3 ret))
	       (pgp-boundary (concat "pgp-" boundary))
	       )
	  (goto-char beg)
	  (insert header)
	  (insert (format "Content-Type: %s\n" ctype))
	  (if encoding
	      (insert (format "Content-Transfer-Encoding: %s\n" encoding))
	    )
	  (insert "\n")
	  (or (funcall (pgp-function 'encrypt)
		       recipients (point-min) (point-max) from)
	      (throw 'mime-edit-error 'pgp-error)
	      )
	  (goto-char beg)
	  (insert (format "--[[multipart/encrypted;
 boundary=\"%s\";
 protocol=\"application/pgp-encrypted\"][7bit]]
--%s
Content-Type: application/pgp-encrypted

--%s
Content-Type: application/octet-stream
Content-Transfer-Encoding: 7bit

" pgp-boundary pgp-boundary pgp-boundary))
	  (goto-char (point-max))
	  (insert (format "\n--%s--\n" pgp-boundary))
	  )))))

(defun mime-edit-sign-pgp-kazu (beg end boundary)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let* ((ret
	      (mime-edit-translate-region beg end boundary))
	     (ctype    (car ret))
	     (encoding (nth 1 ret))
	     (parts    (nth 3 ret))
	     )
	(goto-char beg)
	(insert (format "Content-Type: %s\n" ctype))
	(if encoding
	    (insert (format "Content-Transfer-Encoding: %s\n" encoding))
	  )
	(insert "\n")
	(or (as-binary-process
	     (funcall (pgp-function 'traditional-sign)
		      beg (point-max)))
	    (throw 'mime-edit-error 'pgp-error)
	    )
	(goto-char beg)
	(insert
	 "--[[application/pgp; format=mime][7bit]]\n")
	))
    ))

(defun mime-edit-encrypt-pgp-kazu (beg end boundary)
  (save-excursion
    (let (from recipients header)
      (let ((ret (mime-edit-make-encrypt-recipient-header)))
	(setq from (aref ret 0)
	      recipients (aref ret 1)
	      header (aref ret 2))
	)
      (save-restriction
	(narrow-to-region beg end)
	(let* ((ret
		(mime-edit-translate-region beg end boundary))
	       (ctype    (car ret))
	       (encoding (nth 1 ret))
	       (parts    (nth 3 ret))
	       )
	  (goto-char beg)
	  (insert header)
	  (insert (format "Content-Type: %s\n" ctype))
	  (if encoding
	      (insert (format "Content-Transfer-Encoding: %s\n" encoding))
	    )
	  (insert "\n")
	  (or (as-binary-process
	       (funcall (pgp-function 'encrypt)
			recipients beg (point-max) nil 'maybe)
	       )
	      (throw 'mime-edit-error 'pgp-error)
	      )
	  (goto-char beg)
	  (insert
	   "--[[application/pgp; format=mime][7bit]]\n")
	  ))
      )))

(defsubst replace-space-with-underline (str)
  (mapconcat (function
	      (lambda (arg)
		(char-to-string
		 (if (eq arg ?\ )
		     ?_
		   arg)))) str "")
  )

(defun mime-edit-make-boundary ()
  (concat mime-multipart-boundary "_"
	  (replace-space-with-underline (current-time-string))
	  ))

(defun mime-edit-translate-body ()
  "Encode the tagged MIME body in current buffer in MIME compliant message."
  (interactive)
  (save-excursion
    (let ((boundary (mime-edit-make-boundary))
	  (i 1)
	  ret)
      (while (mime-edit-process-multipart-1
	      (format "%s-%d" boundary i))
	(setq i (1+ i))
	)
      (save-restriction
	;; We are interested in message body.
	(let* ((beg
		(progn
		  (goto-char (point-min))
		  (re-search-forward
		   (concat "\n" (regexp-quote mail-header-separator)
			   (if mime-ignore-preceding-spaces
			       "[ \t\n]*\n" "\n")) nil 'move)
		  (point)))
	       (end
		(progn
		  (goto-char (point-max))
		  (and mime-ignore-trailing-spaces
		       (re-search-backward "[^ \t\n]\n" beg t)
		       (forward-char 1))
		  (point))))
	  (setq ret (mime-edit-translate-region
		     beg end
		     (format "%s-%d" boundary i)))
	  ))
      (mime-edit-dequote-region (point-min)(point-max))
      (let ((contype (car ret))		;Content-Type
	    (encoding (nth 1 ret))	;Content-Transfer-Encoding
	    )
	;; Insert X-Emacs field
	(and mime-edit-insert-x-emacs-field
	     (or (mail-position-on-field "X-Emacs")
		 (insert mime-edit-x-emacs-value)
		 ))
	;; Make primary MIME headers.
	(or (mail-position-on-field "Mime-Version")
	    (insert mime-edit-mime-version-value))
	;; Remove old Content-Type and other fields.
	(save-restriction
	  (goto-char (point-min))
	  (search-forward (concat "\n" mail-header-separator "\n") nil t)
	  (narrow-to-region (point-min) (point))
	  (goto-char (point-min))
	  (mime-delete-field "Content-Type")
	  (mime-delete-field "Content-Transfer-Encoding"))
	;; Then, insert Content-Type and Content-Transfer-Encoding fields.
	(mail-position-on-field "Content-Type")
	(insert contype)
	(if encoding
	    (progn
	      (mail-position-on-field "Content-Transfer-Encoding")
	      (insert encoding)))
	))))

(defun mime-edit-translate-single-part-tag (&optional prefix)
  (if (re-search-forward mime-edit-single-part-tag-regexp nil t)
      (let* ((beg (match-beginning 0))
	     (end (match-end 0))
	     (tag (buffer-substring beg end))
	     )
	(delete-region beg end)
	(setq contype (mime-edit-get-contype tag))
	(setq encoding (mime-edit-get-encoding tag))
	(insert (concat prefix "--" boundary "\n"))
	(save-restriction
	  (narrow-to-region (point)(point))
	  (insert "Content-Type: " contype "\n")
	  (if encoding
	      (insert "Content-Transfer-Encoding: " encoding "\n"))
	  (eword-encode-header)
	  )
	t)))

(defun mime-edit-translate-region (beg end &optional boundary multipart)
  (or boundary
      (setq boundary (mime-edit-make-boundary))
      )
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((tag nil)			;MIME tag
	    (contype nil)		;Content-Type
	    (encoding nil)		;Content-Transfer-Encoding
	    (nparts 0))			;Number of body parts
	;; Normalize the body part by inserting appropriate message
	;; tags for every message contents.
	(mime-edit-normalize-body)
	;; Counting the number of Content-Type.
	(goto-char (point-min))
	(while (re-search-forward mime-edit-single-part-tag-regexp nil t)
	  (setq nparts (1+ nparts)))
	;; Begin translation.
	(cond
	 ((and (<= nparts 1)(not multipart))
	  ;; It's a singular message.
	  (goto-char (point-min))
	  (while (re-search-forward
		  mime-edit-single-part-tag-regexp nil t)
	    (setq tag
		  (buffer-substring (match-beginning 0) (match-end 0)))
	    (delete-region (match-beginning 0) (1+ (match-end 0)))
	    (setq contype (mime-edit-get-contype tag))
	    (setq encoding (mime-edit-get-encoding tag))
	    ))
	 (t
	  ;; It's a multipart message.
	  (goto-char (point-min))
	  (and (mime-edit-translate-single-part-tag)
	       (while (mime-edit-translate-single-part-tag "\n"))
	       )
	  ;; Define Content-Type as "multipart/mixed".
	  (setq contype
		(concat "multipart/mixed;\n boundary=\"" boundary "\""))
	  ;; Content-Transfer-Encoding must be "7bit".
	  ;; The following encoding can be `nil', but is
	  ;; specified as is since there is no way that a user
	  ;; specifies it.
	  (setq encoding "7bit")
	  ;; Insert the trailer.
	  (goto-char (point-max))
	  (insert "\n--" boundary "--\n")
	  ))
	(list contype encoding boundary nparts)
	))))

(defun mime-edit-normalize-body ()
  "Normalize the body part by inserting appropriate message tags."
  ;; Insert the first MIME tags if necessary.
  (goto-char (point-min))
  (if (not (looking-at mime-edit-single-part-tag-regexp))
      (insert (mime-make-text-tag) "\n"))
  ;; Check each tag, and add new tag or correct it if necessary.
  (goto-char (point-min))
  (while (re-search-forward mime-edit-single-part-tag-regexp nil t)
    (let* ((tag (buffer-substring (match-beginning 0) (match-end 0)))
	   (contype (mime-edit-get-contype tag))
	   (charset (mime-get-parameter contype "charset"))
	   (encoding (mime-edit-get-encoding tag)))
      ;; Remove extra whitespaces after the tag.
      (if (looking-at "[ \t]+$")
	  (delete-region (match-beginning 0) (match-end 0)))
      (let ((beg (point))
	    (end (mime-edit-content-end))
	    )
	(if (= end (point-max))
	    nil
	  (goto-char end)
	  (or (looking-at mime-edit-beginning-tag-regexp)
	      (eobp)
	      (insert (mime-make-text-tag) "\n")
	      ))
	(visible-region beg end)
	(goto-char beg)
	)
      (cond
       ((mime-test-content-type contype "message")
	;; Content-type "message" should be sent as is.
	(forward-line 1)
	)
       ((mime-test-content-type contype "text")
	;; Define charset for text if necessary.
	(setq charset (if charset
			  (intern (downcase charset))
			(mime-edit-choose-charset)))
	(mime-edit-define-charset charset)
	(cond ((string-equal contype "text/x-rot13-47")
	       (save-excursion
		 (forward-line)
		 (set-mark (point))
		 (goto-char (mime-edit-content-end))
		 (tm:caesar-region)
		 ))
	      ((string-equal contype "text/enriched")
	       (save-excursion
		 (let ((beg (progn
			      (forward-line)
			      (point)))
		       (end (mime-edit-content-end))
		       )
		   ;; Patch for hard newlines
                   ;; (save-excursion
                   ;;   (goto-char beg)
                   ;;   (while (search-forward "\n" end t)
                   ;;     (put-text-property (match-beginning 0)
                   ;;                        (point)
                   ;;                        'hard t)))
		   ;; End patch for hard newlines
		   (enriched-encode beg end)
		   (goto-char beg)
		   (if (search-forward "\n\n")
		       (delete-region beg (match-end 0))
		     )
		   ))))
	;; Point is now on current tag.
	;; Define encoding and encode text if necessary.
	(or encoding	;Encoding is not specified.
	    (let* ((encoding
		    (cdr
		     (assq charset
			   mime-edit-charset-default-encoding-alist)
		     ))
		   (beg (mime-edit-content-beginning))
		   )
	      (encode-mime-charset-region beg (mime-edit-content-end)
					  charset)
	      (mime-encode-region beg (mime-edit-content-end) encoding)
	      (mime-edit-define-encoding encoding)
	      ))
	(goto-char (mime-edit-content-end))
	)
       ((null encoding)		;Encoding is not specified.
	;; Application, image, audio, video, and any other
	;; unknown content-type without encoding should be
	;; encoded.
	(let* ((encoding "base64")	;Encode in BASE64 by default.
	       (beg (mime-edit-content-beginning))
	       (end (mime-edit-content-end))
	       (body (buffer-substring beg end))
	       )
	  (mime-encode-region beg end encoding)
	  (mime-edit-define-encoding encoding))
	(forward-line 1)
	))
      )))

(defun mime-delete-field (field)
  "Delete header FIELD."
  (let ((regexp (format "^%s:[ \t]*" field)))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (delete-region (match-beginning 0)
		     (progn (forward-line 1) (point)))
      )))


;;;
;;; Platform dependent functions
;;;

;; Sun implementations

(defun mime-edit-voice-recorder-for-sun (encoding)
  "Record voice in a buffer using Sun audio device,
and insert data encoded as ENCODING. [mime-edit.el]"
  (message "Start the recording on %s.  Type C-g to finish the recording..."
	   (system-name))
  (mime-insert-encoded-file "/dev/audio" encoding)
  )


;;; @ Other useful commands.
;;;

;; Message forwarding commands as content-type "message/rfc822".

(defun mime-edit-insert-message (&optional message)
  (interactive)
  (let ((inserter (cdr (assq major-mode mime-edit-message-inserter-alist))))
    (if (and inserter (fboundp inserter))
	(progn
	  (mime-edit-insert-tag "message" "rfc822")
	  (funcall inserter message)
	  )
      (message "Sorry, I don't have message inserter for your MUA.")
      )))

(defun mime-edit-insert-mail (&optional message)
  (interactive)
  (let ((inserter (cdr (assq major-mode mime-edit-mail-inserter-alist))))
    (if (and inserter (fboundp inserter))
	(progn
	  (mime-edit-insert-tag "message" "rfc822")
	  (funcall inserter message)
	  )
      (message "Sorry, I don't have mail inserter for your MUA.")
      )))

(defun mime-edit-inserted-message-filter ()
  (save-excursion
    (save-restriction
      (let ((header-start (point))
	    (case-fold-search t)
	    beg end)
	;; for Emacs 18
	;; (if (re-search-forward "^$" (marker-position (mark-marker)))
	(if (re-search-forward "^$" (mark t))
	    (narrow-to-region header-start (match-beginning 0))
	  )
	(goto-char header-start)
	(while (and (re-search-forward
		     mime-edit-yank-ignored-field-regexp nil t)
		    (setq beg (match-beginning 0))
		    (setq end (1+ (std11-field-end)))
		    )
	  (delete-region beg end)
	  )
	))))


;;; @ multipart enclosure
;;;

(defun mime-edit-enclose-region (type beg end)
  (save-excursion
    (goto-char beg)
    (let ((current (point)))
      (save-restriction
	(narrow-to-region beg end)
	(insert (format "--<<%s>>-{\n" type))
	(goto-char (point-max))
	(insert (format "--}-<<%s>>\n" type))
	(goto-char (point-max))
	)
      (or (looking-at mime-edit-beginning-tag-regexp)
	  (eobp)
	  (insert (mime-make-text-tag) "\n")
	  )
      )))

(defun mime-edit-enclose-quote-region (beg end)
  (interactive "*r")
  (mime-edit-enclose-region "quote" beg end)
  )

(defun mime-edit-enclose-mixed-region (beg end)
  (interactive "*r")
  (mime-edit-enclose-region "mixed" beg end)
  )

(defun mime-edit-enclose-parallel-region (beg end)
  (interactive "*r")
  (mime-edit-enclose-region "parallel" beg end)
  )

(defun mime-edit-enclose-digest-region (beg end)
  (interactive "*r")
  (mime-edit-enclose-region "digest" beg end)
  )

(defun mime-edit-enclose-alternative-region (beg end)
  (interactive "*r")
  (mime-edit-enclose-region "alternative" beg end)
  )

(defun mime-edit-enclose-signed-region (beg end)
  (interactive "*r")
  (if mime-edit-signing-type
      (mime-edit-enclose-region "signed" beg end)
    (message "Please specify signing type.")
    ))

(defun mime-edit-enclose-encrypted-region (beg end)
  (interactive "*r")
  (if mime-edit-signing-type
      (mime-edit-enclose-region "encrypted" beg end)
    (message "Please specify encrypting type.")
    ))

(defun mime-edit-insert-key (&optional arg)
  "Insert a pgp public key."
  (interactive "P")
  (mime-edit-insert-tag "application" "pgp-keys")
  (mime-edit-define-encoding "7bit")
  (funcall (pgp-function 'insert-key))
  )


;;; @ flag setting
;;;

(defun mime-edit-set-split (arg)
  (interactive
   (list
    (y-or-n-p "Do you want to enable split?")
    ))
  (setq mime-edit-split-message arg)
  (if arg
      (message "This message is enabled to split.")
    (message "This message is not enabled to split.")
    ))

(defun mime-edit-toggle-transfer-level (&optional transfer-level)
  "Toggle transfer-level is 7bit or 8bit through.

Optional TRANSFER-LEVEL is a number of transfer-level, 7 or 8."
  (interactive)
  (if (numberp transfer-level)
      (setq mime-transfer-level transfer-level)
    (if (< mime-transfer-level 8)
	(setq mime-transfer-level 8)
      (setq mime-transfer-level 7)
      ))
  (setq mime-edit-charset-default-encoding-alist
	(mime-make-charset-default-encoding-alist mime-transfer-level))
  (message (format "Current transfer-level is %d bit"
		   mime-transfer-level))
  (setq mime-transfer-level-string
	(mime-encoding-name mime-transfer-level 'not-omit))
  (force-mode-line-update)
  )

(defun mime-edit-set-transfer-level-7bit ()
  (interactive)
  (mime-edit-toggle-transfer-level 7)
  )

(defun mime-edit-set-transfer-level-8bit ()
  (interactive)
  (mime-edit-toggle-transfer-level 8)
  )


;;; @ pgp
;;;

(defun mime-edit-set-sign (arg)
  (interactive
   (list
    (y-or-n-p "Do you want to sign?")
    ))
  (if arg
      (if mime-edit-signing-type
	  (progn
	    (setq mime-edit-pgp-processing 'sign)
	    (message "This message will be signed.")
	    )
	(message "Please specify signing type.")
	)
    (if (eq mime-edit-pgp-processing 'sign)
	(setq mime-edit-pgp-processing nil)
      )
    (message "This message will not be signed.")
    ))

(defun mime-edit-set-encrypt (arg)
  (interactive
   (list
    (y-or-n-p "Do you want to encrypt?")
    ))
  (if arg
      (if mime-edit-encrypting-type
	  (progn
	    (setq mime-edit-pgp-processing 'encrypt)
	    (message "This message will be encrypt.")
	    )
	(message "Please specify encrypting type.")
	)
    (if (eq mime-edit-pgp-processing 'encrypt)
	(setq mime-edit-pgp-processing nil)
      )
    (message "This message will not be encrypt.")
    ))

(defvar mime-edit-pgp-processing nil)
(make-variable-buffer-local 'mime-edit-pgp-processing)

(defun mime-edit-pgp-enclose-buffer ()
  (let ((beg (save-excursion
	       (goto-char (point-min))
	       (if (search-forward (concat "\n" mail-header-separator "\n"))
		   (match-end 0)
		 )))
	(end (point-max))
	)
    (if beg
	(cond ((eq mime-edit-pgp-processing 'sign)
	       (mime-edit-enclose-signed-region beg end)
	       )
	      ((eq mime-edit-pgp-processing 'encrypt)
	       (mime-edit-enclose-encrypted-region beg end)
	       ))
      )))


;;; @ split
;;;

(defun mime-edit-insert-partial-header
  (fields subject id number total separator)
  (insert fields)
  (insert (format "Subject: %s (%d/%d)\n" subject number total))
  (insert (format "Mime-Version: 1.0 (split by %s)\n"
		  mime-edit-version-name))
  (insert (format "\
Content-Type: message/partial; id=%s; number=%d; total=%d\n%s\n"
		  id number total separator))
  )

(defun mime-edit-split-and-send
  (&optional cmd lines mime-edit-message-max-length)
  (interactive)
  (or lines
      (setq lines
	    (count-lines (point-min) (point-max)))
      )
  (or mime-edit-message-max-length
      (setq mime-edit-message-max-length
	    (or (cdr (assq major-mode mime-edit-message-max-lines-alist))
		mime-edit-message-default-max-lines))
      )
  (let* ((mime-edit-draft-file-name 
	  (or (buffer-file-name)
	      (make-temp-name
	       (expand-file-name "mime-draft" mime-temp-directory))))
	 (separator mail-header-separator)
	 (id (concat "\""
		     (replace-space-with-underline (current-time-string))
		     "@" (system-name) "\"")))
    (run-hooks 'mime-edit-before-split-hook)
    (let ((the-buf (current-buffer))
	  (copy-buf (get-buffer-create " *Original Message*"))
	  (header (std11-header-string-except
		   mime-edit-split-ignored-field-regexp separator))
	  (subject (mail-fetch-field "subject"))
	  (total (+ (/ lines mime-edit-message-max-length)
		    (if (> (mod lines mime-edit-message-max-length) 0)
			1)))
	  (command
	   (or cmd
	       (cdr
		(assq major-mode
		      mime-edit-split-message-sender-alist))
	       (function
		(lambda ()
		  (interactive)
		  (error "Split sender is not specified for `%s'." major-mode)
		  ))
	       ))
	  (mime-edit-partial-number 1)
	  data)
      (save-excursion
	(set-buffer copy-buf)
	(erase-buffer)
	(insert-buffer the-buf)
	(save-restriction
	  (if (re-search-forward
	       (concat "^" (regexp-quote separator) "$") nil t)
	      (let ((he (match-beginning 0)))
		(replace-match "")
		(narrow-to-region (point-min) he)
		))
	  (goto-char (point-min))
	  (while (re-search-forward mime-edit-split-blind-field-regexp nil t)
	    (delete-region (match-beginning 0)
			   (1+ (std11-field-end)))
	    )))
      (while (< mime-edit-partial-number total)
	(erase-buffer)
	(save-excursion
	  (set-buffer copy-buf)
	  (setq data (buffer-substring
		      (point-min)
		      (progn
			(goto-line mime-edit-message-max-length)
			(point))
		      ))
	  (delete-region (point-min)(point))
	  )
	(mime-edit-insert-partial-header
	 header subject id mime-edit-partial-number total separator)
	(insert data)
	(save-excursion
	  (message (format "Sending %d/%d..."
			   mime-edit-partial-number total))
	  (call-interactively command)
	  (message (format "Sending %d/%d... done"
			   mime-edit-partial-number total))
	  )
	(setq mime-edit-partial-number
	      (1+ mime-edit-partial-number))
	)
      (erase-buffer)
      (save-excursion
	(set-buffer copy-buf)
	(setq data (buffer-string))
	(erase-buffer)
	)
      (mime-edit-insert-partial-header
       header subject id mime-edit-partial-number total separator)
      (insert data)
      (save-excursion
	(message (format "Sending %d/%d..."
			 mime-edit-partial-number total))
	(message (format "Sending %d/%d... done"
			 mime-edit-partial-number total))
	)
      )))

(defun mime-edit-maybe-split-and-send (&optional cmd)
  (interactive)
  (run-hooks 'mime-edit-before-send-hook)
  (let ((mime-edit-message-max-length
	 (or (cdr (assq major-mode mime-edit-message-max-lines-alist))
	     mime-edit-message-default-max-lines))
	(lines (count-lines (point-min) (point-max)))
	)
    (if (and (> lines mime-edit-message-max-length)
	     mime-edit-split-message)
	(mime-edit-split-and-send cmd lines mime-edit-message-max-length)
      )))


;;; @ preview message
;;;

(defvar mime-edit-buffer nil) ; buffer local variable

(defun mime-edit-preview-message ()
  "preview editing MIME message. [mime-edit.el]"
  (interactive)
  (let* ((str (buffer-string))
	 (separator mail-header-separator)
	 (the-buf (current-buffer))
	 (buf-name (buffer-name))
	 (temp-buf-name (concat "*temp-article:" buf-name "*"))
	 (buf (get-buffer temp-buf-name))
	 )
    (if buf
	(progn
	  (switch-to-buffer buf)
	  (erase-buffer)
	  )
      (setq buf (get-buffer-create temp-buf-name))
      (switch-to-buffer buf)
      )
    (insert str)
    (setq major-mode 'mime-temp-message-mode)
    (make-local-variable 'mail-header-separator)
    (setq mail-header-separator separator)
    (make-local-variable 'mime-edit-buffer)
    (setq mime-edit-buffer the-buf)
    
    (run-hooks 'mime-edit-translate-hook)
    (mime-edit-translate-buffer)
    (goto-char (point-min))
    (if (re-search-forward
	 (concat "^" (regexp-quote separator) "$"))
	(replace-match "")
      )
    (mime-view-mode)
    ))

(defun mime-edit-quitting-method ()
  (let ((temp mime::preview/article-buffer)
	buf)
    (mime-view-kill-buffer)
    (set-buffer temp)
    (setq buf mime-edit-buffer)
    (kill-buffer temp)
    (switch-to-buffer buf)
    ))

(set-alist 'mime-view-quitting-method-alist
	   'mime-temp-message-mode
	   (function mime-edit-quitting-method)
	   )


;;; @ edit again
;;;

(defun mime-editor::edit-again (code-conversion)
  (save-excursion
    (goto-char (point-min))
    (let ((ctl (mime/Content-Type)))
      (if ctl
	  (let ((ctype (car ctl))
		(params (cdr ctl))
		type stype)
	    (if (string-match "/" ctype)
		(progn
		  (setq type (substring ctype 0 (match-beginning 0)))
		  (setq stype (substring ctype (match-end 0)))
		  )
	      (setq type ctype)
	      )
	    (cond
	     ((string= ctype "application/pgp-signature")
	      (delete-region (point-min)(point-max))
	      )
	     ((string= type "multipart")
	      (let* ((boundary (cdr (assoc "boundary" params)))
		     (boundary-pat
		      (concat "\n--" (regexp-quote boundary) "[ \t]*\n"))
		     )
		(re-search-forward boundary-pat nil t)
		(let ((bb (match-beginning 0)) eb tag)
		  (setq tag (format "\n--<<%s>>-{\n" stype))
		  (goto-char bb)
		  (insert tag)
		  (setq bb (+ bb (length tag)))
		  (re-search-forward
		   (concat "\n--" (regexp-quote boundary) "--[ \t]*\n")
		   nil t)
		  (setq eb (match-beginning 0))
		  (replace-match (format "--}-<<%s>>\n" stype))
		  (save-restriction
		    (narrow-to-region bb eb)
		    (goto-char (point-min))
		    (while (re-search-forward boundary-pat nil t)
		      (let ((beg (match-beginning 0))
			    end)
			(delete-region beg (match-end 0))
			(save-excursion
			  (if (re-search-forward boundary-pat nil t)
			      (setq end (match-beginning 0))
			    (setq end (point-max))
			    )
			  (save-restriction
			    (narrow-to-region beg end)
			    (mime-editor::edit-again code-conversion)
			    (goto-char (point-max))
			    ))))
		    ))
		(goto-char (point-min))
		(or (= (point-min) 1)
		    (delete-region (point-min)
				   (if (search-forward "\n\n" nil t)
				       (match-end 0)
				     (point-min)
				     )))
		))
	     (t
	      (let* (charset
		     (pstr
		      (let ((bytes (+ 14 (length ctype))))
			(mapconcat (function
				    (lambda (attr)
				      (if (string-equal (car attr) "charset")
					  (progn
					    (setq charset (cdr attr))
					    "")
					(let* ((str
						(concat (car attr)
							"=" (cdr attr))
						)
					       (bs (length str))
					       )
					  (setq bytes (+ bytes bs 2))
					  (if (< bytes 76)
					      (concat "; " str)
					    (setq bytes (+ bs 1))
					    (concat ";\n " str)
					    )
					  ))))
				   params "")))
		     encoding
		     encoded)
		(save-excursion
		  (if (re-search-forward
		       "Content-Transfer-Encoding:" nil t)
		      (let ((beg (match-beginning 0))
			    (hbeg (match-end 0))
			    (end (std11-field-end)))
			(setq encoding
			      (eliminate-top-spaces
			       (std11-unfold-string
				(buffer-substring hbeg end))))
			(if (or charset (string-equal type "text"))
			    (progn
			      (delete-region beg (1+ end))
			      (goto-char (point-min))
			      (if (search-forward "\n\n" nil t)
				  (progn
				    (mime-decode-region
				     (match-end 0)(point-max) encoding)
				    (setq encoded t
					  encoding nil)
				    )))))))
		(if (or code-conversion encoded)
		    (decode-mime-charset-region
		     (point-min)(point-max)
		     (or charset default-mime-charset))
		  )
		(let ((he
		       (if (re-search-forward "^$" nil t)
			   (match-end 0)
			 (point-min)
			 )))
		  (if (= (point-min) 1)
		      (progn
			(goto-char he)
			(insert
			 (concat "\n"
				 (mime-create-tag
				  (concat type "/" stype pstr) encoding)))
			)
		    (delete-region (point-min) he)
		    (insert
		     (mime-create-tag
		      (concat type "/" stype pstr) encoding))
		    ))
		))))
	(if code-conversion
	    (decode-mime-charset-region (point-min) (point-max)
					default-mime-charset)
	  )
	))))

(defun mime-edit-again (&optional code-conversion no-separator no-mode)
  "Convert current buffer to MIME-Edit buffer and turn on MIME-Edit mode.
Content-Type and Content-Transfer-Encoding header fields will be
converted to MIME-Edit tags."
  (interactive)
  (mime-editor::edit-again code-conversion)
  (goto-char (point-min))
  (save-restriction
    (narrow-to-region
     (point-min)
     (if (re-search-forward
	  (concat "^\\(" (regexp-quote mail-header-separator) "\\)?$")
	  nil t)
	 (match-end 0)
       (point-max)
       ))
    (goto-char (point-min))
    (while (re-search-forward
	    "^\\(Content-.*\\|Mime-Version\\):" nil t)
      (delete-region (match-beginning 0) (1+ (std11-field-end)))
      ))
  (or no-separator
      (and (re-search-forward "^$")
	   (replace-match mail-header-separator)
	   ))
  (or no-mode
      (mime-edit-mode)
      ))


;;; @ end
;;;

(provide 'mime-edit)

(run-hooks 'mime-edit-load-hook)

;;; mime-edit.el ends here
