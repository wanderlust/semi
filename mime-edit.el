;;;
;;; mime-edit.el --- Simple MIME Composer for GNU Emacs
;;;
;;; Copyright (C) 1993 UMEDA Masanobu
;;; Copyright (C) 1994 .. 1996 MORIOKA Tomohiko
;;;
;;; Author: UMEDA Masanobu <umerin@mse.kyutech.ac.jp>
;;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Created: 1994/08/21 renamed from mime.el by UMEDA Masanobu;
;;;          1996/05/24 renamed from tm-edit.el
;;; Version: $Revision$
;;; Keywords: mail, news, MIME, multimedia, multilingual
;;;
;;; This file is part of SEMI (September, Emacs MIME Interface)
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with This program.  If not, write to the Free Software
;;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This is an Emacs minor mode for editing Internet multimedia
;; messages formatted in MIME (RFC 1521 and RFC 1522). All messages in
;; this mode are composed in the tagged MIME format, that are
;; described in the following examples. The messages composed in the
;; tagged MIME format are automatically translated into a MIME
;; compliant message when exiting the mode.

;; Mule (a multilingual extension to Emacs 18 and 19) has a capability
;; of handling multilingual text in limited ISO-2022 manner that is
;; based on early experiences in Japanese Internet community and
;; resulted in RFC 1468 (ISO-2022-JP charset for MIME). In order to
;; enable multilingual capability in single text message in MIME,
;; charset of multilingual text written in Mule is declared as either
;; `ISO-2022-JP-2' [RFC 1554] or `ISO-2022-INT-1'. Mule is required
;; for reading the such messages.

;; This MIME composer can work with Mail mode, mh-e letter Mode, and
;; News mode.  First of all, you need the following autoload
;; definition to load mime/editor-mode automatically:
;;
;; (autoload 'mime/editor-mode "mime-edit"
;;           "Minor mode for editing MIME message." t)
;;
;; In case of Mail mode (includes VM mode), you need the following
;; hook definition:
;;
;; (add-hook 'mail-mode-hook 'mime/editor-mode)
;; (add-hook 'mail-send-hook 'mime-editor/maybe-translate)
;;
;; In case of MH-E, you need the following hook definition:
;;
;; (add-hook 'mh-letter-mode-hook
;;           (function
;;            (lambda ()
;;              (mime/editor-mode)
;;              (make-local-variable 'mail-header-separator)
;;              (setq mail-header-separator "--------")
;;              ))))
;; (add-hook 'mh-before-send-letter-hook 'mime-editor/maybe-translate)
;;
;; In case of News mode, you need the following hook definition:
;;
;; (add-hook 'news-reply-mode-hook 'mime/editor-mode)
;; (add-hook 'news-inews-hook 'mime-editor/maybe-translate)
;;
;; In case of Emacs 19, it is possible to emphasize the message tags
;; using font-lock mode as follows:
;;
;; (add-hook 'mime/editor-mode-hook
;;           (function
;;            (lambda ()
;;              (font-lock-mode 1)
;;              (setq font-lock-keywords (list mime-editor/tag-regexp))
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
;;
;;--[[text/plain; charset=ISO-2022-JP]]
;; これは charset を ISO-2022-JP に指定した日本語の plain テキストです.
;;
;;--[[text/richtext]]
;; <center>This is a richtext.</center>
;;
;;--[[image/gif][base64]]^M...image encoded in base64 comes here...
;;
;;--[[audio/basic][base64]]^M...audio encoded in base64 comes here...

;;; Code:

(require 'sendmail)
(require 'mail-utils)
(require 'mel)
(require 'tl-822)
(require 'tl-list)
(require 'tm-view)
(require 'tm-ew-e)
(require 'signature)


;;; @ version
;;;

(defconst mime-editor/RCS-ID
  "$Id$")

(defconst mime-editor/version (get-version-string mime-editor/RCS-ID))

(defconst mime-editor/version-name
  (concat "SEMI mime-edit " mime-editor/version))


;;; @ variables
;;;

(defvar mime-prefix "\C-c\C-x"
  "*Keymap prefix for MIME commands.")

(defvar mime-ignore-preceding-spaces nil
  "*Ignore preceding white spaces if non-nil.")

(defvar mime-ignore-trailing-spaces nil
  "*Ignore trailing white spaces if non-nil.")

(defvar mime-ignore-same-text-tag t
  "*Ignore preceding text content-type tag that is same with new one.
If non-nil, the text tag is not inserted unless something different.")

(defvar mime-auto-hide-body t
  "*Hide non-textual body encoded in base64 after insertion if non-nil.")

(defvar mime-voice-recorder
  (function mime-voice-recorder-for-sun)
  "*Function to record a voice message and return a buffer that contains it.")

(defvar mime/editor-mode-hook nil
  "*Hook called when enter MIME mode.")

(defvar mime-editor/translate-hook nil
  "*Hook called before translating into a MIME compliant message.
To insert a signature file automatically, call the function
`mime-editor/insert-signature' from this hook.")

(defvar mime-editor/exit-hook nil
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
     nil
     "attachment"	(("filename" . file))
     )
    ("\\.tgz$"
     "application" "octet-stream" (("type" . "tar+gzip"))
     nil
     "attachment"	(("filename" . file))
     )
    ("\\.tar\\.Z$"
     "application" "octet-stream" (("type" . "tar+compress"))
     nil
     "attachment"	(("filename" . file))
     )
    ("\\.taz$"
     "application" "octet-stream" (("type" . "tar+compress"))
     nil
     "attachment"	(("filename" . file))
     )
    ("\\.gz$"
     "application" "octet-stream" (("type" . "gzip"))
     nil
     "attachment"	(("filename" . file))
     )
    ("\\.Z$"
     "application" "octet-stream" (("type" . "compress"))
     nil
     "attachment"	(("filename" . file))
     )
    ("\\.lzh$"
     "application" "octet-stream" (("type" . "lha"))
     nil
     "attachment"	(("filename" . file))
     )
    ("\\.zip$"
     "application" "zip" nil
     nil
     "attachment"	(("filename" . file))
     )
    ("\\.diff$"
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

(defvar mime-editor/transfer-level 7
  "*A number of network transfer level. It should be bigger than 7.")
(make-variable-buffer-local 'mime-editor/transfer-level)

(defvar mime-editor/transfer-level-string
  (mime/encoding-name mime-editor/transfer-level 'not-omit)
  "*A string formatted version of mime/defaul-transfer-level")
(make-variable-buffer-local 'mime-editor/transfer-level-string)

(defvar mime-editor/charset-default-encoding-alist
  (mime/make-charset-default-encoding-alist mime-editor/transfer-level))
(make-variable-buffer-local 'mime-editor/charset-default-encoding-alist)

;;; @@ about message inserting
;;;

(defvar mime-editor/yank-ignored-field-list
  '("Received" "Approved" "Path" "Replied" "Status" "X-VM-.*" "X-UIDL")
  "Delete these fields from original message when it is inserted
as message/rfc822 part.
Each elements are regexp of field-name. [mime-edit.el]")

(defvar mime-editor/yank-ignored-field-regexp
  (concat "^"
	  (apply (function regexp-or) mime-editor/yank-ignored-field-list)
	  ":"))

(defvar mime-editor/message-inserter-alist nil)
(defvar mime-editor/mail-inserter-alist nil)

;;; @@ about message splitting
;;;

(defvar mime-editor/split-message t
  "*Split large message if it is non-nil. [mime-edit.el]")

(defvar mime-editor/message-default-max-length 1000
  "*Default maximum size of a message. [mime-edit.el]")

(defvar mime-editor/message-max-length-alist
  '((news-reply-mode . 500)))

(defconst mime-editor/split-ignored-field-regexp
  "\\(^Content-\\|^Subject:\\|^Mime-Version:\\)")

(defvar mime-editor/split-blind-field-regexp
  "\\(^[BDFbdf]cc:\\|^cc:[ \t]*$\\)")

(defvar mime-editor/message-default-sender-alist
  '((mail-mode . mail-send-and-exit)
    (mh-letter-mode . mh-send-letter)
    (news-reply-mode . gnus-inews-news)
    ))

(defvar mime-editor/split-message-sender-alist
  '((mail-mode
     . (lambda ()
	 (interactive)
	 (sendmail-send-it)
	 ))
    (mh-letter-mode
     . (lambda (&optional arg)
	 (interactive "P")
	 (write-region (point-min) (point-max)
		       mime-editor/draft-file-name nil 'no-message)
	 (cond (arg
		(pop-to-buffer "MH mail delivery")
		(erase-buffer)
		(mh-exec-cmd-output mh-send-prog t "-watch" "-nopush"
				    "-nodraftfolder"
				    mh-send-args mime-editor/draft-file-name)
		(goto-char (point-max))	; show the interesting part
		(recenter -1)
		(sit-for 1))
	       (t
		(apply 'mh-exec-cmd-quiet t mh-send-prog 
		       (mh-list-to-string
			(list "-nopush" "-nodraftfolder"
			      "-noverbose" "-nowatch"
			      mh-send-args mime-editor/draft-file-name)))))
	 ))
    ))

(defvar mime-editor/window-config-alist
  '((mail-mode       . nil)
    (mh-letter-mode  . mh-previous-window-config)
    (news-reply-mode . (cond ((boundp 'gnus-winconf-post-news)
			      (prog1
				  gnus-winconf-post-news
				(setq gnus-winconf-post-news nil)
				))
			     ((boundp 'gnus-prev-winconf)
			      (prog1
				  gnus-prev-winconf
				(setq gnus-prev-winconf nil)
				))
			     ))
    ))

(defvar mime-editor/news-reply-mode-server-running nil)


;;; @@ about PGP
;;;

(defvar mime-editor/signing-type nil
  "*PGP signing type (pgp-elkins, pgp-kazu or nil). [mime-edit.el]")

(defvar mime-editor/encrypting-type nil
  "*PGP encrypting type (pgp-elkins, pgp-kazu or nil). [mime-edit.el]")

(if (or mime-editor/signing-type mime-editor/encrypting-type)
    (require 'mailcrypt)
  )


;;; @@ about tag
;;;

(defconst mime-editor/single-part-tag-regexp
  "--[[][[]\\([^]]*\\)]\\([[]\\([^]]*\\)]\\|\\)]"
  "*Regexp of MIME tag in the form of [[CONTENT-TYPE][ENCODING]].")

(defconst mime-editor/quoted-single-part-tag-regexp
  (concat "- " (substring mime-editor/single-part-tag-regexp 1)))

(defconst mime-editor/multipart-beginning-regexp "^--<<\\([^<>]+\\)>>-{\n")

(defconst mime-editor/multipart-end-regexp "^--}-<<\\([^<>]+\\)>>\n")

(defconst mime-editor/beginning-tag-regexp
  (regexp-or mime-editor/single-part-tag-regexp
	     mime-editor/multipart-beginning-regexp))

(defconst mime-editor/end-tag-regexp
  (regexp-or mime-editor/single-part-tag-regexp
	     mime-editor/multipart-end-regexp))

(defconst mime-editor/tag-regexp
  (regexp-or mime-editor/single-part-tag-regexp
	     mime-editor/multipart-beginning-regexp
	     mime-editor/multipart-end-regexp))

(defvar mime-tag-format "--[[%s]]"
  "*Control-string making a MIME tag.")

(defvar mime-tag-format-with-encoding "--[[%s][%s]]"
  "*Control-string making a MIME tag with encoding.")

;;; @@ multipart boundary
;;;

(defvar mime-multipart-boundary "Multipart"
  "*Boundary of a multipart message.")


;;; @@ buffer local variables
;;;

(defvar mime/editor-mode-old-local-map nil)
(defvar mime/editor-mode-old-selective-display nil)
(defvar mime/editing-buffer nil)


;;; @ constants
;;;

(defconst mime-tspecials-regexp "[][()<>@,;:\\\"/?.= \t]"
  "*Specify MIME tspecials.
Tspecials means any character that matches with it in header must be quoted.")

(defconst mime-editor/mime-version-value
  (concat "1.0 (" mime-editor/version-name ")")
  "MIME version number.")

(defconst mime-editor/mime-map (make-sparse-keymap)
  "Keymap for MIME commands.")


;;; @ keymap and menu
;;;

(defvar mime/editor-mode-flag nil)
(make-variable-buffer-local 'mime/editor-mode-flag)

(set-alist 'minor-mode-alist
	   'mime/editor-mode-flag
	   '((" MIME-Edit "  mime-editor/transfer-level-string)))

(defun mime-editor/define-keymap (keymap)
  "Add mime-editor commands to KEYMAP."
  (if (not (keymapp keymap))
      nil
    (define-key keymap "\C-t" 'mime-editor/insert-text)
    (define-key keymap "\C-i" 'mime-editor/insert-file)
    (define-key keymap "\C-e" 'mime-editor/insert-external)
    (define-key keymap "\C-v" 'mime-editor/insert-voice)
    (define-key keymap "\C-y" 'mime-editor/insert-message)
    (define-key keymap "\C-m" 'mime-editor/insert-mail)
    (define-key keymap "\C-w" 'mime-editor/insert-signature)
    (define-key keymap "\C-s" 'mime-editor/insert-signature)
    (define-key keymap "\C-k" 'mime-editor/insert-key)
    (define-key keymap "t"    'mime-editor/insert-tag)
    (define-key keymap "a"    'mime-editor/enclose-alternative-region)
    (define-key keymap "p"    'mime-editor/enclose-parallel-region)
    (define-key keymap "m"    'mime-editor/enclose-mixed-region)
    (define-key keymap "d"    'mime-editor/enclose-digest-region)
    (define-key keymap "s"    'mime-editor/enclose-signed-region)
    (define-key keymap "e"    'mime-editor/enclose-encrypted-region)
    (define-key keymap "\C-p" 'mime-editor/preview-message)
    (define-key keymap "\C-z" 'mime-editor/exit)
    (define-key keymap "?"    'mime-editor/help)
    ))

(mime-editor/define-keymap mime-editor/mime-map)

(defconst mime-editor/menu-title "MIME-Edit")

(defconst mime-editor/menu-list
  '((mime-help	"Describe MIME editor mode" mime-editor/help)
    (file	"Insert File"		mime-editor/insert-file)
    (external	"Insert External"	mime-editor/insert-external)
    (voice	"Insert Voice"		mime-editor/insert-voice)
    (message	"Insert Message"	mime-editor/insert-message)
    (mail	"Insert Mail"		mime-editor/insert-mail)
    (signature	"Insert Signature"	mime-editor/insert-signature)
    (text	"Insert Text"		mime-editor/insert-text)
    (tag	"Insert Tag"		mime-editor/insert-tag)
    (alternative "Enclose as alternative"
		 mime-editor/enclose-alternative-region)
    (parallel	"Enclose as parallel"	mime-editor/enclose-parallel-region)
    (mixed	"Enclose as serial"	mime-editor/enclose-mixed-region)
    (digest	"Enclose as digest"	mime-editor/enclose-digest-region)
    (signed	"Enclose as signed"	mime-editor/enclose-signed-region)
    (encrypted	"Enclose as encrypted"	mime-editor/enclose-encrypted-region)
    (key	"Insert Public Key"	mime-editor/insert-key)
    (split	"About split"           mime-editor/set-split)
    (sign	"About sign"		mime-editor/set-sign)
    (encrypt	"About encryption"	mime-editor/set-encrypt)
    (preview	"Preview Message"	mime-editor/preview-message)
    (level	"Toggle transfer-level"	mime-editor/toggle-transfer-level)
    )
  "MIME-edit menubar entry.")

(defun mime-editor/define-menu-for-emacs19 ()
  "Define menu for Emacs 19."
  (define-key (current-local-map) [menu-bar mime-edit]
    (cons mime-editor/menu-title
	  (make-sparse-keymap mime-editor/menu-title)))
  (mapcar (function
	   (lambda (item)
	     (define-key (current-local-map)
	       (vector 'menu-bar 'mime-edit (car item))
	       (cons (nth 1 item)(nth 2 item))
	       )
	     ))
	  (reverse mime-editor/menu-list)
	  ))

;;; modified by Pekka Marjola <pema@iki.fi>
;;;	1995/9/5 (c.f. [tm-en:69])
(defun mime-editor/define-menu-for-xemacs ()
  "Define menu for Emacs 19."
  (cond ((featurep 'menubar)
	 (make-local-variable 'current-menubar)
	 (set-buffer-menubar current-menubar)
	 (add-submenu nil
		      (cons mime-editor/menu-title
			    (mapcar (function
				     (lambda (item)
				       (vector (nth 1 item)(nth 2 item)
					       mime/editor-mode-flag)
				       ))
				    mime-editor/menu-list)))
	 )))

;;; modified by Steven L. Baur <steve@miranova.com>
;;;	1995/12/6 (c.f. [tm-en:209])
(if (and running-xemacs (not (boundp 'mime-editor/popup-menu-for-xemacs)))
    (setq mime-editor/popup-menu-for-xemacs
	  (append '("MIME Commands" "---")
		  (mapcar (function (lambda (item)
				      (vector (nth 1 item)
					      (nth 2 item)
					      t)))
			  mime-editor/menu-list)))
  )
;;; end


;;; @ functions
;;;

;;;###autoload
(defun mime/editor-mode ()
  "MIME minor mode for editing the tagged MIME message.

In this mode, basically, the message is composed in the tagged MIME
format.  The message tag looks like:

	`--[[text/plain; charset=ISO-2022-JP][7bit]]'.

The tag specifies the MIME content type, subtype, optional parameters
and transfer encoding of the message following the tag.  Messages
without any tag are treated as `text/plain' by default.  Charset and
transfer encoding are automatically defined unless explicitly
specified.  Binary messages such as audio and image are usually hidden
using selective-display facility.  The messages in the tagged MIME
format are automatically translated into a MIME compliant message when
exiting this mode.

Available charsets depend on Emacs version being used.  The following
lists the available charsets of each emacs.

Emacs18:	US-ASCII is only available.
NEmacs:		US-ASCII and ISO-2022-JP are available.
Emacs19:	US-ASCII and ISO-8859-1 are available.
Mule:		US-ASCII, ISO-8859-* (except for ISO-8859-6),
		ISO-2022-JP, ISO-2022-JP-2 and ISO-2022-INT-1 are available.

ISO-2022-JP-2 and ISO-2022-INT-1 charsets used in Mule is expected to
be used to represent multilingual text in intermixed manner.  Any
languages that has no registered charset are represented as either
ISO-2022-JP-2 or ISO-2022-INT-1 in Mule.

Following commands are available in addition to major mode commands:
\\[mime-editor/insert-text]	insert a text message.
\\[mime-editor/insert-file]	insert a (binary) file.
\\[mime-editor/insert-external]	insert a reference to external body.
\\[mime-editor/insert-voice]	insert a voice message.
\\[mime-editor/insert-message]	insert a mail or news message.
\\[mime-editor/insert-mail]	insert a mail message.
\\[mime-editor/insert-signature]	insert a signature file at end.
\\[mime-editor/insert-tag]	insert a new MIME tag.
\\[mime-editor/enclose-alternative-region]	enclose as multipart/alternative.
\\[mime-editor/enclose-parallel-region]	enclose as multipart/parallel.
\\[mime-editor/enclose-mixed-region]	enclose as multipart/mixed.
\\[mime-editor/enclose-digest-region]	enclose as multipart/digest.
\\[mime-editor/enclose-signed-region]	enclose as PGP signed.
\\[mime-editor/enclose-encrypted-region]	enclose as PGP encrypted.
\\[mime-editor/insert-key]	insert PGP public key.
\\[mime-editor/preview-message]	preview editing MIME message.
\\[mime-editor/exit]	exit and translate into a MIME compliant message.
\\[mime-editor/maybe-translate]	exit and translate if in MIME mode, then split.
\\[mime-editor/help]	show this help.

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
	--[[text/plain; charset=ISO-2022-JP]]
	これは charset を ISO-2022-JP に指定した日本語の plain テキス
	トです.
	--[[text/richtext]]
	<center>This is a richtext.</center>
	--[[image/gif][base64]]^M...image encoded in base64 here...
	--[[audio/basic][base64]]^M...audio encoded in base64 here...

User customizable variables (not documented all of them):
 mime-prefix
    Specifies a key prefix for MIME minor mode commands.

 mime-ignore-preceding-spaces
    Preceding white spaces in a message body are ignored if non-nil.

 mime-ignore-trailing-spaces
    Trailing white spaces in a message body are ignored if non-nil.

 mime-auto-fill-header
    Fill header fields that contain encoded-words if non-nil.

 mime-auto-hide-body
    Hide a non-textual body message encoded in base64 after insertion
    if non-nil.

 mime-voice-recorder
    Specifies a function to record a voice message and return a buffer
    that contains it.  The function mime-voice-recorder-for-sun is for
    Sun SparcStations.

 mime/editor-mode-hook
    Turning on MIME mode calls the value of mime/editor-mode-hook, if
    it is non-nil.

 mime-editor/translate-hook
    The value of mime-editor/translate-hook is called just before translating
    the tagged MIME format into a MIME compliant message if it is
    non-nil.  If the hook call the function mime-editor/insert-signature,
    the signature file will be inserted automatically.

 mime-editor/exit-hook
    Turning off MIME mode calls the value of mime-editor/exit-hook, if it is
    non-nil."
  (interactive)
  (if mime/editor-mode-flag
      (error "You are already editing a MIME message.")
    (setq mime/editor-mode-flag t)
    ;; Remember old key bindings.
    (if running-xemacs
	nil
      (make-local-variable 'mime/editor-mode-old-local-map)
      (setq mime/editor-mode-old-local-map (current-local-map))
      ;; Add MIME commands to current local map.
      (use-local-map (copy-keymap (current-local-map)))
      )
    (if (not (lookup-key (current-local-map) mime-prefix))
	(define-key (current-local-map) mime-prefix mime-editor/mime-map))

    ;; Set transfer level into mode line
    ;;
    (setq mime-editor/transfer-level-string
 	  (mime/encoding-name mime-editor/transfer-level 'not-omit))
    (force-mode-line-update)
    
    ;; Define menu.  Menus for other emacs implementations are
    ;; welcome.
    (cond (running-xemacs
	   (mime-editor/define-menu-for-xemacs))
	  ((>= emacs-major-version 19)
	   (mime-editor/define-menu-for-emacs19)
	   ))
    ;; end
    
    ;; Remember old selective-display.
    (make-local-variable 'mime/editor-mode-old-selective-display)
    (setq mime/editor-mode-old-selective-display selective-display)
    (setq selective-display t)
    ;; I don't care about saving these.
    (setq paragraph-start
	  (regexp-or mime-editor/single-part-tag-regexp
		     paragraph-start))
    (setq paragraph-separate
	  (regexp-or mime-editor/single-part-tag-regexp
		     paragraph-separate))
    (run-hooks 'mime/editor-mode-hook)
    (message
     (substitute-command-keys
      "Type \\[mime-editor/exit] to exit MIME mode, and type \\[mime-editor/help] to get help."))
    ))

;;;###autoload
(defalias 'edit-mime 'mime/editor-mode)		; for convenience
(defalias 'mime-mode 'mime/editor-mode)		; for convenience

(defun mime-editor/exit (&optional nomime no-error)
  "Translate the tagged MIME message into a MIME compliant message.
With no argument encode a message in the buffer into MIME, otherwise
just return to previous mode."
  (interactive "P")
  (if (not mime/editor-mode-flag)
      (if (null no-error)
	  (error "You aren't editing a MIME message.")
	)
    (if (not nomime)
	(progn
	  (run-hooks 'mime-editor/translate-hook)
	  (mime-editor/translate-buffer)))
    ;; Restore previous state.
    (setq mime/editor-mode-flag nil)
    (cond (running-xemacs
	   ;; mime-prefix only defined if binding was nil
	   (if (eq (lookup-key (current-local-map) mime-prefix)
		   mime-editor/mime-map)
	       (define-key (current-local-map) mime-prefix nil))
	   (delete-menu-item (list mime-editor/menu-title)))
	  (t
	   (use-local-map mime/editor-mode-old-local-map)))
    
    (setq selective-display mime/editor-mode-old-selective-display)
    (set-buffer-modified-p (buffer-modified-p))
    (run-hooks 'mime-editor/exit-hook)
    (message "Exit MIME editor mode.")
    ))

(defun mime-editor/maybe-translate ()
  (interactive)
  (mime-editor/exit nil t)
  (call-interactively 'mime-editor/maybe-split-and-send)
  )

(defun mime-editor/help ()
  "Show help message about MIME mode."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "MIME editor mode:\n")
    (princ (documentation 'mime/editor-mode))
    (print-help-return-message)))

(defun mime-editor/insert-text ()
  "Insert a text message.
Charset is automatically obtained from the `mime/lc-charset-alist'."
  (interactive)
  (if (and (mime-editor/insert-tag "text" nil nil)
	   (looking-at mime-editor/single-part-tag-regexp))
      (progn
	;; Make a space between the following message.
	(insert "\n")
	(forward-char -1)
	)))

(defun mime-editor/insert-file (file)
  "Insert a message from a file."
  (interactive "fInsert file as MIME message: ")
  (let*  ((guess (mime-find-file-type file))
	  (pritype (nth 0 guess))
	  (subtype (nth 1 guess))
	  (parameters (nth 2 guess))
	  (default (nth 3 guess))	;Guess encoding from its file name.
	  (disposition-type (nth 4 guess))
	  (disposition-params (nth 5 guess))
	  (encoding
	   (if (not (interactive-p))
	       default
	     (completing-read
	      (concat "What transfer encoding"
		      (if default
			  (concat " (default "
				  (if (string-equal default "")
				      "\"\""
				    default)
				  ")"
				  ))
		      ": ")
	      mime-encoding-method-alist nil t nil))))
    (if (string-equal encoding "")
	(setq encoding default))
    (if (or (consp parameters) (stringp disposition-type))
	(let ((rest parameters) cell attribute value)
	  (setq parameters "")
	  (while rest
	    (setq cell (car rest))
	    (setq attribute (car cell))
	    (setq value (cdr cell))
	    (if (eq value 'file)
		(setq value (rfc822/wrap-as-quoted-string
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
		      (setq value (rfc822/wrap-as-quoted-string
				   (file-name-nondirectory file)))
		    )
		  (setq parameters
			(concat parameters "; " attribute "=" value))
		  (setq rest (cdr rest))
		  )
		))
	  ))
    (mime-editor/insert-tag pritype subtype parameters)
    (mime-editor/insert-binary-file file encoding)
    ))

(defun mime-editor/insert-external ()
  "Insert a reference to external body."
  (interactive)
  (mime-editor/insert-tag "message" "external-body" nil ";\n\t")
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
	   (not (looking-at mime-editor/single-part-tag-regexp)))
      (insert (mime-make-text-tag) "\n")))

(defun mime-editor/insert-voice ()
  "Insert a voice message."
  (interactive)
  (mime-editor/insert-tag "audio" "basic" nil)
  (let ((buffer (funcall mime-voice-recorder)))
    (unwind-protect
	(mime-editor/insert-binary-buffer buffer "base64")
      (kill-buffer buffer)
      )))

(defun mime-editor/insert-signature (&optional arg)
  "Insert a signature file."
  (interactive "P")
  (let ((signature-insert-hook
         (function
          (lambda ()
            (apply (function mime-editor/insert-tag)
                   (mime-find-file-type signature))
            )))
        )
    (insert-signature arg)
    ))


;; Insert a new tag around a point.

(defun mime-editor/insert-tag (&optional pritype subtype parameters delimiter)
  "Insert new MIME tag and return a list of PRITYPE, SUBTYPE, and PARAMETERS.
If nothing is inserted, return nil."
  (interactive)
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
	    (if (mime-editor/goto-tag)
		(buffer-substring (match-beginning 0) (match-end 0))
	      ;; Assume content type is 'text/plan'.
	      (mime-make-tag "text" "plain")
	      )))
    ;; We are only interested in TEXT.
    (if (and oldtag
	     (not (mime-test-content-type
		   (mime-editor/get-contype oldtag) "text")))
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

;; Insert the binary content after MIME tag.
;;	modified by MORITA Masahiro <hiro@isl.ntt.JP>
;;	for x-uue
(defun mime-editor/insert-binary-file (file &optional encoding)
  "Insert binary FILE at point.
Optional argument ENCODING specifies an encoding method such as base64."
  (let ((tmpbuf (get-buffer-create " *MIME insert*")))
    (save-excursion
      (set-buffer tmpbuf)
      (erase-buffer)
      (let ((mc-flag nil)		;Mule
	    (file-coding-system-for-read
	     (if (featurep 'mule) *noconv*))
	    (kanji-flag nil)		;NEmacs
	    (emx-binary-mode t)	        ;Stop CRLF to LF conversion in OS/2
	    )
	(let (jka-compr-compression-info-list
	      jam-zcat-filename-list)
	  (insert-file-contents file))))
    (prog1
 	(if (and (stringp encoding)
		 (string-equal (downcase encoding) "x-uue"))
	    (progn
	      (require 'mel-u)
	      (let ((uuencode-external-encoder
		     (cons (car uuencode-external-encoder)
			   (list (file-name-nondirectory file))
			   )))
		(mime-editor/insert-binary-buffer tmpbuf encoding)
		))
 	  (mime-editor/insert-binary-buffer tmpbuf encoding))
      (kill-buffer tmpbuf))))

;; Insert the binary content after MIME tag.
;;	modified by MORITA Masahiro <hiro@isl.ntt.JP>
;;	for x-uue
(defun mime-editor/insert-binary-buffer (buffer &optional encoding)
  "Insert binary BUFFER at point.
Optional argument ENCODING specifies an encoding method such as base64."
  (let* ((tagend (1- (point)))		;End of the tag
	 (hide-p (and mime-auto-hide-body
		      (stringp encoding)
		      (let ((en (downcase encoding)))
			(or (string-equal en "base64")
			    (string-equal en "x-uue")
			    ))))
	 )
    (save-restriction
      (narrow-to-region (1- (point)) (point))
      (let ((start (point))
	    (emx-binary-mode t))	;Stop LF to CRLF conversion in OS/2
	(insert-buffer-substring buffer)
	;; Encode binary message if necessary.
	(if encoding
	    (mime-encode-region start (point-max) encoding)
	  ))
      (if hide-p
	  (progn
	    (mime-flag-region (point-min) (1- (point-max)) ?\^M)
	    (goto-char (point-max)))
	))
    ;; Define encoding even if it is 7bit.
    (if (stringp encoding)
	(save-excursion
	  (goto-char tagend)		;Make sure which line the tag is on.
	  (mime-editor/define-encoding encoding)))
    ))


;; Commands work on a current message flagment.

(defun mime-editor/goto-tag ()
  "Search for the beginning of the tagged MIME message."
  (let ((current (point)) multipart)
    (if (looking-at mime-editor/tag-regexp)
	t
      ;; At first, go to the end.
      (cond ((re-search-forward mime-editor/beginning-tag-regexp nil t)
	     (goto-char (1- (match-beginning 0))) ;For multiline tag
	     )
	    (t
	     (goto-char (point-max))
	     ))
      ;; Then search for the beginning. 
      (re-search-backward mime-editor/end-tag-regexp nil t)
      (or (looking-at mime-editor/beginning-tag-regexp)
	  ;; Restore previous point.
	  (progn
	    (goto-char current)
	    nil
	    ))
      )))

(defun mime-editor/content-beginning ()
  "Return the point of the beginning of content."
  (save-excursion
    (let ((beg (save-excursion
		 (beginning-of-line) (point))))
      (if (mime-editor/goto-tag)
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

(defun mime-editor/content-end ()
  "Return the point of the end of content."
  (save-excursion
    (let ((beg (save-excursion
		 (beginning-of-line) (point))))
      (if (mime-editor/goto-tag)
	  (let ((top (point)))
	    (goto-char (match-end 0))
	    (if (and (= beg top)	;Must be on the same line.
		     (= (following-char) ?\^M))
		(progn
		  (end-of-line)
		  (point))
	      ;; Move to the end of this text.
	      (if (re-search-forward mime-editor/tag-regexp nil 'move)
		  ;; Don't forget a multiline tag.
		  (goto-char (match-beginning 0)))
	      (point)
	      ))
	;; Assume the message begins with text/plain.
	(goto-char (mime-editor/content-beginning))
	(if (re-search-forward mime-editor/tag-regexp nil 'move)
	    ;; Don't forget a multiline tag.
	    (goto-char (match-beginning 0)))
	(point))
      )))

(defun mime-editor/define-charset (charset)
  "Set charset of current tag to CHARSET."
  (save-excursion
    (if (mime-editor/goto-tag)
	(let ((tag (buffer-substring (match-beginning 0) (match-end 0))))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert
	   (mime-create-tag (mime-set-parameter
			     (mime-editor/get-contype tag) "charset" charset)
			    (mime-editor/get-encoding tag))))
      )))

(defun mime-editor/define-encoding (encoding)
  "Set encoding of current tag to ENCODING."
  (save-excursion
    (if (mime-editor/goto-tag)
	(let ((tag (buffer-substring (match-beginning 0) (match-end 0))))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert (mime-create-tag (mime-editor/get-contype tag) encoding)))
      )))

(defun mime-editor/choose-charset ()
  "Choose charset of a text following current point."
  (mime/find-charset-region (point) (mime-editor/content-end))
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

(defun mime-editor/get-contype (tag)
  "Return Content-Type (including parameters) of TAG."
  (and (stringp tag)
       (or (string-match mime-editor/single-part-tag-regexp tag)
	   (string-match mime-editor/multipart-beginning-regexp tag)
	   (string-match mime-editor/multipart-end-regexp tag)
	   )
       (substring tag (match-beginning 1) (match-end 1))
       ))

(defun mime-editor/get-encoding (tag)
  "Return encoding of TAG."
  (and (stringp tag)
       (string-match mime-editor/single-part-tag-regexp tag)
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

(defun mime-set-parameter (contype parameter value)
  "For given CONTYPE set PARAMETER to VALUE."
  (if (string-match
       (concat
	";[ \t\n]*\\("
	(regexp-quote parameter)
	"[ \t\n]*=[ \t\n]*\\([^\" \t\n;]*\\|\"[^\"]*\"\\)\\)[ \t\n]*\\(;\\|$\\)")
       contype)
      ;; Change value
      (concat (substring contype 0 (match-beginning 1))
	      parameter "=" value
	      (substring contype (match-end 1)))
    (concat contype "; " parameter "=" value)))

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

(defun mime-prompt-for-type ()
  "Ask for Content-type."
  (let ((type ""))
    ;; Repeat until primary content type is specified.
    (while (string-equal type "")
      (setq type
	    (completing-read "What content type: "
			     mime-content-types
			     nil
			     'require-match ;Type must be specified.
			     nil
			     ))
      (if (string-equal type "")
	  (progn
	    (message "Content type is required.")
	    (beep)
	    (sit-for 1)
	    ))
      )
    type
    ))

(defun mime-prompt-for-subtype (pritype)
  "Ask for Content-type subtype of Content-Type PRITYPE."
  (let* ((default (car (car (cdr (assoc pritype mime-content-types)))))
	 (answer
	  (completing-read
	   (if default
	       (concat
		"What content subtype: (default " default ") ")
	     "What content subtype: ")
	   (cdr (assoc pritype mime-content-types))
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

(defun mime-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is `\\n' (newline character) then text is shown,
while if FLAG is `\\^M' (control-M) the text is hidden."
  (let ((buffer-read-only nil)		;Okay even if write protected.
	(modp (buffer-modified-p)))
    (unwind-protect
        (subst-char-in-region from to
			      (if (= flag ?\n) ?\^M ?\n)
			      flag t)
      (set-buffer-modified-p modp))))


;;; @ Translate the tagged MIME messages into a MIME compliant message.
;;;

(defvar mime-editor/translate-buffer-hook
  '(mime-editor/pgp-enclose-buffer
    mime/encode-message-header
    mime-editor/translate-body))

(defun mime-editor/translate-buffer ()
  "Encode the tagged MIME message in current buffer in MIME compliant message."
  (interactive)
  (if (catch 'mime-editor/error
	(save-excursion
	  (run-hooks 'mime-editor/translate-buffer-hook)
	  ))
      (progn
	(undo)
	(error "Translation error!")
	)))

(defun mime-editor/find-inmost ()
  (goto-char (point-min))
  (if (re-search-forward mime-editor/multipart-beginning-regexp nil t)
      (let ((bb (match-beginning 0))
	    (be (match-end 0))
	    (type (buffer-substring (match-beginning 1)(match-end 1)))
	    end-exp eb ee)
	(setq end-exp (format "^--}-<<%s>>\n" type))
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
	(if (re-search-forward mime-editor/multipart-beginning-regexp nil t)
	    (let (ret)
	      (narrow-to-region (match-beginning 0)(point-max))
	      (mime-editor/find-inmost)
	      )
	  (widen)
	  (list type bb be eb)
	  ))))

(defun mime-editor/process-multipart-1 (boundary)
  (let ((ret (mime-editor/find-inmost)))
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
	  (if (looking-at mime-editor/multipart-end-regexp)
	      (let ((beg (match-beginning 0))
		    (end (match-end 0))
		    )
		(delete-region beg end)
		(if (and (not (looking-at mime-editor/single-part-tag-regexp))
			 (not (eobp)))
		    (insert (concat (mime-make-text-tag) "\n"))
		  )))
	  (cond ((string-equal type "quote")
		 (mime-editor/enquote-region bb eb)
		 )
		((string= type "signed")
		 (cond ((eq mime-editor/signing-type 'pgp-elkins)
			(mime-editor/sign-pgp-elkins bb eb boundary)
			)
		       ((eq mime-editor/signing-type 'pgp-kazu)
			(mime-editor/sign-pgp-kazu bb eb boundary)
			))
		 )
		((string= type "encrypted")
		 (cond ((eq mime-editor/encrypting-type 'pgp-elkins)
			(mime-editor/encrypt-pgp-elkins bb eb boundary)
			)
		       ((eq mime-editor/encrypting-type 'pgp-kazu)
			(mime-editor/encrypt-pgp-kazu bb eb boundary)
			)))
		(t
		 (setq boundary
		       (nth 2 (mime-editor/translate-region bb eb
							    boundary t)))
		 (goto-char bb)
		 (insert
		  (format "--[[multipart/%s;
 boundary=\"%s\"][7bit]]\n"
			  type boundary))
		 ))
	  boundary))))

(defun mime-editor/enquote-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (while (re-search-forward mime-editor/single-part-tag-regexp nil t)
	(let ((tag (buffer-substring (match-beginning 0)(match-end 0))))
	  (replace-match (concat "- " (substring tag 1)))
	  )))))

(defun mime-editor/dequote-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (while (re-search-forward
	      mime-editor/quoted-single-part-tag-regexp nil t)
	(let ((tag (buffer-substring (match-beginning 0)(match-end 0))))
	  (replace-match (concat "-" (substring tag 2)))
	  )))))

(autoload 'mc-pgp-lookup-key "mc-pgp")
(autoload 'mc-pgp-sign-region "mc-pgp")
(autoload 'mc-pgp-encrypt-region "mc-pgp")

(defun tm:mc-pgp-generic-parser (result)
  (let ((ret (mc-pgp-generic-parser result)))
    (if (consp ret)
	(vector (car ret)(cdr ret))
      )))

(defun tm:mc-process-region
  (beg end passwd program args parser &optional buffer boundary)
  (let ((obuf (current-buffer))
	(process-connection-type nil)
	mybuf result rgn proc)
    (unwind-protect
	(progn
	  (setq mybuf (or buffer (generate-new-buffer " *mailcrypt temp")))
	  (set-buffer mybuf)
	  (erase-buffer)
	  (set-buffer obuf)
	  (buffer-disable-undo mybuf)
	  (setq proc
		(apply 'start-process "*PGP*" mybuf program args))
	  (if passwd
	      (progn
		(process-send-string proc (concat passwd "\n"))
		(or mc-passwd-timeout (mc-deactivate-passwd t))))
	  (process-send-region proc beg end)
	  (process-send-eof proc)
	  (while (eq 'run (process-status proc))
	    (accept-process-output proc 5))
	  (setq result (process-exit-status proc))
	  ;; Hack to force a status_notify() in Emacs 19.29
	  (delete-process proc)
	  (set-buffer mybuf)
	  (goto-char (point-max))
	  (if (re-search-backward "\nProcess \\*PGP.*\n\\'" nil t)
	      (delete-region (match-beginning 0) (match-end 0)))
	  (goto-char (point-min))
	  ;; CRNL -> NL
	  (while (search-forward "\r\n" nil t)
	    (replace-match "\n"))
	  ;; Hurm.  FIXME; must get better result codes.
	  (if (stringp result)
	      (error "%s exited abnormally: '%s'" program result)
	    (setq rgn (funcall parser result))
	    ;; If the parser found something, migrate it
	    (if (consp rgn)
		(progn
		  (set-buffer obuf)
		  (if boundary
		      (save-restriction
			(narrow-to-region beg end)
			(goto-char beg)
			(insert (format "--%s\n" boundary))
			(goto-char (point-max))
			(insert (format "\n--%s
Content-Type: application/pgp-signature
Content-Transfer-Encoding: 7bit

" boundary))
			(insert-buffer-substring mybuf (car rgn) (cdr rgn))
			(goto-char (point-max))
			(insert (format "\n--%s--\n" boundary))
			)
		    (delete-region beg end)
		    (goto-char beg)
		    (insert-buffer-substring mybuf (car rgn) (cdr rgn))
		    )
		  (set-buffer mybuf)
		  (delete-region (car rgn) (cdr rgn)))))
	  ;; Return nil on failure and exit code on success
	  (if rgn result))
      ;; Cleanup even on nonlocal exit
      (if (and proc (eq 'run (process-status proc)))
	  (interrupt-process proc))
      (set-buffer obuf)
      (or buffer (null mybuf) (kill-buffer mybuf)))))

(defun tm:mc-pgp-sign-region (start end &optional id unclear boundary)
  (if (not (boundp 'mc-pgp-user-id))
      (load "mc-pgp")
    )
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	passwd args key
	(parser (function mc-pgp-generic-parser))
	(pgp-path mc-pgp-path)
	)
    (setq key (mc-pgp-lookup-key (or id mc-pgp-user-id)))
    (setq passwd
	  (mc-activate-passwd
	   (cdr key)
	   (format "PGP passphrase for %s (%s): " (car key) (cdr key))))
    (setenv "PGPPASSFD" "0")
    (setq args
	  (cons
	   (if boundary
	       "-fbast"
	     "-fast")
	   (list "+verbose=1" "+language=en"
		 (format "+clearsig=%s" (if unclear "off" "on"))
		 "+batchmode" "-u" (cdr key))))
    (if mc-pgp-comment
	(setq args (cons (format "+comment=%s" mc-pgp-comment) args))
      )
    (message "Signing as %s ..." (car key))
    (if (tm:mc-process-region
	 start end passwd pgp-path args parser buffer boundary)
	(progn
	  (if boundary
	      (progn
		(goto-char (point-min))
		(insert
		 (format "\
--[[multipart/signed; protocol=\"application/pgp-signature\";
 boundary=\"%s\"; micalg=pgp-md5][7bit]]\n" boundary))
		))
	  (message "Signing as %s ... Done." (car key))
	  t)
      nil)))

(defun mime-editor/sign-pgp-elkins (beg end boundary)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let* ((ret
	      (mime-editor/translate-region beg end boundary))
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
	(or (tm:mc-pgp-sign-region (point-min)(point-max)
				   nil nil pgp-boundary)
	    (throw 'mime-editor/error 'pgp-error)
	    )
	))))

(defun mime-editor/encrypt-pgp-elkins (beg end boundary)
  (save-excursion
    (save-restriction
      (let ((from (rfc822/get-field-body "From"))
	    (to (rfc822/get-field-body "To"))
	    (cc (rfc822/get-field-body "cc"))
	    recipients)
	(narrow-to-region beg end)
	(let* ((ret
		(mime-editor/translate-region beg end boundary))
	       (ctype    (car ret))
	       (encoding (nth 1 ret))
	       (parts    (nth 3 ret))
	       (pgp-boundary (concat "pgp-" boundary))
	       )
	  (goto-char beg)
	  (if (and (stringp from)
		   (not (string-equal from "")))
	      (insert (format "From: %s\n" from))
	    )
	  (if (and (stringp to)
		   (not (string-equal to "")))
	      (progn
		(insert (format "To: %s\n" to))
		(setq recipients to)
		))
	  (if (and (stringp cc)
		   (not (string-equal cc "")))
	      (progn
		(insert (format "cc: %s\n" cc))
		(if recipients
		    (setq recipients (concat recipients "," cc))
		  (setq recipients cc)
		  )))
	  (insert (format "Content-Type: %s\n" ctype))
	  (if encoding
	      (insert (format "Content-Transfer-Encoding: %s\n" encoding))
	    )
	  (insert "\n")
	  (if (null
	       (let ((mc-pgp-always-sign 'never))
		 (mc-pgp-encrypt-region
		  (mc-split "\\([ \t\n]*,[ \t\n]*\\)+" recipients)
		  (point-min) (point-max) from nil)
		 ))
	      (throw 'mime-editor/error 'pgp-error)
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

(defun mime-editor/sign-pgp-kazu (beg end boundary)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let* ((ret
	      (mime-editor/translate-region beg end boundary))
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
	(or (let ((program-coding-system-alist
		   (cons (cons (cons nil ".*pgp.*")
			       (cons *noconv* *noconv*))
			 program-coding-system-alist))
		  )
	      (mc-pgp-sign-region beg (point-max))
	      )
	    (throw 'mime-editor/error 'pgp-error)
	    )
	(goto-char beg)
	(insert
	 "--[[application/pgp; format=mime][7bit]]\n")
	))
    ))

(defun mime-editor/encrypt-pgp-kazu (beg end boundary)
  (save-excursion
    (let ((from (rfc822/get-field-body "From"))
	  (to (rfc822/get-field-body "To"))
	  (cc (rfc822/get-field-body "cc"))
	  recipients)
      (save-restriction
	(narrow-to-region beg end)
	(let* ((ret
		(mime-editor/translate-region beg end boundary))
	       (ctype    (car ret))
	       (encoding (nth 1 ret))
	       (parts    (nth 3 ret))
	       )
	  (goto-char beg)
	  (if (and (stringp from)
		   (not (string-equal from "")))
	      (insert (format "From: %s\n" from))
	    )
	  (if (and (stringp to)
		   (not (string-equal to "")))
	      (progn
		(insert (format "To: %s\n" to))
		(setq recipients to)
		))
	  (if (and (stringp cc)
		   (not (string-equal cc "")))
	      (progn
		(insert (format "cc: %s\n" cc))
		(if recipients
		    (setq recipients (concat recipients "," cc))
		  (setq recipients cc)
		  )))
	  (insert (format "Content-Type: %s\n" ctype))
	  (if encoding
	      (insert (format "Content-Transfer-Encoding: %s\n" encoding))
	    )
	  (insert "\n")
	  (or (let ((program-coding-system-alist
		     (cons (cons (cons nil ".*pgp.*")
				 (cons *noconv* *noconv*))
			   program-coding-system-alist))
		    )
		(mc-pgp-encrypt-region
		 (mc-split "\\([ \t\n]*,[ \t\n]*\\)+" recipients)
		 beg (point-max))
		)
	      (throw 'mime-editor/error 'pgp-error)
	      )
	  (goto-char beg)
	  (insert
	   "--[[application/pgp; format=mime][7bit]]\n")
	  ))
      )))

(defun mime-editor/translate-body ()
  "Encode the tagged MIME body in current buffer in MIME compliant message."
  (interactive)
  (save-excursion
    (let ((boundary
	   (concat mime-multipart-boundary "_"
		   (replace-space-with-underline (current-time-string))
		   ))
	  (i 1)
	  ret)
      (while (mime-editor/process-multipart-1
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
	  (setq ret (mime-editor/translate-region
		     beg end
		     (format "%s-%d" boundary i)))
	  ))
      (mime-editor/dequote-region (point-min)(point-max))
      (let ((contype (car ret))		;Content-Type
	    (encoding (nth 1 ret))	;Content-Transfer-Encoding
	    )
	;; Make primary MIME headers.
	(or (mail-position-on-field "Mime-Version")
	    (insert mime-editor/mime-version-value))
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

(defun mime-editor/translate-region (beg end &optional boundary multipart)
  (if (null boundary)
      (setq boundary
	    (concat mime-multipart-boundary "_"
		    (replace-space-with-underline (current-time-string))))
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
	(mime-editor/normalize-body)
	;; Counting the number of Content-Type.
	(goto-char (point-min))
	(while (re-search-forward mime-editor/single-part-tag-regexp nil t)
	  (setq nparts (1+ nparts)))
	;; Begin translation.
	(cond
	 ((and (<= nparts 1)(not multipart))
	  ;; It's a singular message.
	  (goto-char (point-min))
	  (while (re-search-forward
		  mime-editor/single-part-tag-regexp nil t)
	    (setq tag
		  (buffer-substring (match-beginning 0) (match-end 0)))
	    (delete-region (match-beginning 0) (1+ (match-end 0)))
	    (setq contype (mime-editor/get-contype tag))
	    (setq encoding (mime-editor/get-encoding tag))
	    ))
	 (t
	  ;; It's a multipart message.
	  (goto-char (point-min))
	  (if (re-search-forward
	       mime-editor/single-part-tag-regexp nil t)
	      (progn
		(setq tag
		      (buffer-substring
		       (match-beginning 0) (match-end 0)))
		(delete-region (match-beginning 0) (match-end 0))
		(setq contype (mime-editor/get-contype tag))
		(setq encoding (mime-editor/get-encoding tag))
		(insert "--" boundary "\n")
		(insert "Content-Type: " contype "\n")
		(if encoding
		    (insert "Content-Transfer-Encoding: " encoding "\n"))
		
		(while (re-search-forward
			mime-editor/single-part-tag-regexp nil t)
		  (setq tag
			(buffer-substring (match-beginning 0) (match-end 0)))
		  (delete-region (match-beginning 0) (match-end 0))
		  (setq contype (mime-editor/get-contype tag))
		  (setq encoding (mime-editor/get-encoding tag))
		  (insert "\n--" boundary "\n")
		  (insert "Content-Type: " contype "\n")
		  (if encoding
		      (insert "Content-Transfer-Encoding: " encoding "\n"))
		  )))
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
	  (if multipart
	      (insert "--" boundary "--\n")
	    (insert "\n--" boundary "--\n")
	    )))
	(list contype encoding boundary nparts)
	))))

(defun mime-editor/normalize-body ()
  "Normalize the body part by inserting appropriate message tags."
  ;; Insert the first MIME tags if necessary.
  (goto-char (point-min))
  (if (not (looking-at mime-editor/single-part-tag-regexp))
      (insert (mime-make-text-tag) "\n"))
  ;; Check each tag, and add new tag or correct it if necessary.
  (goto-char (point-min))
  (while (re-search-forward mime-editor/single-part-tag-regexp nil t)
    (let* ((tag (buffer-substring (match-beginning 0) (match-end 0)))
	   (contype (mime-editor/get-contype tag))
	   (charset (mime-get-parameter contype "charset"))
	   (encoding (mime-editor/get-encoding tag)))
      ;; Remove extra whitespaces after the tag.
      (if (looking-at "[ \t]+$")
	  (delete-region (match-beginning 0) (match-end 0)))
      (cond
       ((= (following-char) ?\^M)
	;; It must be image, audio or video.
	(let ((beg (point))
	      (end (mime-editor/content-end)))
	  ;; Insert explicit MIME tags after hidden messages.
	  (forward-line 1)
	  (if (and (not (eobp))
		   (not (looking-at mime-editor/single-part-tag-regexp)))
	      (progn
		(insert (mime-make-text-tag) "\n")
		(forward-line -1)	;Process it again as text.
		))
	  ;; Show a hidden message.  The point is not altered
	  ;; after the conversion.
	  (mime-flag-region beg end ?\n)
	  ))
       ((mime-test-content-type contype "message")
	;; Content-type "message" should be sent as is.
	(forward-line 1)
	)
       ((mime-test-content-type contype "text")
	;; Define charset for text if necessary.
	(setq charset (or charset (mime-editor/choose-charset)))
	(mime-editor/define-charset charset)
	(cond ((string-equal contype "text/x-rot13-47")
	       (save-excursion
		 (forward-line)
		 (set-mark (point))
		 (goto-char (mime-editor/content-end))
		 (tm:caesar-region)
		 ))
	      ((string-equal contype "text/enriched")
	       (save-excursion
		 (let ((beg (progn
			      (forward-line)
			      (point)))
		       (end (mime-editor/content-end))
		       )
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
		     (assoc charset
			    mime-editor/charset-default-encoding-alist)
		     ))
		   (beg (mime-editor/content-beginning))
		   )
	      (mime-charset-encode-region beg (mime-editor/content-end)
					  charset)
	      (mime-encode-region beg (mime-editor/content-end) encoding)
	      (mime-editor/define-encoding encoding)
	      ))
	(goto-char (mime-editor/content-end))
	)
       ((null encoding)		;Encoding is not specified.
	;; Application, image, audio, video, and any other
	;; unknown content-type without encoding should be
	;; encoded.
	(let* ((encoding "base64")	;Encode in BASE64 by default.
	       (beg (mime-editor/content-beginning))
	       (end (mime-editor/content-end))
	       (body (buffer-substring beg end))
	       )
	  (mime-encode-region beg end encoding)
	  (mime-editor/define-encoding encoding))
	(forward-line 1)
	)
       )
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

(defun mime-voice-recorder-for-sun ()
  "Record voice in a buffer using Sun audio device, and return the buffer.
If the environment variable AUDIOHOST is defined, its value is used as
a recording host instead of local host."
  (let ((buffer (get-buffer-create " *MIME audio*"))
	(host (getenv "AUDIOHOST")))
    (message "Start the recording on %s.  Type C-g to finish the recording..."
	     (or host (system-name)))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (condition-case errorcode
	  (let ((selective-display nil) ;Disable ^M to nl translation.
		(mc-flag nil)		;Mule
		(kanji-flag nil))	;NEmacs
	    ;; If AUDIOHOST is defined, use the value as recording host.
	    (cond ((not (null host))
		   ;; Disable automatic conversion of coding system if Mule.
		   (if (featurep 'mule)
		       (define-program-coding-system nil "rsh" *noconv*))
		   (call-process "rsh"
				 nil
				 buffer
				 nil
				 host
				 "cat"
				 "/dev/audio"
				 ))
		  (t
		   ;; Disable automatic conversion of coding system if Mule.
		   (if (featurep 'mule)
		       (define-program-coding-system nil "cat" *noconv*))
		   (call-process "cat"
				 "/dev/audio"
				 buffer
				 nil
				 ))))
	(quit (message "Type C-g to finish recording... done.")
	      buffer			;Return the buffer
	      )))))


;;; @ Other useful commands.
;;;

;; Message forwarding commands as content-type "message/rfc822".

(defun mime-editor/insert-message (&optional message)
  (interactive)
  (let ((inserter (assoc-value major-mode mime-editor/message-inserter-alist)))
    (if (and inserter (fboundp inserter))
	(progn
	  (mime-editor/insert-tag "message" "rfc822")
	  (funcall inserter message)
	  )
      (message "Sorry, I don't have message inserter for your MUA.")
      )))

(defun mime-editor/insert-mail (&optional message)
  (interactive)
  (let ((inserter (assoc-value major-mode mime-editor/mail-inserter-alist)))
    (if (and inserter (fboundp inserter))
	(progn
	  (mime-editor/insert-tag "message" "rfc822")
	  (funcall inserter message)
	  )
      (message "Sorry, I don't have mail inserter for your MUA.")
      )))

(defun mime-editor/inserted-message-filter ()
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
		     mime-editor/yank-ignored-field-regexp nil t)
		    (setq beg (match-beginning 0))
		    (setq end (1+ (rfc822/field-end)))
		    )
	  (delete-region beg end)
	  )
	))))


;;; @ multipart enclosure
;;;

(defun mime-editor/enclose-region (type beg end)
  (save-excursion
    (goto-char beg)
    (let ((current (point))
	  need-new-line)
      (or (bolp)
	  (setq need-new-line t))
      (save-restriction
	(narrow-to-region beg end)
	(if need-new-line
	    (insert "\n")
	  )
	(insert (format "--<<%s>>-{\n" type))
	(goto-char (point-max))
	(insert (format "\n--}-<<%s>>\n" type))
	(goto-char (point-max))
	)
      (if (and (not (looking-at mime-editor/single-part-tag-regexp))
	       (not (eobp)))
	  (insert (mime-make-text-tag) "\n")
	)
      )))

(defun mime-editor/enclose-mixed-region (beg end)
  (interactive "*r")
  (mime-editor/enclose-region "mixed" beg end)
  )

(defun mime-editor/enclose-parallel-region (beg end)
  (interactive "*r")
  (mime-editor/enclose-region "parallel" beg end)
  )

(defun mime-editor/enclose-digest-region (beg end)
  (interactive "*r")
  (mime-editor/enclose-region "digest" beg end)
  )

(defun mime-editor/enclose-alternative-region (beg end)
  (interactive "*r")
  (mime-editor/enclose-region "alternative" beg end)
  )

(defun mime-editor/enclose-signed-region (beg end)
  (interactive "*r")
  (if mime-editor/signing-type
      (mime-editor/enclose-region "signed" beg end)
    (message "Please specify signing type.")
    ))

(defun mime-editor/enclose-encrypted-region (beg end)
  (interactive "*r")
  (if mime-editor/signing-type
      (mime-editor/enclose-region "encrypted" beg end)
    (message "Please specify encrypting type.")
    ))

(defun mime-editor/insert-key (&optional arg)
  "Insert a pgp public key."
  (interactive "P")
  (mime-editor/insert-tag "application" "pgp-keys")
  (mime-editor/define-encoding "7bit")
  (mc-insert-public-key)
  )


;;; @ flag setting
;;;

(defun mime-editor/set-split (arg)
  (interactive
   (list
    (y-or-n-p "Do you want to enable split?")
    ))
  (setq mime-editor/split-message arg)
  (if arg
      (message "This message is enabled to split.")
    (message "This message is not enabled to split.")
    ))

(defun mime-editor/toggle-transfer-level (&optional transfer-level)
  "Toggle transfer-level is 7bit or 8bit through.

Optional TRANSFER-LEVEL is a number of transfer-level, 7 or 8."
  (interactive)
  (if (numberp transfer-level)
      (setq mime-editor/transfer-level transfer-level)
    (if (< mime-editor/transfer-level 8)
	(setq mime-editor/transfer-level 8)
      (setq mime-editor/transfer-level 7)
      ))
  (setq mime-editor/charset-default-encoding-alist
	(mime/make-charset-default-encoding-alist
	 mime-editor/transfer-level))
  (message (format "Current transfer-level is %d bit"
		   mime-editor/transfer-level))
  (setq mime-editor/transfer-level-string
	(mime/encoding-name mime-editor/transfer-level 'not-omit))
  (force-mode-line-update)
  )


;;; @ pgp
;;;

(defun mime-editor/set-sign (arg)
  (interactive
   (list
    (y-or-n-p "Do you want to sign?")
    ))
  (if arg
      (if mime-editor/signing-type
	  (progn
	    (setq mime-editor/pgp-processing 'sign)
	    (message "This message will be signed.")
	    )
	(message "Please specify signing type.")
	)
    (if (eq mime-editor/pgp-processing 'sign)
	(setq mime-editor/pgp-processing nil)
      )
    (message "This message will not be signed.")
    ))

(defun mime-editor/set-encrypt (arg)
  (interactive
   (list
    (y-or-n-p "Do you want to encrypt?")
    ))
  (if arg
      (if mime-editor/encrypting-type
	  (progn
	    (setq mime-editor/pgp-processing 'encrypt)
	    (message "This message will be encrypt.")
	    )
	(message "Please specify encrypting type.")
	)
    (if (eq mime-editor/pgp-processing 'encrypt)
	(setq mime-editor/pgp-processing nil)
      )
    (message "This message will not be encrypt.")
    ))

(defvar mime-editor/pgp-processing nil)
(make-variable-buffer-local 'mime-editor/pgp-processing)

(defun mime-editor/pgp-enclose-buffer ()
  (let ((beg (save-excursion
	       (goto-char (point-min))
	       (if (search-forward (concat "\n" mail-header-separator "\n"))
		   (match-end 0)
		 )))
	(end (point-max))
	)
    (if beg
	(cond ((eq mime-editor/pgp-processing 'sign)
	       (mime-editor/enclose-signed-region beg end)
	       )
	      ((eq mime-editor/pgp-processing 'encrypt)
	       (mime-editor/enclose-encrypted-region beg end)
	       ))
      )))


;;; @ split
;;;

(defun mime-editor/insert-partial-header
  (fields subject id number total separator)
  (insert fields)
  (insert (format "Subject: %s (%d/%d)\n" subject number total))
  (insert (format "Mime-Version: 1.0 (split by %s)\n"
		  mime-editor/version-name))
  (insert (format "\
Content-Type: message/partial; id=%s; number=%d; total=%d\n%s\n"
		  id number total separator))
  )

(defun mime-editor/split-and-send
  (&optional cmd lines mime-editor/message-max-length)
  (interactive)
  (or lines
      (setq lines
	    (count-lines (point-min) (point-max)))
      )
  (or mime-editor/message-max-length
      (setq mime-editor/message-max-length
	    (or (cdr (assq major-mode mime-editor/message-max-length-alist))
		mime-editor/message-default-max-length))
      )
  (let* ((mime-editor/draft-file-name 
	  (or (buffer-file-name)
	      (make-temp-name
	       (expand-file-name "tm-draft" mime/tmp-dir))))
	 (separator mail-header-separator)
	 (config
	  (eval (cdr (assq major-mode mime-editor/window-config-alist))))
	 (id (concat "\""
		     (replace-space-with-underline (current-time-string))
		     "@" (system-name) "\"")))
    (run-hooks 'mime-editor/before-split-hook)
    (let* ((header (rfc822/get-header-string-except
		    mime-editor/split-ignored-field-regexp separator))
	   (subject (mail-fetch-field "subject"))
	   (total (+ (/ lines mime-editor/message-max-length)
		     (if (> (mod lines mime-editor/message-max-length) 0)
			 1)))
	   (the-buf (current-buffer))
	   (buf (get-buffer "*tmp-send*"))
	   (command
	    (or cmd
		(cdr
		 (assq major-mode
		       mime-editor/split-message-sender-alist))
		(cdr
		 (assq major-mode
		       mime-editor/message-default-sender-alist))
		))
	   data)
      (goto-char (point-min))
      (if (re-search-forward (concat "^" (regexp-quote separator) "$")
			     nil t)
	  (replace-match "")
	)
      (if buf
	  (progn
	    (switch-to-buffer buf)
	    (erase-buffer)
	    (switch-to-buffer the-buf)
	    )
	(setq buf (get-buffer-create "*tmp-send*"))
	)
      (switch-to-buffer buf)
      (make-local-variable 'mail-header-separator)
      (setq mail-header-separator separator)
      (switch-to-buffer the-buf)
      (goto-char (point-min))
      (re-search-forward "^$" nil t)
      (let ((mime-editor/partial-number 1))
	(setq data (buffer-substring
		    (point-min)
		    (progn
		      (goto-line mime-editor/message-max-length)
		      (point))
		    ))
	(delete-region (point-min)(point))
	(switch-to-buffer buf)
	(mime-editor/insert-partial-header
	 header subject id mime-editor/partial-number total separator)
	(insert data)
	(save-excursion
	  (save-restriction
	    (goto-char (point-min))
	    (search-forward (concat "\n" mail-header-separator "\n"))
	    (narrow-to-region
	     (match-end 0)
	     (if (re-search-forward "^$" nil t)
		 (match-beginning 0)
	       (point-max)
	       ))
	    (goto-char (point-min))
	    (while (re-search-forward
		    mime-editor/split-blind-field-regexp nil t)
	      (delete-region (match-beginning 0)
			     (let ((e (rfc822/field-end)))
			       (if (< e (point-max))
				   (1+ e)
				 e)))
	      )
	    ))
	(save-excursion
	  (message (format "Sending %d/%d..."
			   mime-editor/partial-number total))
	  (call-interactively command)
	  (message (format "Sending %d/%d... done"
			   mime-editor/partial-number total))
	  )
	(erase-buffer)
	(switch-to-buffer the-buf)
	(setq mime-editor/partial-number 2)
	(while (< mime-editor/partial-number total)
	  (setq data (buffer-substring
		      (point-min)
		      (progn
			(goto-line mime-editor/message-max-length)
			(point))
		      ))
	  (delete-region (point-min)(point))
	  (switch-to-buffer buf)
	  (mime-editor/insert-partial-header
	   header subject id mime-editor/partial-number total separator)
	  (insert data)
	  (save-excursion
	    (message (format "Sending %d/%d..."
			     mime-editor/partial-number total))
	    (call-interactively command)
	    (message (format "Sending %d/%d... done"
			     mime-editor/partial-number total))
	    )
	  (erase-buffer)
	  (switch-to-buffer the-buf)
	  (setq mime-editor/partial-number
		(1+ mime-editor/partial-number))
	  )
	(goto-char (point-min))
	(mime-editor/insert-partial-header
	 header subject id mime-editor/partial-number total separator)
	(message (format "Sending %d/%d..."
			 mime-editor/partial-number total))
	))))

(defun mime-editor/maybe-split-and-send (&optional cmd)
  (interactive)
  (run-hooks 'mime-editor/before-send-hook)
  (let ((mime-editor/message-max-length
	 (or (cdr (assq major-mode mime-editor/message-max-length-alist))
	     mime-editor/message-default-max-length))
	(lines (count-lines (point-min) (point-max)))
	)
    (if (and (> lines mime-editor/message-max-length)
	     mime-editor/split-message)
	(mime-editor/split-and-send cmd lines mime-editor/message-max-length)
      )))


;;; @ preview message
;;;

(defun mime-editor/preview-message ()
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
    (setq major-mode 'mime/temporary-message-mode)
    (make-local-variable 'mail-header-separator)
    (setq mail-header-separator separator)
    (make-local-variable 'mime/editing-buffer)
    (setq mime/editing-buffer the-buf)
    
    (run-hooks 'mime-editor/translate-hook)
    (mime-editor/translate-buffer)
    (goto-char (point-min))
    (if (re-search-forward
	 (concat "^" (regexp-quote separator) "$"))
	(replace-match "")
      )
    (mime/viewer-mode)
    ))

(defun mime-editor/quitting-method ()
  (let ((temp mime::preview/article-buffer)
	buf)
    (mime-viewer/kill-buffer)
    (set-buffer temp)
    (setq buf mime/editing-buffer)
    (kill-buffer temp)
    (switch-to-buffer buf)
    ))

(set-alist 'mime-viewer/quitting-method-alist
	   'mime/temporary-message-mode
	   (function mime-editor/quitting-method)
	   )


;;; @ draft preview
;;; 
;; by "OKABE Yasuo <okabe@kudpc.kyoto-u.ac.jp>
;;	 Mon, 10 Apr 1995 20:03:07 +0900

(defvar mime-editor/draft-header-separator-alist
  '((news-reply-mode . mail-header-separator)
    (mh-letter-mode . mail-header-separator)
    ))

(defvar mime::article/draft-header-separator nil)

(defun mime-editor/draft-preview ()
  (interactive)
  (let ((sep (cdr (assq major-mode mime-editor/draft-header-separator-alist))))
    (or (stringp sep) (setq sep (eval sep)))
    (make-variable-buffer-local 'mime::article/draft-header-separator)
    (goto-char (point-min))
    (re-search-forward
     (concat "^\\(" (regexp-quote sep) "\\)?$"))
    (setq mime::article/draft-header-separator
	  (buffer-substring (match-beginning 0) (match-end 0)))
    (replace-match "")
    (mime/viewer-mode (current-buffer))
    (pop-to-buffer (current-buffer))
    ))

(defun mime-viewer::quitting-method/draft-preview ()
  (let ((mother mime::preview/mother-buffer))
    (save-excursion
      (switch-to-buffer mother)
      (goto-char (point-min))
      (if (and
	   (re-search-forward
	    (concat "^\\("
		    (regexp-quote mime::article/draft-header-separator)
		    "\\)?$") nil t)
	   (bolp))
	  (progn
	    (insert mime::article/draft-header-separator)
	    (set-buffer-modified-p (buffer-modified-p))
	    )))
    (mime-viewer/kill-buffer)
    (pop-to-buffer mother)
    ))

(set-alist 'mime-viewer/quitting-method-alist
	   'mh-letter-mode
	   (function mime-viewer::quitting-method/draft-preview)
	   )

(set-alist 'mime-viewer/quitting-method-alist
	   'news-reply-mode
	   (function mime-viewer::quitting-method/draft-preview)
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
	     ((string-equal type "multipart")
	      (let ((boundary (assoc-value "boundary" params)))
		(re-search-forward (concat "\n--" boundary) nil t)
		(let ((bb (match-beginning 0)) eb tag)
		  (setq tag (format "\n--<<%s>>-{" stype))
		  (goto-char bb)
		  (insert tag)
		  (setq bb (+ bb (length tag)))
		  (re-search-forward (concat "\n--" boundary "--") nil t)
		  (setq eb (match-beginning 0))
		  (replace-match (format "\n--}-<<%s>>" stype))
		  (save-restriction
		    (narrow-to-region bb eb)
		    (goto-char (point-min))
		    (while (re-search-forward
			    (concat "\n--" boundary "\n") nil t)
		      (let ((beg (match-beginning 0))
			    end)
			(delete-region beg (match-end 0))
			(save-excursion
			  (if (re-search-forward
			       (concat "\n--" boundary) nil t)
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
				   (if (re-search-forward "^$" nil t)
				       (match-end 0)
				     (point-min)
				     )))
		))
	     (t
	      (let* (charset
		     (pstr
		      (mapconcat (function
				  (lambda (attr)
				    (if (string-equal (car attr)
						      "charset")
					(progn
					  (setq charset (cdr attr))
					  "")
				      (concat ";" (car attr)
					      "=" (cdr attr))
				      )
				    ))
				 params ""))
		     encoding
		     encoded)
		(save-excursion
		  (if (re-search-forward
		       "Content-Transfer-Encoding:" nil t)
		      (let ((beg (match-beginning 0))
			    (hbeg (match-end 0))
			    (end (rfc822/field-end)))
			(setq encoding
			      (eliminate-top-spaces
			       (rfc822/unfolding-string
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
		    (if charset
			(mime-charset-decode-region (point-min)(point-max)
						    charset)
		      (character-decode-region (point-min)(point-max)
					       mime/default-coding-system)
		      ))
		(let ((he
		       (if (re-search-forward "^$" nil t)
			   (match-end 0)
			 (point-min)
			 )))
		  (if (= (point-min) 1)
		      (progn
			(goto-char he)
			(insert
			 (concat
			  "\n"
			  (mime-create-tag
			   (concat type "/" stype pstr) encoding)
			  ))
			)
		    (delete-region (point-min) he)
		    (insert
		     (concat "\n"
			     (mime-create-tag
			      (concat type "/" stype pstr) encoding)
			     ))
		    ))
		))))
	(if code-conversion
	    (character-decode-region (point-min) (point-max)
				     mime/default-coding-system)
	  )
	))))

(defun mime/edit-again (&optional code-conversion no-separator no-mode)
  (interactive)
  (mime-editor::edit-again code-conversion)
  (goto-char (point-min))
  (save-restriction
    (narrow-to-region (point-min)
		      (if (re-search-forward "^$" nil t)
			  (match-end 0)
			(point-max)
			))
    (goto-char (point-min))
    (while (re-search-forward
	    "^\\(Content-.*\\|Mime-Version\\):" nil t)
      (delete-region (match-beginning 0) (1+ (rfc822/field-end)))
      ))
  (or no-separator
      (and (re-search-forward "^$")
	   (replace-match mail-header-separator)
	   ))
  (or no-mode
      (mime/editor-mode)
      ))


;;; @ end
;;;

(provide 'mime-edit)

(run-hooks 'mime-edit-load-hook)

;;; mime-edit.el ends here
