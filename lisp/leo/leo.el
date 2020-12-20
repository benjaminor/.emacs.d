;;; leo.el --- Quickly translate words using the dict.leo.org website -*- lexical-binding: t -*-

;; Copyright (C) 2020  Benjamin Orthen

;; Package-Requires: ((emacs "26.3") (request "0.3.2") (s "1.12.0") (dash "2.17.0") (esxml "0.3.5"))
;; URL: https://github.com/benjaminor/TBD
;; Version: 0.1
;; Keywords: languages, util, translate

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Road-map
;; 1. Make requests for one word and make response pop-up in extra buffer (messages or pop-up buffer, killable with q)
;; 2. Provide requests for word under cursor
;; 3. Add helm-interface
;; Examples of requests:
;; one-sided request: https://dict.leo.org/dictQuery/m-vocab/ende/query.xml?lp=ende&lang=en&search=love&side=left&order=basic&partial=show&sectLenMax=16&n=2&filtered=-1&trigger=null
;; one-sided request: https://dict.leo.org/dictQuery/m-vocab/ende/query.xml?lp=ende&lang=en&search=liebe&side=right&order=basic&partial=show&sectLenMax=16&n=2&filtered=-1&trigger=null
;; http://dict.leo.org/dictQuery/m-vocab/ende/query.xml?tolerMode=nof&lp=ende&lang=de&rmWords=off&rmSearch=on&search=test&searchLoc=0&resultOrder=basic&multiwordShowSingle=on&pos=0&sectLenMax=10&n=1"
;; http://dict.leo.org/dictQuery/m-query/conf/ende/query.conf/strlist.json?q=short&sort=PLa&shortQuery&noDescription&sideInfo=on&where=0&term=short


;;; Code:

(require 'request)
(require 'esxml-query)
(require 'dash)
(require 's)


(defgroup leo nil
  "Translation service using API from dict.leo.org"
  :group 'convenience
  :prefix "leo-")

(defcustom leo-result-order "basic"
  "Specify the order for the translation result."
  :type 'string
  :group 'leo)

(defcustom leo-supported-languages
  '((english "en")
	(spanish "es")
	(german "de")
	(french "fr")
	(italian "it")
	(polish "pl")
	(russian "ru")
	(chinese "ch")
	(portuguese "pt"))
  "Define possible translation offered by leo."
  :type '(alist
		  :key-type (symbol :tag "Name of supported language")
		  :value-type (string :tag "Internal language abbreviation of leo API"))
  :group 'leo)

(defcustom leo-supported-translations
  '((english-german "ende")
	(spanish-german "esde")
	(french-german "frde")
	(english-spanish "enes")
	(italian-german  "itde")
	(polish-german  "plde")
	(russian-german  "rude")
	(chinese-german  "chde")
	(portuguese-german  "ptde"))
  "Define possible translation offered by leo."
  :type '(alist
		  :key-type (symbol :tag "Name of translation service")
		  :value-type (string :tag "Internal translation abbreviation of leo API"))
  :group 'leo)

(defcustom leo-process-response-fun
  'leo--print-buffer-response
  "Function to use to print response.
Has to be one of ('leo--message-response 'leo--print-buffer-response)"
  :type 'command
  :group 'leo)

(defcustom leo-default-translation 'english-german
  "The language to use for the translation."
  :type 'symbol
  :group 'leo)

(defcustom leo-multi-word-show-single t
  "Show a single word for a multiword result."
  :type 'boolean
  :group 'leo)

(defcustom leo-maximal-responses-per-category 5
  "Specify how many responses per category (noun, adjective, ...) are to be shown."
  :type 'integer
  :group 'leo)

(defcustom leo-sections-not-to-display
  '("Examples")
  "List of sections not to display when printing response."
  :type 'list
  :group 'leo)

(defconst leo-base-url "https://dict.leo.org/dictQuery/m-vocab/%s/query.xml?lp=%s")

(defun leo--get-language-abbreviation (lang)
  "Get the leo API abbreviation for the language LANG."
  (cadr (assoc lang leo-supported-languages)))

(defun leo--get-translation-service (langto langfrom)
  "Get the applicable, if existing string for the translation service.
LANGTO and LANGFROM are used to determine the service."
  (let ((firstlang (symbol-name langto))
		(secondlang (symbol-name langfrom)))
	(if-let ((trans (assoc (intern (concat firstlang "-" secondlang)) leo-supported-translations)))
		(cadr trans)
	  (cadr (assoc (intern (concat secondlang "-" firstlang)) leo-supported-translations)))))

(defun leo--get-API-side-for-language (translation-service lang)
  "Get side 'right' or 'left' for LANG corresponding to TRANSLATION-SERVICE."
  (if (s-starts-with? (leo--get-language-abbreviation lang) translation-service) "left" "right"))

(defun leo--base-request-url-additional-parameters (request-url)
  "Add additional parameters to the REQUEST-URL."
  (concat request-url
		  (if leo-result-order
			  (concat "&resultOrder=" leo-result-order))
		  (if leo-multi-word-show-single
			  "&multiWordShowSingle=on")))

(defun leo--build-request-url (word langto langfrom &optional no-reverse-translation)
  "Create URL to send as request.
WORD is the word which should be translated.
LANGTO and LANGFROM specifiy the languages to be used for as translation.
If NO-REVERSE-TRANSLATION is not specified, translations from LANGTO to LANGFROM
are also considered."
  (when-let* ((translation-service (leo--get-translation-service langto langfrom))
			  (base-url (format leo-base-url translation-service translation-service))
			  (specify-lang-from-in-url (if no-reverse-translation (concat "&side=" (leo--get-API-side-for-language translation-service langfrom)) ""))
			  (base-url-with-word (concat base-url "&search=" (url-hexify-string word) specify-lang-from-in-url)))
	(leo--base-request-url-additional-parameters base-url-with-word)))

(defun leo--parse-response (xml-doc)
  "Parse the XML-DOC response sent for a certain translation.
The result is a list of plists, one for each section of the response."
  (let ((sections (esxml-query-all "sectionlist section" xml-doc)))
	(seq-map #'leo--extract-information-from-section sections)))

(defun leo--extract-information-from-section (section)
  "Query the SECTION xml-node to look for the necessary information.
This information contains the section title, number and the translation pairs."
  (list 'section-title (esxml-node-attribute 'sctTitle section)
		'section-number (esxml-node-attribute 'sctnum section)
		'translation-pairs (let* ((entries (esxml-query-all "entry" section)))
							 (seq-map (lambda (entry)
										(let* ((sides (esxml-query-all "side" entry))
											   (first-side (car sides))
											   (second-side (cadr sides))
											   (first-words-node (esxml-query-all "words word" first-side))
											   (second-words-node (esxml-query-all "words word" second-side))
											   (first-words (mapconcat (lambda (it) (car (last (esxml-node-children it)))) first-words-node ", "))
											   (second-words (mapconcat (lambda (it) (car (last (esxml-node-children it)))) second-words-node ", ")))
										  (list
										   first-words second-words)))
									  entries))))

(defun leo--message-response (parsed-response)
  "Pretty-print PARSED-RESPONSE into echo area.
This tries to use as little space as possible while remaining concise.
Sections specified in leo-sections-not-to-display are ignored."
  (let* ((section-output (--map-when (not (-contains? leo-sections-not-to-display (plist-get it 'section-title)))
									 (concat "=== " (s-upcase (plist-get it 'section-title)) " ===:  "
											 (s-join " || " (-map (lambda (x)(s-join " : " x))
																  (plist-get it 'translation-pairs))))
									 parsed-response))
		 (output (s-join "\n" (-non-nil section-output))))
	(message nil)
	(message output)))

(defun leo--print-buffer-response (parsed-response)
  "Pretty-print PARSED-RESPONSE into a pop-up buffer."
  (let* ((section-output (--map (let ((title (plist-get it 'section-title)))
								  (concat "=== " (s-upcase title) " ===\n"
										  (mapconcat (lambda (x) (s-join " : " x)) (plist-get it 'translation-pairs) "\n")))
								parsed-response))

		 (output (s-join "\n\n" (-non-nil section-output))))
	(with-output-to-temp-buffer "*leo-trans*"
      (princ output))
	(switch-to-buffer-other-window "*leo-trans*")))

(cl-defun leo--process-response (parsed-response)
  "Call function to process PARSED-RESPONSE."
  (funcall leo-process-response-fun parsed-response))

(cl-defun leo-translate-word (word langto langfrom &optional (no-reverse-translation nil))
  "Translate WORD from LANGTO to LANGFROM.
Translate also from LANGFROM to LANGTO if NO-REVERSE-TRANSLATION is nil."
  (if-let ((request-url (leo--build-request-url word langto langfrom no-reverse-translation)))
	  (request
		request-url
		:parser (lambda () (libxml-parse-xml-region (point) (point-max)))
		:success (cl-function (lambda (&key data &allow-other-keys)
								(leo--process-response (leo--parse-response data)))))
	(message "Requesting translation for unsupported languages or translation service.")))

(cl-defun leo-translate-word-at-point-or-prompt (langto langfrom &optional (no-reverse-translation nil))
  "Translate word at point or word provided by prompt if non-nil prefix.
Translate from LANGTO to LANGFROM.
Translate also from LANGFROM to LANGTO if NO-REVERSE-TRANSLATION is nil."
  (interactive "P")
  (if current-prefix-arg
	  (progn
		(leo-translate-word (read-string "Enter word to translate: ") langto langfrom no-reverse-translation))
	(leo-translate-word (current-word nil t) langto langfrom no-reverse-translation)))


(provide 'leo)
;;; leo.el ends here
