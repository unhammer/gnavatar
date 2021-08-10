;;; gnavatar.el --- Gather avatars from various sources -*- lexical-binding: t -*-

;; Copyright (C) 2021 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/unhammer/gnavatar
;; Keywords: mail, faces, hypermedia

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Currently just a wrapper around gravatar that adds support for
;;; BBDB image-uri's; could one day become a general avatar-source.

;;; Meant to be used as a library.


;;; Code:

(require 'gravatar)
(require 'bbdb)
(require 'bbdb-com)

(defgroup gnavatar nil
  "Options for gnavatar."
  :tag "gnavatar"
  :group 'comm)

(defcustom gnavatar-providers (list #'gravatar-retrieve
                                    #'gnavatar-bbdb-retrieve)
  "Prioritised list of avatar providers.
They should accept the same arguments as `gravatar-retrieve', and
return the symbol 'error in order to move to the next one in the list."
  :group 'gnavatar
  :type '(list function))


;;;;;;;;;;;;;;;;;;;
;; BBDB provider ;;
;;;;;;;;;;;;;;;;;;;

(defun gnavatar-bbdb-create-image (record)
  "Create an image from RECORD, if it has an image-uri."
  (when-let ((image-uri (bbdb-record-xfield record 'image-uri)))
    (create-image (base64-decode-string image-uri)
                  nil                 ; try to guess format
                  'data)))

(defun gnavatar-bbdb-find-image (mail)
  "Get image from BBDB record with e-mail MAIL."
  (when-let ((records (bbdb-search (bbdb-records)
                                   :mail (regexp-quote mail))))
    ;; bbdb prefers at most one record per mail, probably no need to
    ;; support multiple matches
    (gnavatar-bbdb-create-image (car records))))

(defun gnavatar-bbdb-retrieve (mail-address callback &optional cbargs)
  "Get image from BBDB record with e-mail MAIL-ADDRESS.
CALLBACK and CBARGS as in `gravatar-retrieve'."
  (if-let ((image (gnavatar-bbdb-find-image mail-address)))
      (apply callback image cbargs)
    'error))


;;;;;;;;;;;;;;;;;;;;;;
;; Gnavatar workers ;;
;;;;;;;;;;;;;;;;;;;;;;

(defvar gnavatar-calling nil)

(defun gnavatar-override (orig-fn &rest rest)
  "Apply `gnavatar-retrieve' to REST.
Or use ORIG-FN if we were called by `gnavatar-retrieve' (to avoid
a loop if gnavatar wants to use the function we're overriding as
a provider).  Meant to be used as around-advice for
`gravatar-retrieve', e.g. (advice-add 'gravatar-retrieve :around
#'gnavatar-override)."
  (if gnavatar-calling
      (apply orig-fn rest)
    (apply #'gnavatar-retrieve rest)))

(defun gnavatar-work (result search-term providers callback &optional cbargs)
  "Callback wrapper.
If RESULT is 'error, we try the next of PROVIDERS with
SEARCH-TERM, else call CALLBACK with RESULT and CBARGS.  We wrap
the callback to the provider so it'll try the next one if that
one doesn't work."
  (if (eq result 'error)
      (if providers
          (apply (car providers)        ; gravatar-retrieve
                 search-term            ; mail-address
                 (list
                  (lambda (i) (gnavatar-work i search-term (cdr providers) callback cbargs))))
        result)
    (apply callback result cbargs)))

;;;###autoload
(defun gnavatar-retrieve (mail-address callback &optional cbargs)
  "Asynchronously retrieve an avatar for MAIL-ADDRESS.
When finished, call CALLBACK as (apply CALLBACK AVATAR CBARGS),
where AVATAR is either an image descriptor, or the symbol
`error' if the retrieval failed.

Note: Providers may change the current buffer, so use
`with-current-buffer' to insert into the buffer you're in when
calling this."
  (let ((gnavatar-calling t))
    (gnavatar-work 'error ; we haven't got a result yet, so try the next
                   mail-address
                   gnavatar-providers
                   callback
                   cbargs)))

(provide 'gnavatar)
;;; gnavatar.el ends here
