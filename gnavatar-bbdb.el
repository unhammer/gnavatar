;;; gnavatar-bbdb.el --- Add avatars to BBDB entries -*- lexical-binding: t -*-

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

;;; Helpers for adding image-uri's to BBDB entries from clipboard,
;;; Github or URLs.  Requires ImageMagick (convert) for resizing
;;; clipboard/URL contents; uses Github avatars unresized.

;;; Interactive functions, call them in BBDB on a record:
;;; gnavatar-bbdb-add-clipboard-image-contents
;;; gnavatar-bbdb-add-clipboard-image-url
;;; gnavatar-bbdb-add-github-image

;;; Code:

(require 'bbdb)
(require 'bbdb-com)

(defcustom gnavatar-bbdb-image-size 48
  "What size to resize the image to before storing in image-uri."
  :group 'gnavatar
  :type 'integer)

;;;###autoload
(defun gnavatar-bbdb-add-clipboard-image-contents (record)
  "Add image-uri to RECORD using clipboard contents."
  (interactive
   (let* ((_ (bbdb-editable))
          (record (or (bbdb-current-record)
                      (error "Point not on a record")))
          (completion-ignore-case t))
     (list record)))
  (let* ((file (make-temp-file "clip" nil ".png"))
         (selection-coding-system 'no-conversion)
         (coding-system-for-write 'binary))
    (write-region (or (gui-get-selection 'CLIPBOARD 'image/png)
                      (error "No image in CLIPBOARD"))
                  nil file nil 'quiet)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (set-buffer-multibyte nil)
      (gnavatar-bbdb-add-buffer-image-via-jpg (list record)))))

;;;###autoload
(defun gnavatar-bbdb-add-clipboard-image-url (record)
  "Add image-uri field to RECORD using clipboard contents."
  (interactive
   (let* ((_ (bbdb-editable))
          (record (or (bbdb-current-record)
                      (error "Point not on a record")))
          (completion-ignore-case t))
     (list record)))
  (gnavatar-bbdb-add-image-from-url (list record) (current-kill 0)))

(defvar gnavatar-bbdb-url-timeout 5)

(defun gnavatar-bbdb-add-image-from-url (records url)
  "Retrieve URL image link, store in image-uri of RECORDS in BBDB."
  (lexical-let* ((field 'image-uri)
                 (timeout gnavatar-bbdb-url-timeout))
    (with-current-buffer (url-retrieve-synchronously url
                                                     'silent
                                                     'inhibit-cookies
                                                     timeout)
      (goto-char (point-min))
      (if (and (re-search-forward "^Content-Type: image/\\(.*\\)$" nil 'noerror)
               (search-forward "\n\n" nil 'noerror))
          (progn
            (narrow-to-region (match-end 0) (point-max))
            (gnavatar-bbdb-add-buffer-image-via-jpg records))
        (message "Couldn't find image at url %s" url)))))

(defun gnavatar-bbdb-add-buffer-image-via-jpg (records)
  "Convert image data of current buffer to JPG and add to RECORDS."
  (shell-command-on-region (point-min) (point-max)
                           (format "convert -quality 70 -resize %dx - jpeg:-"
                                   gnavatar-bbdb-image-size)
                           nil
                           'replace)
  (gnavatar-bbdb-add-image-uri records
                         (buffer-substring-no-properties (point-min) (point-max))))

(defun gnavatar-bbdb-add-image-uri (records img)
  "Add IMG to each record of RECORDS.
The image will be base64-encoded and stored in the image-uri field."
  (let ((field 'image-uri)
        (img64 (base64-encode-string img 'no-line-break)))
    (mapcar (lambda (r)
              (if (bbdb-record-field r field)
                  (message "%s already has an %s, not setting"
                           (bbdb-record-field r 'name)
                           field)
                (message "Setting %s of %s"
                         field
                         (bbdb-record-field r 'name))
                (bbdb-insert-field r field img64)))
            records)))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding from Github ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun gnavatar-bbdb-add-github-image (record github-username)
  "Add image-uri field to RECORD using GITHUB-USERNAME.
If called interactively, get GITHUB-USERNAME from RECORD if it's
there, otherwise ask for it."
  (interactive
   (let* ((_ (bbdb-editable))
          (record (or (bbdb-current-record)
                      (error "Point not on a record")))
          (completion-ignore-case t))
     (list record
           (or (bbdb-record-field record 'github)
               (let ((u (bbdb-read-field record 'github current-prefix-arg)))
                 (bbdb-insert-field record 'github u)
                 u)))))
  (gnavatar-bbdb-image-from-github (list record) github-username))

(defun gnavatar-bbdb-image-from-github (records-to-set github-username)
  "Snatch avatar of GITHUB-USERNAME and put into RECORDS-TO-SET in BBDB."
  (lexical-let* ((field 'image-uri)
                 (records (cl-remove-if
                           (lambda (r)
                             (when (bbdb-record-field r field)
                               (message "%s already has an %s, not setting"
                                        (bbdb-record-field r 'name)
                                        field)
                               t))
                           records-to-set)))
    (if records
        (gnavatar-github-username-retrieve
         github-username
         (lambda (img)
           (unless (eq img 'error)
             (gnavatar-bbdb-add-image-uri records (image-property img :data)))))
      (message "None of the records were missing image-uri"))))

(provide 'gnavatar-bbdb)
;;; gnavatar-bbdb.el ends here
