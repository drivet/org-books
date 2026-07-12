;;; org-books.el --- Reading list management with Org mode and helm   -*- lexical-binding: t -*-

;; Copyright (C) 2026 Desmond Rivet

;; Author: Desmond Rivet <desmond.rivet@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((enlive "0.0.1") (s "1.11.0") (dash "2.14.1") (org "9.3") (emacs "25"))
;; URL: https://github.com/drivet/org-books
;; Keywords: outlines

;;; Commentary:

;; org-books.el is a tool for managing reading list in an Org mode file.  Forked from
;; https://github.com/lepisma/org-books
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'enlive)
(require 'json)
(require 'org)
(require 's)
(require 'subr-x)
(require 'url)
(require 'url-parse)

(defgroup org-books nil
  "Org reading list management."
  :group 'org)

(defcustom org-books-url-host-pattern-dispatches
  '(("^\\(www\\.\\)?amazon\\." . org-books-get-details-amazon)
    ("^\\(www\\.\\)?goodreads\\.com" . org-books-get-details-goodreads)
    ("openlibrary\\.org" . org-books-get-details-olb))
  "Pairs of url patterns and functions taking url and returning
book details. Check documentation of `org-books-get-details' for
return structure from these functions."
  :type '(alist :key-type string :value-type symbol)
  :group 'org-books)

(defun org-books--get-json (url)
  "Parse JSON data from given URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "^$")
    (json-read)))

(defun org-books--clean-str (text)
  "Clean TEXT to remove extra whitespaces."
  (s-trim (s-collapse-whitespace text)))

(defun org-books-get-details-amazon-authors (page-node)
  "Return author names for amazon PAGE-NODE.

PAGE-NODE is the return value of `enlive-fetch' on the page url."
  (or (mapcar #'enlive-text (enlive-query-all page-node [.a-section .author .contributorNameID]))
      (mapcar #'enlive-text (enlive-query-all page-node [.a-section .author > a]))))

(defun org-books-get-details-amazon (url)
  "Get book details from amazon URL."
  (let* ((page-node (enlive-fetch url))
         (title (org-books--clean-str (enlive-text (enlive-get-element-by-id page-node "productTitle"))))
         (author (s-join ", " (org-books-get-details-amazon-authors page-node))))
    (if (not (string-equal title ""))
        (list title author `() "" url))))

(defun org-books-get-details-goodreads (url)
  "Get book details from Goodreads URL."
  (let* ((page-node (enlive-fetch url))
          (title (org-books--clean-str (enlive-text (enlive-query page-node [.Text__title1]))))
          (author (org-books--clean-str
                    (s-join ", " (mapcar #'enlive-text (enlive-query-all page-node [.ContributorLink__name] ))))))
    (if (not (string-equal title ""))
        (list title author `() "" url))))

(defun org-books-get-url-from-isbn (isbn)
  "Make and return openlibrary url from ISBN."
  (concat "https://openlibrary.org/api/books?bibkeys=ISBN:" isbn "&jscmd=data&format=json"))

(defun create-isbn-cover-image-url (isbn)
  (format "https://covers.openlibrary.org/b/ISBN/%s-M.jpg" isbn))

(defun org-books-create-isbn-content (isbn)
  (format "#+BEGIN_aside\n#+ATTR_HTML: :loading lazy\n[[%s]]\n#+END_aside"
          (create-isbn-cover-image-url isbn)))

(defun org-books-get-details-isbn (url)
  "Get book details from Open Library ISBN response from URL."
  (message "ISBN %s" url)
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (org-books--get-json url))
         (isbn (car (hash-table-keys json)))
         (data (gethash isbn json))
         (title (gethash "title" data))
         (author (gethash "name" (car (gethash "authors" data))))
         (pageurl (gethash "url" data))
         (rawisbn (substring isbn 5)))
    (list title author `(("ISBN" . ,rawisbn))
          (org-books-create-isbn-content rawisbn) pageurl)))

(defun org-books-get-details-olb (url)
  "Get book details from Open Library book URL."
  (let* ((match-index (string-match "\\(https://openlibrary\\.org/books/[[:alnum:]]+\\)/" url))
         (m (match-string 1 url))
         (jsonurl (format "%s.json" m))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (org-books--get-json jsonurl))
         (isbn (or (car (gethash "isbn_13" json)) (car (gethash "isbn_10" json)))))
    (org-books-get-details-isbn (org-books-get-url-from-isbn isbn))))

(defun org-books-get-details (url)
  "Fetch book details from given URL.

Return a list of three items: title (string), author (string) and
an alist of properties to be applied to the org entry.  If the url
is not supported, throw an error."
  (let ((output 'no-match)
        (url-host-string (url-host (url-generic-parse-url url))))
    (cl-dolist (pattern-fn-pair org-books-url-host-pattern-dispatches)
      (when (s-matches? (car pattern-fn-pair) url-host-string)
        (setq output (funcall (cdr pattern-fn-pair) url))
        (cl-return)))
    (if (eq output 'no-match)
        (error (format "Url %s not understood" url))
      output)))

(defun org-books-format (title author &optional props content url)
  "Return formatted details as an org headline entry.

If both TITLE and URL, the headline is a link.
If only TITLE is defined, the headline is just the TITLE.
AUTHOR and properties from PROPS go as org-property.  CONTENT is
the actual content below"
  (with-temp-buffer
    (org-mode)
    (if (null url)
      (insert "* " title "\n")
      (let ((headline (format "[[%s][%s]]" url title)))
        (insert "* " headline "\n")))
    (org-set-property "Author" author)
    (org-set-property "Added" (format-time-string "%Y-%02m-%02d"))
    (dolist (prop props)
      (org-set-property (car prop) (cdr prop)))
    (if content
      (insert "\n" content "\n"))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-books--insert (title author &optional props content url)
  "Insert book template at current position in buffer.

Formatting is specified by TITLE, AUTHOR, PROPS and CONTENT as
described in docstring of `org-books-format' function."
  (insert (org-books-format title author props content url)))

;;;###autoload
(defun org-books-add-url (url)
  "Add book from web URL."
  (interactive "sUrl: ")
  (let ((details (org-books-get-details url)))
    (if (null details)
        (message "Error in fetching url. Please retry.")
      (apply #'org-books--insert details))))

;;;###autoload
(defun org-books-cliplink ()
  "Clip link from clipboard."
  (interactive)
  (let ((url (substring-no-properties (current-kill 0))))
    (org-books-add-url url)))

;;;###autoload
(defun org-books-cliplink-capture ()
  "Clip link from clipboard."
  (interactive)
  (let* ((url (substring-no-properties (current-kill 0)))
					(details (org-books-get-details url)))
     (if (null details)
        (message "Error in fetching url. Please retry.")
       (apply #'org-books-format details))))

;;;###autoload
(defun org-books-add-isbn (isbn)
  "Add book from ISBN."
  (interactive "sISBN: ")
  (let ((details (org-books-get-details-isbn (org-books-get-url-from-isbn isbn))))
    (if (null details)
        (message "Error in fetching url. Please retry.")
      (apply #'org-books--insert details))))

;;;###autoload
(defun org-books-rate-book (rating)
  "Apply RATING to book at current point."
  (interactive "nRating (stars 1-5): ")
  (if (> rating 0)
      (org-set-property "Rating" (s-repeat rating ":star:"))))

(provide 'org-books)
;;; org-books.el ends here
