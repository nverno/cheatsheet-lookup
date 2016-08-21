;;; cheatsheet-lookup --- Emacs API to lookup cheatsheets from cheat-sheets.org

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/cheatsheet-lookup
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created: 20 August 2016

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Description:

;; Lookup cheatsheets at cheat-sheets.org from Emacs.  This interface uses
;; ido-completion to prompt for input.

;;; Installation:

;; `cheatsheet-lookup' is the only interactive function.  So just autoload it or
;; require this file.

;;; Code:

(defgroup cheatsheet-lookup nil
  "Lookup cheatsheets from cheat-sheet.org"
  :group 'convenience
  :prefix "cheatsheet-lookup-")

(defcustom cheatsheet-lookup-data-location
  (expand-file-name "cheatsheet-lookup-data.el" ".")
  "Location of lookup data."
  :group 'cheatsheet-lookup
  :type 'file)

;; ------------------------------------------------------------
;;* Load data
(defvar cheatsheet-lookup-data nil)

(defun cheatsheet-lookup-alist-to-hash (alist &optional options)
  (let ((ht (apply 'make-hash-table options)))
    (mapc
     #'(lambda (item) (puthash (car item) (cdr item) ht)) alist)
    (setq cheatsheet-lookup-data ht)
    ht))

(defun cheatsheet-lookup-load-hash (file)
  (cheatsheet-lookup-alist-to-hash
   (with-temp-buffer
     (insert-file-contents file)
     (car (read-from-string (buffer-substring-no-properties
                             (point-min) (point-max)))))
   '(:test equal :size 300)))

(defun cheatsheet-lookup-load-alist (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((data (car
                 (read-from-string
                  (buffer-substring-no-properties (point-min) (point-max))))))
      (setq cheatsheet-lookup-data data)))
  cheatsheet-lookup-data)

;;;###autoload
(defun cheatsheet-lookup (&optional arg)
  "Lookup a cheatsheet. With ARG or prefix, jump to the location of a selected
language/resource at 'http://cheat-sheets.org instead'."
  (interactive "P")
  (let* ((data (or cheatsheet-lookup-data
                   (cheatsheet-lookup-load-alist cheatsheet-lookup-data-location)))
         (lang (ido-completing-read "Language: " data))
         (subdat (assoc lang data)))
    (if (or arg current-prefix-arg)
        ;; Just go to section corresponding to chosen language
        (browse-url (concat "http://cheat-sheets.org/#" (cdr (assoc "id" subdat))))
      (let* ((sheets (cdr (assoc "sheets" subdat)))
             (sheet (ido-completing-read "Cheatsheet: " sheets))
             (uris (cdr (assoc "hrefs" (cdr (assoc sheet sheets))))))
        (if (= 1 (length uris))
            (browse-url (aref uris 0))
          (let ((uri (ido-completing-read "Location: " (append uris nil))))
            (browse-url uri)))))))

(provide 'cheatsheet-lookup)

;;; cheatsheet-lookup.el ends here
