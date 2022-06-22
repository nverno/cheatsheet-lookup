;;; cheatsheet-lookup --- Emacs API to lookup cheatsheets from cheat-sheets.org -*- lexical-binding: t -*-
;;
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/cheatsheet-lookup
;; Package-Requires:
;; Created: 20 August 2016
;;
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

;; Lookup cheatsheets online from emacs.

;;; Code:

(defgroup cheatsheet-lookup nil
  "Lookup cheatsheets from cheat-sheet.org."
  :group 'convenience
  :prefix "cheatsheet-lookup-")

(defvar cheatsheet-lookup-data-dir nil)
(setq cheatsheet-lookup-data-dir
      (when load-file-name
        (expand-file-name "data" (file-name-directory load-file-name))))

(defcustom cheatsheet-lookup-completing-read 'completing-read
  "Completing read function."
  :group 'cheatsheet-lookup
  :type 'function)

;; ------------------------------------------------------------
;;* Load data
(defvar cheatsheet-lookup-cache nil)

(defun cheatsheet-lookup-alist-to-hash (alist &optional options)
  "Convert ALIST to hash. OPTIONS passed to hash."
  (let ((ht (apply 'make-hash-table options)))
    (mapc
     #'(lambda (item) (puthash (car item) (cdr item) ht)) alist)
    (setq cheatsheet-lookup-cache ht)
    ht))

(defun cheatsheet-lookup-load-hash (file)
  "Load hash from FILE."
  (cheatsheet-lookup-alist-to-hash
   (with-temp-buffer
     (insert-file-contents file)
     (car (read-from-string (buffer-substring-no-properties
                             (point-min) (point-max)))))
   '(:test equal :size 300)))

(defun cheatsheet-lookup-load (&optional arg)
  "Load cheatsheets. Optionally force load with ARG."
  (when (or arg (not cheatsheet-lookup-cache))
    (dolist (file (directory-files cheatsheet-lookup-data-dir t "^[^.]"))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((data (car
                     (read-from-string
                      (buffer-substring-no-properties (point-min) (point-max))))))
          (if cheatsheet-lookup-cache
              (setcdr
               (nthcdr (1- (length cheatsheet-lookup-cache)) cheatsheet-lookup-cache)
               data)
            (setq cheatsheet-lookup-cache data))))))
  cheatsheet-lookup-cache)

;; cheat-sheets.org
(defun cheatsheet-lookup-cheatsheets-org (subdat &optional arg)
  "Dispatch to lookup at cheat-sheets.org. SUBDAT denotes section.
With ARG, goto section."
  (if (or arg current-prefix-arg)
      ;; Just go to section corresponding to chosen language
      (browse-url (concat "http://cheat-sheets.org/#" (cdr (assoc "id" subdat))))
    (let* ((sheets (cdr (or (assoc "sheets" subdat) subdat)))
           (sheet (funcall cheatsheet-lookup-completing-read
                            "Cheatsheet: " sheets))
           (uris (cdr (assoc "hrefs" (cdr (assoc sheet sheets))))))
      (if (= 1 (length uris))
          (browse-url (aref uris 0))
        (let ((uri (funcall cheatsheet-lookup-completing-read
                            "Location: " (append uris nil))))
          (browse-url uri))))))

;;;###autoload
(defun cheatsheet-lookup (&optional arg)
  "Lookup a cheatsheet.
With ARG or prefix, jump to the location of a selected
language/resource at http://cheat-sheets.org instead."
  (interactive "P")
  (let* ((data (cheatsheet-lookup-load))
         (subject
          (funcall cheatsheet-lookup-completing-read "Cheatsheet subject: " data))
         (subdat (assoc subject data)))
    (if (assoc "id" subdat)
        (cheatsheet-lookup-cheatsheets-org subdat arg)
      (if (= 1 (length (cdr subdat)))
          (browse-url (cl-cdadr subdat))
        (browse-url
         (let ((key (funcall cheatsheet-lookup-completing-read
                             "Cheatsheet: " (cdr subdat))))
           (cdr (assoc key (cdr subdat)))))))))

(provide 'cheatsheet-lookup)

;;; cheatsheet-lookup.el ends here
