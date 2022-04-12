;;; org-epubinfo.el --- Integrate Org with epubinfo program -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (org "9.3"))
;; Keywords: outlines wp media
;; URL: https://github.com/akirak/org-epubinfo

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides some functions for using the features of
;; epubinfo program from inside Org mode.

;; epubinfo is available from here: <https://github.com/akirak/epubinfo>.

;;; Code:

(require 'org)

(defgroup org-epubinfo nil
  "Integrate Org with epubinfo."
  :group 'org)

(defcustom org-epubinfo-executable "epubinfo"
  "Executable name of epubinfo."
  :type 'file)

;;;; Table of contents

(defcustom org-epubinfo-toc-dblock-defaults
  '(:depth 1)
  "Default parameters for the \"epub-toc\" dynamic block."
  :type 'plist)

(defcustom org-epubinfo-context 'self
  "Entry in which a file link should be located."
  :type '(choice (const :tag "Entry of the dblock" self)
                 (const :tag "Parent entry" parent)))

;;;###autoload
(defun org-dblock-write:epub-toc (params)
  "Insert the content of an \"epub-toc\" dynamic block.

PARAMS is a plist, as in other dynamic block definitions.

See also `org-epubinfo-toc-dblock-defaults'."
  (let* ((params (org-combine-plists org-epubinfo-toc-dblock-defaults params))
         (checkbox (plist-get params :checkbox))
         (depth (plist-get params :depth))
         (file (or (plist-get params :file)
                   (org-epubinfo--file-from-context)
                   (user-error ":file is not specified and no file link is in the entry"))))
    (apply #'call-process
           "epubinfo" nil t nil
           "toc" "--org"
           (append (when checkbox
                     '("--checkbox"))
                   (when depth
                     (list "--depth" (number-to-string depth)))
                   (list (expand-file-name file))))))

(defun org-epubinfo--file-from-context ()
  "Return an epub file discovered from the context."
  (catch 'epub-file
    (save-excursion
      (org-back-to-heading)
      (when (eq org-epubinfo-context 'parent)
        (org-up-element))
      (save-match-data
        (while (re-search-forward org-link-any-re (org-entry-end-position) t)
          (let ((dest (match-string 2)))
            (when (and (string-match (rx bol "file:" (group (+ anything))) dest)
                       (string-suffix-p ".epub" (match-string 1 dest)))
              (throw 'epub-file
                     (substring-no-properties (match-string 1 dest))))))))))

(provide 'org-epubinfo)
;;; org-epubinfo.el ends here
