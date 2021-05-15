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

(defcustom org-epubinfo-toc-dblock-params
  '(:context self)
  "The default parameters for \"epub-toc\" dynamic blocks.

The following options are supported:

:context
  Denote the Org entry from which a file link.
  The following symbols are accepted:

  * `self': The entry the dblock resides in.
  * `parent': The parent entry of `self'.

:depth
  Maximum depth of rendered items.

:checkbox
  When non-nil, a checkbox is added to each item."
  :type 'plist)

;;;###autoload
(defun org-dblock-write:epub-toc (params)
  "Insert the toc of an EPUB file into the current block.

PARAMS is a plist of parameters to configure the output."
  (let* ((params (org-combine-plists org-epubinfo-toc-dblock-params
                                     params))
         (file (or (plist-get params :file)
                   (org-epubinfo--find-file-link
                    (plist-get params :context)))))
    (insert (apply #'org-epubinfo--toc-string file
                   (list :depth (plist-get params :depth)
                         :checkbox (plist-get params :checkbox))))))

;;;###autoload
(defun org-epubinfo-insert-toc (file)
  "Insert the table of contents of FILE."
  (interactive "fEPUB file: ")
  (insert (org-epubinfo--toc-string file)))

(cl-defun org-epubinfo--toc-string (file &key depth checkbox)
  "Insert the toc of an EPUB file.

FILE is the name of an EPUB file.

When DEPTH is given as a number, limit the depth to it.

When CHECKBOX is non-nil, add a checkbox to each item."
  (with-temp-buffer
    (unless (zerop (apply #'process-file
                          org-epubinfo-executable
                          nil (list (current-buffer) nil) nil
                          "toc" "--org"
                          `(,@(when depth
                                (list (format "--depth=%d" depth)))
                            ,@(when checkbox
                                '("--checkbox"))
                            ,file)))
      (error "Error from epubinfo on %s" file))
    (buffer-string)))

;;;; Helpers

(defun org-epubinfo--find-file-link (context)
  "Return the first file link in CONTEXT."
  (org-with-wide-buffer
   (cl-ecase context
     (self (org-back-to-heading))
     (parent (org-up-heading-all 1)))
   (let ((limit (org-entry-end-position))
         (headline (nth 4 (org-heading-components))))
     (catch 'file
       (while (re-search-forward org-link-bracket-re limit)
         (let ((uri (plist-get (get-char-property (1- (point)) 'htmlize-link)
                               :uri)))
           (when (string-prefix-p "file:" uri)
             (throw 'file (expand-file-name
                           (string-remove-prefix "file:" uri))))))
       (error "No file link was found in \"%s\"" headline)))))

(provide 'org-epubinfo)
;;; org-epubinfo.el ends here
