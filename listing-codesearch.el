;;; listing-codesearch.el --- Simple, list-based UI for codesearch
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 1
;; URL: https://github.com/abingham/emacs-codesearch
;; Keywords: tools, development, search
;; Package-Requires: ()
;;
;; This file is not part of GNU Emacs.
;;
;; Copyright (c) 2016 Austin Bingham
;;
;;; Commentary:
;;
;; Description:
;;
;; Provides a simple, listing-oriented UI for data from codesearch.
;;
;; For more details, see the project page at
;; https://github.com/abingham/emacs-codesearch
;;
;; For more details on codesearch, see its project page at
;; https://github.com/google/codesearch
;;
;; Installation:
;;
;; The simple way is to use package.el:
;;
;;   M-x package-install listing-codesearch
;;
;; Or, copy codesearch.el to some location in your emacs load
;; path. Then add "(require 'listing-codesearch)" to your emacs initialization
;; (.emacs, init.el, or something).
;;
;; Example config:
;;
;;   (require 'listing-codesearch)
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'codesearch)

(defun listing-codesearch--handle-listing (results)
  (with-current-buffer "*codesearch-directories*"
    (let ((dirs (-slice (split-string results "\n") 0 -1)))
      (erase-buffer)
      (insert "[codesearch: currently indexed directories]\n\n")
      (mapcar
       (lambda (dir) (insert (format "%s\n" dir)))
       dirs))))

;;;###autoload
(defun listing-codesearch-list-directories ()
  "List the directories currently being indexed."
  (interactive)
  (codesearch-run-cindex
   'codesearch--handle-listing
   nil
   "-list"))

(provide 'listing-codesearch)

;;; listing-codesearch.el ends here
