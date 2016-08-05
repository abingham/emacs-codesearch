;;; codesearch.el --- Core support for managing codesearch tools
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 1
;; URL: https://github.com/abingham/emacs-codesearch
;; Keywords: tools, development, search
;; Package-Requires: ((dash "2.8.0"))
;;
;; This file is not part of GNU Emacs.
;;
;; Copyright (c) 2016 Austin Bingham
;;
;;; Commentary:
;;
;; Description:
;;
;; This extension provides basic support for executing the codesearch tools and
;; for managing codesearch indices.
;;
;; For more details, see the project page at
;; https://github.com/abingham/emacs-codesearch
;;
;; For more details on codesearch, see its project page at
;; https://github.com/google/codesearc
;;
;; Installation:
;;
;; The simple way is to use package.el:
;;
;;   M-x package-install codesearch
;;
;; Or, copy codesearch.el to some location in your emacs load
;; path. Then add "(require 'codesearch)" to your emacs initialization
;; (.emacs, init.el, or something).
;;
;; Example config:
;;
;;   (require 'codesearch)
;;
;; This elisp extension assumes you've got the codesearch tools -
;; csearch and cindex - installed. See the codesearch-csearch and
;; codesearch-cindex variables for more information.
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

(eval-when-compile
  (require 'cl))
(require 'dash)

(defgroup codesearch nil
  "Variables related to codesearch."
  :prefix "codesearch-"
  :group 'tools)

(defcustom codesearch-csearch "csearch"
  "The csearch executable to use."
  :type '(string)
  :group 'codesearch)

(defcustom codesearch-cindex "cindex"
  "The cindex executable to use."
  :type '(string)
  :group 'codesearch)

(defcustom codesearch-global-csearchindex nil
  "The global index file. If defined, this will be used for all codesearch operations."
  :type '(string)
  :group 'codesearch)

(defcustom codesearch-csearchindex ".csearchindex"
  "The name of the index file which will be searched for if no global index is defined."
  :type '(string)
  :group 'codesearch)

(defun codesearch--find-dominant-csearchindex (dir)
  "Search `dir' and its ancestors for the index, returning the path if found."
  (let* ((start-dir (expand-file-name dir))
         (index-dir (locate-dominating-file start-dir codesearch-csearchindex)))
    (if index-dir
        (concat index-dir codesearch-csearchindex)
      (error "Can't find csearchindex"))))

(defun codesearch--csearchindex (dir)
  "Get the full path to the index to use for searches starting in `dir'."
  (expand-file-name (or codesearch-global-csearchindex
                        (codesearch--find-dominant-csearchindex dir))))

(defun codesearch-run-cindex (&optional callback dir buffer &rest args)
  "Run the cindex command passing `args' arguments."
  (let* ((search-dir (or dir default-directory))
         (index-file (codesearch--csearchindex search-dir))
         (indexpath-args (list "-indexpath" index-file))
         (full-args (append indexpath-args args)))
    (set-process-sentinel
     (apply 'start-file-process
            "cindex"
            buffer
            codesearch-cindex
            full-args)
     callback)))

(defun codesearch--handle-listing (process event)
  (when (equal event "finished\n")
    (with-current-buffer (get-buffer-create "*codesearch-directories*")
      (let ((dirs (-slice (split-string (buffer-string) "\n") 0 -1)))
        (erase-buffer)
        (insert "[codesearch: currently indexed directories]\n\n")
        (mapcar
         (lambda (dir) (insert (format "%s\n" dir)))
         dirs)))))

;;;###autoload
(defun codesearch-list-directories ()
  "List the directories currently being indexed."
  (interactive)
  (with-temp-buffer
    (codesearch-run-cindex
     'codesearch--handle-listing
     nil
     (current-buffer)
     "-list")))

;;;###autoload
(defun codesearch-build-index (dir)
  "Add the contents of DIR to the index."
  (interactive
   (list
    (read-directory-name "Directory: ")))
  (codesearch--run-cindex dir))

;;;###autoload
(defun codesearch-update-index ()
  "Rescan all of the directories currently in the index, updating
the index with the new contents."
  (interactive)
  (codesearch--run-cindex))

;;;###autoload
(defun codesearch-reset ()
  "Reset (delete) the codesearch index."
  (interactive)
  (codesearch--run-cindex "-reset"))

(provide 'codesearch)

;;; codesearch.el ends here
