;;; codesearch.el --- Core support for managing codesearch tools
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 1
;; URL: https://github.com/abingham/emacs-codesearch
;; Keywords: tools, development, search
;; Package-Requires: ((dash "2.8.0") (deferred "0.4.0"))
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
;; https://github.com/google/codesearch
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
(require 'deferred)

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

(defcustom codesearch-output-buffer "*codesearch*"
  "Buffer where miscellaneous tool output gets written."
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

(defun codesearch--handle-output (output)
  "Append process output to standard buffer."
  (with-current-buffer (get-buffer-create codesearch-output-buffer)
    (goto-char (point-max))
    (insert "-----\n")
    (insert output)))

(defun codesearch--run-tool (tool &optional callback dir &rest args)
  "Run the `tool' command passing `args' arguments.

`callback' is a 1-arity function that will be give the full
output of the command when it completes. `dir' is the directory
from which any index-file searches will start. `args' is a list
of any arguments to be passed to the tool. Note that and
`-indexpath <index file>' argument will *always* be supplied to
the tool command, and it will come before `args' in the
invocation."
  (let* ((callback (or callback 'codesearch--handle-output))
         (search-dir (or dir default-directory))
         (index-file (codesearch--csearchindex search-dir))
         (indexpath-args (list "-indexpath" index-file))
         (full-args (append indexpath-args args)))
    (deferred:$
      (apply 'deferred:process tool full-args)
      (deferred:nextc it
        callback))))

(defun codesearch-run-cindex (&optional callback dir &rest args)
  "Run the cindex command passing `args' arguments."
  (apply 'codesearch--run-tool codesearch-cindex callback dir args))

(defun codesearch-run-csearch (&optional callback dir &rest args)
  "Run the csearch command passing `args' arguments."
  (apply 'codesearch--run-tool codesearch-csearch callback dir args))

;;;###autoload
(defun codesearch-build-index (dir)
  "Add the contents of DIR to the index."
  (interactive
   (list
    (read-directory-name "Directory: ")))
  (codesearch-run-cindex
   nil
   nil
   dir))

;;;###autoload
(defun codesearch-update-index ()
  "Rescan all of the directories currently in the index, updating
the index with the new contents."
  (interactive)
  (codesearch-run-cindex
   'codesearch--handle-output))

;;;###autoload
(defun codesearch-reset ()
  "Reset (delete) the codesearch index."
  (interactive)
  (codesearch-run-cindex nil nil "-reset"))

(provide 'codesearch)

;;; codesearch.el ends here
