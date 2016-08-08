;;; codesearch.el --- Core support for managing codesearch tools
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 1
;; URL: https://github.com/abingham/emacs-codesearch
;; Keywords: tools, development, search
;; Package-Requires: ((dash "2.8.0") (s "1.10.0"))
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
(require 's)

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
        (concat index-dir codesearch-csearchindex))))

(defun codesearch--csearchindex (dir)
  "Get the full path to the index to use for searches starting in `dir'."
  (expand-file-name (or codesearch-global-csearchindex
                        (codesearch--find-dominant-csearchindex dir)
                        (error "Can't find csearchindex"))))

(defun codesearch--handle-output (process output)
  "Append process output to standard buffer."
  (with-current-buffer (get-buffer-create codesearch-output-buffer)
    (goto-char (point-max))
    (insert "\n")
    (insert output)))

(defun codesearch--run-tool (dir command args)
  "Run `command' with CSEARCHINDEX variable set correctly.

`dir' is the directory from which any index-file searches will
start. Returns the process object."
  ;;  (message command)
  (let* ((search-dir (or dir default-directory))
         (index-file (or (and dir
                              (string= command codesearch-cindex)
                              (concat dir codesearch-csearchindex))
                         (codesearch--csearchindex search-dir)))
         (process-environment (copy-alist process-environment)))
    (setenv "CSEARCHINDEX" (expand-file-name index-file))
    (apply
     'start-process
     "codesearch"
     nil
     command
     args)))

(defun codesearch-run-cindex (&optional dir &rest args)
  "Run the cindex command passing `args' arguments."
  (codesearch--run-tool
   dir codesearch-cindex args))

(defun codesearch-run-csearch (&optional dir args)
  "Run the csearch command passing `args' arguments."
  (codesearch--run-tool
   dir codesearch-csearch args))

;;;###autoload
(defun codesearch-build-index (dir)
  "Add the contents of DIR to the index."
  (interactive
   (list
    (read-directory-name "Directory: ")))
  (set-process-filter
   (codesearch-run-cindex dir dir)
   'codesearch--handle-output))

;;;###autoload
(defun codesearch-update-index ()
  "Rescan all of the directories currently in the index, updating
the index with the new contents."
  (interactive)
  (set-process-filter
   (codesearch-run-cindex)
   'codesearch--handle-output))

;;;###autoload
(defun codesearch-reset ()
  "Reset (delete) the codesearch index."
  (interactive)
  (set-process-filter
   (codesearch-run-cindex nil "-reset")
   'codesearch--handle-output))

(provide 'codesearch)

;;; codesearch.el ends here
