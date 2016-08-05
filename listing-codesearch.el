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

(defgroup listing-codesearch nil
  "Variables related to listing-codesearch."
  :prefix "listing-codesearch-"
  :group 'tools)

(defface listing-codesearch-filename
  '((t :inherit font-lock-constant-face))
  "Face used to highlight filenames in matches."
  :group 'listing-codesearch)

(defface listing-codesearch-line-number
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight line numbers in matches."
  :group 'listing-codesearch)

(defconst listing-codesearch--match-regex "^\\(.*\\):\\([0-9]+\\):"
  "The regular expression used to find matches in the codesearch output.")

(define-button-type 'listing-codesearch--filename-match-button
  'face 'listing-codesearch-filename
  'follow-link 't
  'button 't)

(define-button-type 'listing-codesearch--line-number-match-button
  'face 'listing-codesearch-line-number
  'follow-link 't
  'button 't)

(defun listing-codesearch--make-filenames-clickable (buff)
  "Finds all codesearch matches in BUFF, turning them into
clickable buttons that link to the matched file/line-number.

BUFF is assumed to contain the output from running csearch.
"
  (with-current-buffer buff
    (beginning-of-buffer)
    (while (re-search-forward listing-codesearch--match-regex nil t)
      (lexical-let* ((filename (match-string 1))
                     (line-number (string-to-number (match-string 2)))
                     (visit-match (lambda (b)
                                    (find-file-other-window filename)
                                    (goto-line line-number))))
        (make-text-button
         (match-beginning 1)
         (match-end 1)
         'type 'listing-codesearch--filename-match-button
         'action visit-match)

        (make-text-button
         (match-beginning 2)
         (match-end 2)
         'type 'listing-codesearch--line-number-match-button
         'action visit-match)))))

(defvar listing-codesearch-pattern-history nil)

(defvar listing-codesearch-file-pattern-history nil)

;;;###autoload
(defun listing-codesearch-search (pattern file-pattern)
  "Search files matching FILE-PATTERN in the index for PATTERN."
  (interactive
   (list
    (read-string "Pattern: " (thing-at-point 'symbol)
                 'listing-codesearch-pattern-history (car listing-codesearch-pattern-history))
    (read-string "File pattern: " ".*"
                 'listing-codesearch-file-pattern-history (car listing-codesearch-file-pattern-history))))
  (let ((switch-to-visible-buffer t)
        (buff (get-buffer-create "*codesearch-results*"))
        (file-pattern (if (memq system-type '(windows-nt ms-dos))
                          (replace-regexp-in-string "/" "\\\\\\\\" file-pattern)
                        file-pattern)))
    (with-current-buffer buff
      (read-only-mode 0)
      (erase-buffer)
      (codesearch-run-csearch
       (lambda (result)
         (insert result)
         (listing-codesearch--make-filenames-clickable (current-buffer))
         (pop-to-buffer (current-buffer)))
       default-directory
       "-f" file-pattern
       "-n" pattern))))

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
