;;; toml-mode.el --- Major mode for editing TOML files -*- lexical-binding: t -*-

;; Copyright (C) 2013 Felix Chern

;; Author: Felix Chern <idryman@gmail.com>
;; Keywords: data toml
;; Version: 0.1.3
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; URL: https://github.com/dryman/toml-mode.el

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; This is a major mode for editing files in TOML data format

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'align)

(defvar toml-syntax-table nil "Syntax table for `toml-mode'.")
(setq toml-syntax-table
      (let ((syn-table (make-syntax-table)))
        (modify-syntax-entry ?# "< b" syn-table)
        (modify-syntax-entry ?\n "> b" syn-table)
        syn-table))

(defvar toml-keywords
  '(("\\[\\{1,2\\}[a-zA-Z][^ \n\t\r]+\\]\\{1,2\\}" . font-lock-keyword-face)
    ("[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9][Zz]"
     . font-lock-variable-name-face)
    ("\\b[-+]?\\(?:[0-9]*\\.[0-9]+|[0-9]+\\)\\b" . font-lock-variable-name-face))
  "Syntax highlight keywords for `toml-mode`.")

(defconst toml-mode-align-rules
  '((toml-equals
     (regexp . "\\(\\s-*\\)=\\(\\s-*\\)")
     (group  . (1 2))
     (modes  . '(toml-mode))
     (separate . entire)))
  "Align rules for Toml Mode.")

;;;###autoload
(define-derived-mode toml-mode prog-mode "toml"
  :syntax-table toml-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+[ \t]+")
  (setq-local indent-tabs-mode nil)
  (setq-local tab-stop-list (cl-loop for n from 4 below 120 by 4 collect n))
  (setq-local parse-sexp-ignore-comments t)
  (setq font-lock-defaults '(toml-keywords))
  (setq align-mode-rules-list toml-mode-align-rules))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))

(provide 'toml-mode)

;;; toml-mode.el ends here
