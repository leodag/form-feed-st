;;; form-feed.el --- Display ^L glyphs as horizontal lines -*- lexical-binding: t -*-

;; Copyright (C) 2014-2016 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; URL: https://github.com/wasamasa/form-feed
;; Keywords: faces
;; Version: 0.2.2

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This minor mode displays page delimiters which usually appear as ^L
;; glyphs on a single line as horizontal lines spanning the entire
;; window.  It is suitable for inclusion into mode hooks and is
;; intended to be used that way.  The following snippet would enable
;; it for Emacs Lisp files for instance:
;;
;;     (add-hook 'emacs-lisp-mode-hook 'form-feed-mode)

;; See the README for more info:
;; https://github.com/wasamasa/form-feed

;;; Code:

(require 'cl-lib)


;;; Variables

(defgroup form-feed nil
  "Turn ^L glyphs into horizontal lines."
  :prefix "form-feed-"
  :group 'faces)

(defface form-feed-line
  '((((type graphic)) :strike-through t :extend t)
    (((type tty)) :inherit font-lock-comment-face :underline t :extend t))
  "Face for form-feed-mode lines."
  :group 'form-feed)

(defcustom form-feed-extra-properties nil
  "List of additional text properties to add to form feeds."
  :type '(plist)
  :group 'form-feed)

(defvar form-feed--font-lock-face
  `(face form-feed-line ,@form-feed-extra-properties)
  "Facespec used by form-feed.")

(defvar-local form-feed--font-lock-keywords nil
  "Font-lock keywords added by form-feed. This variable is
set buffer-locally when the mode is enabled so they can be
disabled correctly.")

(defcustom form-feed-lighter " ^L"
  "Lighter for `form-feed-mode'."
  :type 'string
  :group 'form-feed
  :risky t)


;;; Functions

(defun form-feed--add-font-lock-keywords ()
  "Add buffer-local keywords to display page delimiter lines.
Make sure the special properties involved get cleaned up on
removal of the keywords via
`form-feed-remove-font-lock-keywords'."
  (make-local-variable 'font-lock-extra-managed-props)
  (setq form-feed--font-lock-keywords
        `((,(concat page-delimiter ".*\n?") 0 form-feed--font-lock-face t)))
  (dolist (prop `(display ,@form-feed-extra-properties))
    (cl-pushnew prop font-lock-extra-managed-props))
  (font-lock-add-keywords nil form-feed--font-lock-keywords t))

(defun form-feed--remove-font-lock-keywords ()
  "Remove buffer-local keywords displaying page delimiter lines."
  (font-lock-remove-keywords nil form-feed--font-lock-keywords))

(defun form-feed--on ()
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (form-feed--add-font-lock-keywords)
  (aset buffer-display-table ?\^L [\s \s])
  (font-lock-flush))

(defun form-feed--off ()
  (form-feed--remove-font-lock-keywords)
  (aset buffer-display-table ?\^L nil)
  (font-lock-flush))


;;; Minor mode

;;;###autoload
(define-minor-mode form-feed-mode
  "Toggle form-feed-mode.

This minor mode displays page delimiters which usually appear as ^L
glyphs on a single line as horizontal lines spanning the entire
window."
  :lighter form-feed-lighter
  :group 'form-feed
  :require 'form-feed
  (if form-feed-mode
      (form-feed--on)
    (form-feed--off)))


;;; Global mode

(defcustom form-feed-global-modes t
  "Modes for which `form-feed-mode' mode is turned on by `global-form-feed-mode'.
If nil, means no modes.  If t, then all major modes have it turned on.
If a list, it should be a list of `major-mode' symbol names for which
`form-feed-mode' should be automatically turned on.  The sense of the list is
negated if it begins with `not'.  For example:
 (c-mode c++-mode)
means that `form-feed-mode' is turned on for buffers in C and C++ modes only.
 (not message-mode)
means that `form-feed-mode' is always turned on except in `message-mode' buffers."
  :type '(choice (const :tag "none" nil)
                 (const :tag "all" t)
                 (set :menu-tag "mode-specific" :tag "modes"
                      :value (not)
                      (const :tag "Except" not)
                      (repeat :inline t (symbol :tag "mode")))))

(defun form-feed-mode-maybe ()
  (when (and (not (or noninteractive (eq (aref (buffer-name) 0) ?\s)))
             (cond ((eq form-feed-global-modes t)
                    t)
                   ((eq (car-safe form-feed-global-modes) 'not)
                    (not (memq major-mode (cdr form-feed-global-modes))))
                   (t (memq major-mode form-feed-global-modes))))
  (form-feed-mode 1)))

;;;###autoload
(define-global-minor-mode global-form-feed-mode
  form-feed-mode form-feed-mode-maybe)

(provide 'form-feed)

;;; form-feed.el ends here
