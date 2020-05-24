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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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


;;; variables

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

(defvar form-feed--font-lock-keywords nil
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
  (make-local-variable 'form-feed--font-lock-keywords)
  (make-local-variable 'font-lock-extra-managed-props)
  (setq form-feed--font-lock-keywords
        `((,(concat page-delimiter ".*\n?") 0 form-feed--font-lock-face t)))
  (dolist (prop `(display ,@form-feed-extra-properties))
    (cl-pushnew prop font-lock-extra-managed-props))
  (font-lock-add-keywords nil form-feed--font-lock-keywords))

(defun form-feed--remove-font-lock-keywords ()
  "Remove buffer-local keywords displaying page delimiter lines."
  (font-lock-remove-keywords nil form-feed--font-lock-keywords))

(defun form-feed--on ()
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (form-feed--add-font-lock-keywords)
  (aset buffer-display-table ?\^L [\s \s]))

(defun form-feed--off ()
  (form-feed--remove-font-lock-keywords)
  (aset buffer-display-table ?\^L nil))

;;;###autoload
(define-minor-mode form-feed-mode
  "Toggle form-feed-mode.

This minor mode displays page delimiters which usually appear as ^L
glyphs on a single line as horizontal lines spanning the entire
window."
  :lighter form-feed-lighter
  (if form-feed-mode
      (form-feed--on)
    (form-feed--off))

  (when (called-interactively-p 'interactive)
    (font-lock-flush)))

(provide 'form-feed)
;;; form-feed.el ends here
