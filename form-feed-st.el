;;; form-feed-st.el --- Display ^L glyphs as full-width horizontal lines -*- lexical-binding: t -*-

;; Author: Leonardo Schripsema
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/leodag/form-feed-st
;; Keywords: faces

;; Copyright (C) 2020-2023 Leonardo Schripsema

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
;;     (add-hook 'emacs-lisp-mode-hook 'form-feed-st-mode)

;; Forked from Vasilij Schneidermann's form-feed:
;; https://depp.brause.cc/form-feed/
;; https://github.com/wasamasa/form-feed (archived)

;; In comparison to it, this fork does not have configurable line
;; width; however it always displays correctly, even if you have
;; multiple windows showing the same buffer, and will never cause side
;; scrolling since the form feed only occupies two spaces.  It will
;; also only affect form feeds in the beginning of the line, which to
;; me is a feature since it does not hide any undesired characters
;; after the form feed, making them easy to notice and remove.

;;; Code:

(require 'cl-lib)


;;; Variables

(defgroup form-feed-st nil
  "Turn ^L glyphs into horizontal lines."
  :prefix "form-feed-st-"
  :group 'faces)

(defface form-feed-st-line
  '((((type graphic)) :strike-through t :extend t)
    (((type tty)) :inherit font-lock-comment-face :underline t :extend t))
  "Face for `form-feed-st-mode' lines."
  :group 'form-feed-st)

(defcustom form-feed-st-extra-properties nil
  "List of additional text properties to add to form feeds."
  :type '(plist)
  :group 'form-feed-st)

(defvar-local form-feed-st--font-lock-keywords nil
  "Font-lock keywords to be removed when the mode is disabled.")

(defcustom form-feed-st-lighter " ^L"
  "Lighter for `form-feed-st-mode'."
  :type 'string
  :group 'form-feed-st)


;;; Functions

(defun form-feed-st--add-font-lock-keywords ()
  "Add buffer-local keywords to display page delimiter lines.
Make sure the special properties involved get cleaned up on
removal of the keywords via `form-feed-st-remove-font-lock-keywords'."
  (make-local-variable 'font-lock-extra-managed-props)
  (setq form-feed-st--font-lock-keywords
        `((,(concat page-delimiter ".*\n?") 0 '(face form-feed-st-line ,@form-feed-st-extra-properties) t)
          (,page-delimiter 0 '(face nil display (space :width 2)))))
  (dolist (prop `(display ,@form-feed-st-extra-properties))
    (cl-pushnew prop font-lock-extra-managed-props))
  (font-lock-add-keywords nil form-feed-st--font-lock-keywords t))

(defun form-feed-st--remove-font-lock-keywords ()
  "Remove buffer-local keywords displaying page delimiter lines."
  (font-lock-remove-keywords nil form-feed-st--font-lock-keywords))

(defun form-feed-st--on ()
  "Turn on the mode and manage font-lock keywords."
  (form-feed-st--add-font-lock-keywords)
  (font-lock-flush))

(defun form-feed-st--off ()
  "Turn off the mode and manage font-lock keywords."
  (form-feed-st--remove-font-lock-keywords)
  (font-lock-flush))


;;; Minor mode

;;;###autoload
(define-minor-mode form-feed-st-mode
  "Toggle `form-feed-st-mode'.

This minor mode displays page delimiters which usually appear as ^L
glyphs on a single line as horizontal lines spanning the entire
window."
  :lighter form-feed-st-lighter
  :group 'form-feed-st
  :require 'form-feed-st
  (if form-feed-st-mode
      (form-feed-st--on)
    (form-feed-st--off)))


;;; Global mode

(defcustom form-feed-st-include-modes '(prog-mode text-mode)
  "Major modes in which `form-feed-st-mode' is activated.
This is used by `global-form-feed-st-mode' which activates `form-feed-st-mode'
in all buffers whose major mode derives from one of the modes
listed here, but not from one of the modes listed in
`form-feed-st-exclude-modes'."
  :type '(repeat function)
  :group 'form-feed-st)

(defcustom form-feed-st-exclude-modes nil
  "Major modes in which `form-feed-st-mode' is not activated.
This is used by `global-form-feed-st-mode' which activates `form-feed-st-mode'
in all buffers whose major mode derives from one of the modes
listed in `form-feed-st-include-modes', but not from one of the modes
listed here."
  :type '(repeat function)
  :group 'form-feed-st)

(defun form-feed-st-mode-maybe ()
  "Turn on function for the mode."
  (when (and (apply #'derived-mode-p form-feed-st-include-modes)
             (not (apply #'derived-mode-p form-feed-st-exclude-modes))
             (not (bound-and-true-p enriched-mode)))
    (form-feed-st-mode)))

;;;###autoload
(define-global-minor-mode global-form-feed-st-mode
  form-feed-st-mode form-feed-st-mode-maybe)

(provide 'form-feed-st)

;;; form-feed-st.el ends here
