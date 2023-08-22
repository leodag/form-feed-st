;;; ff-st.el --- Display ^L glyphs as horizontal lines -*- lexical-binding: t -*-

;; Author: Leonardo Schripsema
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/leodag/ff-st
;; Keywords: faces

;; Copyright (C) 2020-2021 Leonardo Schripsema

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
;;     (add-hook 'emacs-lisp-mode-hook 'ff-st-mode)

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

(defgroup ff-st nil
  "Turn ^L glyphs into horizontal lines."
  :prefix "ff-st-"
  :group 'faces)

(defface ff-st-line
  '((((type graphic)) :strike-through t :extend t)
    (((type tty)) :inherit font-lock-comment-face :underline t :extend t))
  "Face for ff-st-mode lines."
  :group 'ff-st)

(defcustom ff-st-extra-properties nil
  "List of additional text properties to add to form feeds."
  :type '(plist)
  :group 'ff-st)

(defvar-local ff-st--font-lock-keywords nil
  "Font-lock keywords added by ff-st to be removed when
the mode is disabled.")

(defcustom ff-st-lighter " ^L"
  "Lighter for `ff-st-mode'."
  :type 'string
  :group 'ff-st)


;;; Functions

(defun ff-st--add-font-lock-keywords ()
  "Add buffer-local keywords to display page delimiter lines.
Make sure the special properties involved get cleaned up on
removal of the keywords via `ff-st-remove-font-lock-keywords'."
  (make-local-variable 'font-lock-extra-managed-props)
  (setq ff-st--font-lock-keywords
        `((,(concat page-delimiter ".*\n?") 0 '(face ff-st-line ,@ff-st-extra-properties) t)
          (,page-delimiter 0 '(face nil display (space :width 2)))))
  (dolist (prop `(display ,@ff-st-extra-properties))
    (cl-pushnew prop font-lock-extra-managed-props))
  (font-lock-add-keywords nil ff-st--font-lock-keywords t))

(defun ff-st--remove-font-lock-keywords ()
  "Remove buffer-local keywords displaying page delimiter lines."
  (font-lock-remove-keywords nil ff-st--font-lock-keywords))

(defun ff-st--on ()
  "Turn on the mode and manage font-lock keywords."
  (ff-st--add-font-lock-keywords)
  (font-lock-flush))

(defun ff-st--off ()
  "Turn off the mode and manage font-lock keywords."
  (ff-st--remove-font-lock-keywords)
  (font-lock-flush))


;;; Minor mode

;;;###autoload
(define-minor-mode ff-st-mode
  "Toggle ff-st-mode.

This minor mode displays page delimiters which usually appear as ^L
glyphs on a single line as horizontal lines spanning the entire
window."
  :lighter ff-st-lighter
  :group 'ff-st
  :require 'ff-st
  (if ff-st-mode
      (ff-st--on)
    (ff-st--off)))


;;; Global mode

(defcustom ff-st-include-modes '(prog-mode text-mode)
  "Major modes in which `ff-st-mode' is activated.
This is used by `global-ff-st-mode' which activates `ff-st-mode'
in all buffers whose major mode derives from one of the modes
listed here, but not from one of the modes listed in
`ff-st-exclude-modes'."
  :type '(repeat function)
  :group 'ff-st)

(defcustom ff-st-exclude-modes nil
  "Major modes in which `ff-st-mode' is not activated.
This is used by `global-ff-st-mode' which activates `ff-st-mode'
in all buffers whose major mode derives from one of the modes
listed in `ff-st-include-modes', but not from one of the modes
listed here."
  :type '(repeat function)
  :group 'ff-st)

(defun ff-st-mode-maybe ()
  "Turn on function for the mode."
  (when (and (apply #'derived-mode-p ff-st-include-modes)
             (not (apply #'derived-mode-p ff-st-exclude-modes))
             (not (bound-and-true-p enriched-mode)))
    (ff-st-mode)))

;;;###autoload
(define-global-minor-mode global-ff-st-mode
  ff-st-mode ff-st-mode-maybe)

(provide 'ff-st)

;;; ff-st.el ends here
