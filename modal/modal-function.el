;;; modal-function.el --- function             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  meetcw

;; Author: meetcw <meetcw@outlook.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
;;;; thing
(defun bound-of--other-side-of-sexp (pos &optional backward)
  "Return the other side of sexp from POS.

If BACKWARD is non-nil, search backward."
  (when pos (save-mark-and-excursion (goto-char pos)
                                     (ignore-errors (if backward (backward-sexp 1)
                                                      (forward-sexp 1))
                                                    (point)))))

(defun bound-of--regexp (beg-re)
  "Get the bounds of regexp, located by beg-re."
  (save-mark-and-excursion (let ((orig (point)) beg end)
                             (while (and
                                     (setq beg (re-search-backward beg-re nil t))
                                     (setq end (bound-of--other-side-of-sexp (point)))
                                     (<= end orig)))
                             (when (and end
                                        (> end orig))
                               (cons beg end)))))

(defun bound-of-parentheses-at-point ()
  (bound-of--regexp "("))
(put 'parentheses 'bounds-of-thing-at-point 'bound-of-parentheses-at-point)

(defun bound-of-square-brackets-at-point ()
  (bound-of--regexp "\\["))
(put 'square-brackets 'bounds-of-thing-at-point 'bound-of-square-brackets-at-point)

(defun bound-of-curly-brackets-at-point ()
  (bound-of--regexp "{"))
(put 'curly-brackets 'bounds-of-thing-at-point 'bound-of-curly-brackets-at-point)

(defun bound-of-string-at-point ()
  (when (nth 3 (syntax-ppss))
    (let (beg end)
      (save-mark-and-excursion (while (nth 3 (syntax-ppss))
                                 (backward-char 1))
                               (setq beg (point)))
      (save-mark-and-excursion (while (nth 3 (syntax-ppss))
                                 (forward-char 1))
                               (setq end (point)))
      (cons beg end))))
(put 'string 'bounds-of-thing-at-point 'bound-of-string-at-point)
(provide 'modal-function)
;;; modal-function.el ends here
