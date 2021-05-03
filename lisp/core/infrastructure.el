;;; infrastructure.el --- infrastructure             -*- lexical-binding: t; -*-

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
;;;; mode line
(defface mode-line-buffer-name '((t
                                  (:inherit bold
                                            :background nil)))
  "")
(defface mode-line-buffer-name-modified '((t
                                           (:inherit (error
                                                      bold)
                                                     :background nil)))
  "")
(defface mode-line-buffer-project '((t
                                     (:inherit (font-lock-keyword-face bold)
                                               :background nil)))
  "")
(defface mode-line-buffer-encoding '((t
                                      (:inherit default
                                                :background nil)))
  "")
(defface mode-line-buffer-major-mode
  '((t
     (:inherit (font-lock-keyword-face bold)
               :background nil)))
  "")
(defun mode-line-buffer-name ()
  (propertize " %b " 'face (cond ((and
                                   buffer-file-name
                                   (buffer-modified-p)) 'mode-line-buffer-name-modified)
                                 (t 'mode-line-buffer-name))))
(defun mode-line-buffer-major-mode ()
  (propertize " %m " 'face 'mode-line-buffer-major-mode))
(defun mode-line-buffer-project ()
  (when-let ((project-root (or (when (fboundp 'projectile-project-root)
                                 (projectile-project-root))
                               (when (fboundp 'project-current)
                                 (when-let ((project (project-current)))
                                   (car (project-roots project)))))))
    (propertize (format " %s "  (file-name-nondirectory (directory-file-name project-root))) 'face
                'mode-line-buffer-project)))
(defun mode-line-buffer-encoding ()
  "Displays the eol and the encoding style of the buffer the same way Atom does."
  (concat " "
          ;; eol type
          (let ((eol (coding-system-eol-type buffer-file-coding-system)))
            (propertize (pcase eol (0 "LF ")
                               (1 "CRLF ")
                               (2 "CR ")
                               (_ "")) 'face 'mode-line-buffer-encoding ))
          ;; coding system
          (propertize (let ((sys (coding-system-plist buffer-file-coding-system)))
                        (cond ((memq (plist-get sys
                                                :category)
                                     '(coding-category-undecided coding-category-utf-8)) "UTF-8")
                              (t (upcase (symbol-name (plist-get sys
                                                                 :name)))))) 'face
                                                                 'mode-line-buffer-encoding ) " "))
(defun mode-line-buffer-name-with-project()
  (let ((project-root (or (when (fboundp 'projectile-project-root)
                            (projectile-project-root))
                          (when (fboundp 'project-current)
                            (when-let ((project (project-current)))
                              (car (project-roots project))))))
        (buffer-name (buffer-name)))
    (if project-root (concat  " "(propertize (format "%s"  (file-name-nondirectory
                                                            (directory-file-name project-root)))
                                             'face 'mode-line-buffer-project) ":" (propertize
                                                                                   buffer-name 'face
                                                                                   (cond ((and
                                                                                           buffer-file-name
                                                                                           (buffer-modified-p))
                                                                                          'mode-line-buffer-name-modified)
                                                                                         (t
                                                                                          'mode-line-buffer-name)))
                                             " ")
      (format " %s " (propertize buffer-name 'face (cond ((and
                                                           buffer-file-name
                                                           (buffer-modified-p)) 'mode-line-buffer-name-modified)
                                                         (t 'mode-line-buffer-name)))))))

(provide 'core/infrastructure)
;;; infrastructure.el ends here
