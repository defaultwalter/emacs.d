;;; modal-command.el --- modal command               -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Maf

;; Author: Maf <wmafire@gmail.com>
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
(defun modal--set-mark()
  (when (not (region-active-p))
    (push-mark (point) t t)))

(defun modal-switch-to-default-state()
  (interactive)
  (modal--switch-to-default-state))

(defun modal-insert()
  (interactive)
  (when (region-active-p)
    (goto-char (region-beginning))
    (deactivate-mark t))
  (modal--switch-state 'insert))

(defun modal-append()
  (interactive)
  (when (region-active-p)
    (goto-char (region-end))
    (deactivate-mark t))
  (modal--switch-state 'insert))

(defun modal-line-insert()
  (interactive)
  (when (region-active-p)
    (deactivate-mark t))
  (goto-char (line-beginning-position))
  (modal--switch-state 'insert))

(defun modal-line-append()
  (interactive)
  (when (region-active-p)
    (deactivate-mark t))
  (goto-char (line-end-position))
  (modal--switch-state 'insert))

(defun modal--temporary-insert-callback()
  (unless (eq this-command #'modal-temporary-insert)
    (remove-hook 'post-command-hook #'modal--temporary-insert-callback t)
    (modal-switch-to-default-state)))

(defun modal-temporary-insert()
  (interactive)
  (modal--switch-state 'insert)
  (add-hook 'post-command-hook #'modal--temporary-insert-callback 0 t))

;;;; motion
(defun modal-previous-line (arg)
  (interactive "p")
  (setq this-command #'previous-line)
  (previous-line arg))

(defun modal-next-line (arg)
  (interactive "p")
  (setq this-command #'next-line)
  (next-line arg))

(defun modal-forward-char (arg)
  (interactive "p")
  (let ((boundary-position (line-end-position)))
    (forward-char arg)
    (when (> (point) boundary-position)
      (goto-char boundary-position))))

(defun modal-backward-char (arg)
  (interactive "p")
  (let ((boundary-position (line-beginning-position)))
    (backward-char arg)
    (when (< (point) boundary-position)
      (goto-char boundary-position))))

(defun modal-word-end(arg)
  (interactive "p")
  (forward-thing 'word arg))

(defun modal-word-begin(arg)
  (interactive "p")
  (forward-thing 'word (- arg)))

;;;; select

(defun modal-select-word()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'word)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))

(defun modal-select-symbol()
  (interactive )
  (let ((position (bounds-of-thing-at-point 'symbol)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))

(defun modal-select-inner-line()
  (interactive )
  (goto-char (line-end-position))
  (push-mark (line-beginning-position) t t))
(defun modal-select-whole-line()
  (interactive )
  (let ((position (bounds-of-thing-at-point 'line)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))

(defun modal-select-forward-word(arg)
  (interactive "p")
  (forward-thing 'word arg)
  (let ((position (bounds-of-thing-at-point 'word)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))

(defun modal-select-backward-word(arg)
  (interactive "p")
  (forward-thing 'word (- arg))
  (let ((position (bounds-of-thing-at-point 'word)))
    (goto-char (car position))
    (push-mark (cdr position) t t)))

(defun modal-select-forward-symbol(arg)
  (interactive "p")
  (forward-thing 'word arg)
  (let ((position (bounds-of-thing-at-point 'symbol)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))
(defun modal-select-backward-symbol(arg)
  (interactive "p")
  (forward-thing 'word (- arg))
  (let ((position (bounds-of-thing-at-point 'symbol)))
    (goto-char (car position))
    (push-mark (cdr position) t t)))

(defun modal-select-inner-parentheses()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'parentheses)))
    (goto-char (1- (cdr position)))
    (push-mark (1+ (car position)) t t)))
(defun modal-select-whole-parentheses()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'parentheses)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))

(defun modal-select-inner-square-brackets()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'square-brackets)))
    (goto-char (1- (cdr position)))
    (push-mark (1+ (car position)) t t)))
(defun modal-select-whole-square-brackets()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'square-brackets)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))

(defun modal-select-inner-curly-brackets()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'curly-brackets)))
    (goto-char (1- (cdr position)))
    (push-mark (1+ (car position)) t t)))

(defun modal-select-whole-curly-brackets()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'curly-brackets)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))

(defun modal-select-inner-string()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'string)))
    (goto-char (1- (cdr position)))
    (push-mark (1+ (car position)) t t)))

(defun modal-select-whole-string()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'string)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))

;;;; select to
(defun modal-select-to-forward-char(arg)
  (interactive "p")
  (unless (region-active-p)
    (push-mark (point) t t))
  (modal-forward-char arg))

(defun modal-select-to-backward-char(arg)
  (interactive "p")
  (unless (region-active-p)
    (push-mark (point) t t))
  (modal-backward-char arg))

(defun modal-select-to-word-end(arg)
  (interactive "p")
  (unless (region-active-p)
    (push-mark (point) t t)  )
  (forward-thing 'word arg))

(defun modal-select-to-word-begin(arg)
  (interactive "p")
  (unless (region-active-p)
    (push-mark (point) t t)  )
  (forward-thing 'word (- arg)))

(defun modal-select-to-next-line(arg)
  (interactive "p")
  (unless (region-active-p)
    (push-mark (point) t t))
  (modal-next-line arg))

(defun modal-select-to-previous-line(arg)
  (interactive "p")
  (unless (region-active-p)
    (push-mark (point) t t))
  (modal-previous-line arg))

(defun modal-select-lines()
  (interactive)
  (if (not (region-active-p))
      (modal-select-whole-line)
    (cond ((= (point)
              (region-end))
           (let ((position (bounds-of-thing-at-point 'line)))
             (goto-char (cdr position)))
           (save-excursion (goto-char (region-beginning))
                           (push-mark (line-beginning-position) t t)))
          ((= (point)
              (region-beginning))
           (goto-char (line-beginning-position))
           (save-excursion (goto-char (region-end))
                           (let ((position (bounds-of-thing-at-point 'line)))
                             (push-mark (cdr position) t t)))))))

;;;; modify
(defun modal-open-line-above(arg)
  (interactive "p")
  (goto-char (line-beginning-position))
  (newline arg)
  (backward-char arg)
  (indent-for-tab-command)
  (modal--switch-state 'insert))

(defun modal-open-line-below(arg)
  (interactive "p")
  (goto-char (line-end-position))
  (newline arg)
  (indent-for-tab-command)
  (modal--switch-state 'insert))

(defun modal-delete-char (arg)
  (interactive "p")
  (delete-char 1))
(defun modal-save-and-delete-char (arg)
  (interactive "p")
  (kill-region (point)
               (1+ (point))))

(defun modal-delete ()
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning)
                   (region-end))))

(defun modal-save-and-delete ()
  (interactive)
  (when (region-active-p)
    (kill-region (region-beginning)
                 (region-end))))

(defun modal-change()
  (interactive)
  (if (region-active-p)
      (delete-region (region-beginning)
                     (region-end))
    (delete-region (point)
                   (1+ (point))))
  (modal--switch-state 'insert))

(defun modal-save-and-change()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning)
                   (region-end))
    (kill-region (point)
                 (1+ (point))))
  (modal--switch-state 'insert))

(defun modal-delete-boundary()
  (interactive)
  (when (region-active-p)
    (delete-char 1)))


(defun modal--suround-insert (begin end)
  (when (region-active-p)
    (save-excursion (goto-char (region-end))
                    (insert end)
                    (goto-char (region-beginning))
                    (insert begin))))

(defun modal-suround-insert-par()
  (interactive))

(provide 'modal-command)
;;; modal-command.el ends here
