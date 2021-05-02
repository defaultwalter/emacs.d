;;; modal-modern.el --- modal modern                 -*- lexical-binding: t; -*-

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

(defvar modal-modern-map (make-sparse-keymap)
  "Global keymap for Modal")
(define-key modal-modern-map (kbd "C-z") #'undo)
(define-key modal-modern-map (kbd "C-x") #'kill-region)
(define-key modal-modern-map (kbd "C-c") #'kill-ring-save)
(define-key modal-modern-map (kbd "C-v") #'yank)
(define-key modal-modern-map (kbd "C-s") #'save-buffer)
(add-to-list 'emulation-mode-map-alists `((modal-modern-mode . ,modal-modern-map)))

(define-minor-mode modal-modern-mode "Modal normal state."
  nil
  nil
  modal-modern-map)


(provide 'modal-modern)
;;; modal-modern.el ends here
