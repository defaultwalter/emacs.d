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
;; (define-key modal-modern-map [mouse-1] #'mouse-set-point)
;; (define-key modal-modern-map [down-mouse-1] #'mouse-drag-region)
(define-key modal-modern-map [mouse-4] #'mwheel-scroll)
(define-key modal-modern-map [mouse-5] #'mwheel-scroll)
(define-key modal-modern-map [mouse-6] #'mwheel-scroll)
(define-key modal-modern-map [mouse-7] #'mwheel-scroll)


(substitute-key-definition 'self-insert-command 'self-insert-command modal-modern-map global-map)

(substitute-key-definition 'newline 'newline modal-modern-map global-map)
(substitute-key-definition 'indent-for-tab-command 'indent-for-tab-command modal-modern-map global-map)
(substitute-key-definition 'delete-char 'delete-char modal-modern-map global-map)
(substitute-key-definition 'delete-backward-char 'delete-backward-char modal-modern-map global-map)

(substitute-key-definition 'move-beginning-of-line 'move-beginning-of-line modal-modern-map global-map)
(substitute-key-definition 'move-end-of-line 'move-end-of-line modal-modern-map global-map)

(substitute-key-definition 'right-char 'right-char modal-modern-map global-map)
(substitute-key-definition 'left-char 'left-char modal-modern-map global-map)

(substitute-key-definition 'backward-char 'backward-char modal-modern-map global-map)
(substitute-key-definition 'forward-char 'forward-char modal-modern-map global-map)

(substitute-key-definition 'next-line 'next-line modal-modern-map global-map)
(substitute-key-definition 'previous-line 'previous-line modal-modern-map global-map)

(substitute-key-definition 'swiper 'swiper modal-modern-map global-map)

(substitute-key-definition 'keyboard-quit 'keyboard-quit modal-modern-map global-map)

;; (use-global-map modal-modern-map)


;; (add-to-list 'emulation-mode-map-alists `((modal-modern-mode . ,modal-modern-map))
;; (define-minor-mode modal-modern-mode "Modal modern."
;;   nil
;;   nil
;;   modal-modern-map)
(provide 'modal-modern)
;;; modal-modern.el ends here
