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
(defvar modal-modern-mode-map (make-sparse-keymap)
  "Global keymap for Modal modern")
(define-minor-mode modal-modern-mode "Modal normal state."
  :init-value nil
  :lighter " Modern"
  :keymap modal-modern-mode-map
  (message "hello"))

(defun modal-modern--lookup-minor-mode-key (key)
  "Look up key bindings from minor mode"
  (let ((maps nil))
    (mapc
     (lambda (map)
       (when (and (symbolp (car map)) (symbol-value (car map)))
         (add-to-list 'maps (lookup-key (cdr map) key))))
     minor-mode-map-alist)
    (make-composed-keymap maps)))

(defun modal-modern--keymap (key)
  "Make keymap from key"
  (let ((map (make-composed-keymap (list
                                    (modal-modern--lookup-minor-mode-key key)
                                    (local-key-binding key)
                                    (global-key-binding key)))))
    map))

;; (defun modal-modern-with-space (item)
;;   "Return ITEM key with all Ctrl status of binding toggled."
;;   (cond
;; 	((and (listp item)
;; 	      (not (listp (cdr item))))
;; 	 (cons (rebinder-toggle-ctrl (car item)) (cdr item)))
;; 	((listp item)
;; 	 (mapcar 'rebinder-toggle-ctrl item))
;; 	((event-basic-type item)
;; 	 (let ((mods (event-modifiers item))
;; 	       (key (event-basic-type item)))
;; 	   (if (member 'control mods)
;; 	       (event-convert-list (append (remove 'control mods) (list key)))
;; 	     (event-convert-list (append (append mods '(control)) (list key))))))
;; 	(t item)))


(add-to-list 'minor-mode-overriding-map-alist (cons 'modal-modern-mode  modal-modern-mode-map))

(defun modal-modern-dynamic-keymap(key)
  "Dynamic keymap of key"
  `(menu-item
    ,""
    nil
    :filter
    (lambda (&optional _)
      ,`(modal-modern--keymap ,key))))


;; (modal-leader-set-key "x" (modal-modern-dynamic-keymap (kbd "C-x")))
(define-key modal-modern-mode-map (kbd "C-.") (modal-modern-dynamic-keymap (kbd "C-x")))
(define-key modal-modern-mode-map (kbd "C-,") (modal-modern-dynamic-keymap (kbd "C-c")))
(define-key modal-modern-mode-map (kbd "C-z") #'modal-undo)
(define-key modal-modern-mode-map (kbd "C-S-z") #'modal-redo)
(define-key modal-modern-mode-map (kbd "C-x") #'modal-save-and-delete)
(define-key modal-modern-mode-map (kbd "C-c") #'modal-save)
(define-key modal-modern-mode-map (kbd "C-v") #'modal-paste-before)

;; (define-key modal-modern-map [mouse-1] #'mouse-set-point)
;; (define-key modal-modern-map [down-mouse-1] #'mouse-drag-region)
;; (define-key modal-modern-map [mouse-4] #'mwheel-scroll)
;; (define-key modal-modern-map [mouse-5] #'mwheel-scroll)
;; (define-key modal-modern-map [mouse-6] #'mwheel-scroll)
;; (define-key modal-modern-map [mouse-7] #'mwheel-scroll)


;; (substitute-key-definition 'self-insert-command 'self-insert-command modal-modern-map global-map)

;; (substitute-key-definition 'newline 'newline modal-modern-map global-map)
;; (substitute-key-definition 'indent-for-tab-command 'indent-for-tab-command modal-modern-map global-map)
;; (substitute-key-definition 'delete-char 'delete-char modal-modern-map global-map)
;; (substitute-key-definition 'delete-backward-char 'delete-backward-char modal-modern-map global-map)

;; (substitute-key-definition 'move-beginning-of-line 'move-beginning-of-line modal-modern-map global-map)
;; (substitute-key-definition 'move-end-of-line 'move-end-of-line modal-modern-map global-map)

;; (substitute-key-definition 'right-char 'right-char modal-modern-map global-map)
;; (substitute-key-definition 'left-char 'left-char modal-modern-map global-map)

;; (substitute-key-definition 'backward-char 'backward-char modal-modern-map global-map)
;; (substitute-key-definition 'forward-char 'forward-char modal-modern-map global-map)

;; (substitute-key-definition 'next-line 'next-line modal-modern-map global-map)
;; (substitute-key-definition 'previous-line 'previous-line modal-modern-map global-map)

;; (substitute-key-definition 'swiper 'swiper modal-modern-map global-map)

;; (substitute-key-definition 'keyboard-quit 'keyboard-quit modal-modern-map global-map)

;; (use-global-map modal-modern-map)


;; (add-to-list 'emulation-mode-map-alists `((modal-modern-mode . ,modal-modern-map))
;; (define-minor-mode modal-modern-mode "Modal modern."
;;   nil
;;   nil
;;   modal-modern-map)
(provide 'modal-modern)
;;; modal-modern.el ends here
