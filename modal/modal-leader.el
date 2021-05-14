;;; modal-leader.el --- modal leader                 -*- lexical-binding: t; -*-

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

(require 'modal-option)
(require 'modal-key)

(defvar modal-leader--mode-maps nil
  "Leader map for mode")

(defvar modal-leader--default-map (make-sparse-keymap)
  "Keymap used for mode-independent leader bindings.")

(define-key modal-normal-state-map (read-kbd-macro modal-leader-key) modal-leader--default-map)
(define-key modal-motion-state-map (read-kbd-macro modal-leader-key) modal-leader--default-map)
(define-key modal-visual-state-map (read-kbd-macro modal-leader-key) modal-leader--default-map)

(defun modal-leader--define-key(map key def)
  "Leader define key."
  (let* ((def (if (commandp def)
                  (list :command def)
                (if (commandp (car def))
                    (cons :command def) def)))
         (ignore (plist-get def
                            :ignore))
         (command (plist-get def
                             :command))
         (name (plist-get def
                          :which-key)))
    (unless ignore (define-key map (read-kbd-macro key) command))
    (when name                     ;
      (with-eval-after-load 'which-key  (which-key-add-keymap-based-replacements map key
                                          `(,name . ,command))))))

(defun modal-leader-set-key(key def)
  "Set key for leader."
  (modal--define-key modal-leader--default-map key def))

(defun modal-leader-set-key-for-mode (mode key def)
  "Set key for leader."
  (let* ((normal-state-map (cdr (assoc mode modal--normal-state-maps)))
         (motion-state-map (cdr (assoc mode modal--motion-state-maps)))
         (visual-state-map (cdr (assoc mode modal--visual-state-maps)))
         (leader-map (cdr (assoc mode modal-leader--mode-maps))))
    (unless normal-state-map
      (setq normal-state-map (make-sparse-keymap))
      (set-keymap-parent normal-state-map modal-normal-state-map)
      (push (cons mode normal-state-map) modal--normal-state-maps))
    (unless motion-state-map
      (setq motion-state-map (make-sparse-keymap))
      (set-keymap-parent motion-state-map modal-motion-state-map)
      (push (cons mode motion-state-map) modal--motion-state-maps))
    (unless visual-state-map
      (setq visual-state-map (make-sparse-keymap))
      (set-keymap-parent visual-state-map modal-visual-state-map)
      (push (cons mode visual-state-map) modal--visual-state-maps))
    (unless leader-map
      (setq leader-map (make-sparse-keymap))
      (set-keymap-parent leader-map modal-leader--default-map)
      (push (cons mode leader-map) modal-leader--mode-maps)
      (define-key normal-state-map (read-kbd-macro modal-leader-key) leader-map)
      (define-key motion-state-map (read-kbd-macro modal-leader-key) leader-map)
      (define-key visual-state-map (read-kbd-macro modal-leader-key) leader-map))
    (modal--define-key leader-map key def)))

(provide 'modal-leader)
;;; modal-leader.el ends here
