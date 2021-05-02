;;; modal-leader.el --- modal leader                 -*- lexical-binding: t; -*-

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

(require 'modal-option)
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
         (which-key (plist-get def
                               :which-key)))
    (unless ignore (define-key map (read-kbd-macro key) command))
    (when (and (boundp 'which-key) which-key) (which-key-add-keymap-based-replacements map key which-key))))

(defun modal-leader-set-key(key def)
  "Set key for leader."
  (modal-leader--define-key modal-leader--default-map key def))

(defun modal-leader-set-key-for-mode (mode key def)
  "Set key for leader."
  (let* ((normal-mode-map (cdr (assoc mode modal--normal-mode-maps)))
         (motion-mode-map (cdr (assoc mode modal--motion-mode-maps)))
         (visual-mode-map (cdr (assoc mode modal--visual-mode-maps)))
         (leader-map (cdr (assoc mode modal-leader--mode-maps))))
    (unless normal-mode-map
      (setq normal-mode-map (make-sparse-keymap))
      (set-keymap-parent normal-mode-map modal-normal-state-map)
      (push (cons mode normal-mode-map) modal--normal-mode-maps))
    (unless motion-mode-map
      (setq motion-mode-map (make-sparse-keymap))
      (set-keymap-parent motion-mode-map modal-motion-state-map)
      (push (cons mode motion-mode-map) modal--motion-mode-maps))
    (unless visual-mode-map
      (setq visual-mode-map (make-sparse-keymap))
      (set-keymap-parent visual-mode-map modal-visual-state-map)
      (push (cons mode motion-mode-map) modal--motion-mode-maps))
    (unless leader-map
      (setq leader-map (make-sparse-keymap))
      (set-keymap-parent leader-map modal-leader--default-map)
      (push (cons mode leader-map) modal-leader--mode-maps)
      (define-key normal-mode-map (read-kbd-macro modal-leader-key) leader-map)
      (define-key motion-mode-map (read-kbd-macro modal-leader-key) leader-map)
      (define-key visual-mode-map (read-kbd-macro modal-leader-key) leader-map))
    (modal-leader--define-key leader-map key def)))

(provide 'modal-leader)
;;; modal-leader.el ends here
