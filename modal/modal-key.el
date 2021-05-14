;;; modal-key.el ---                              -*- lexical-binding: t; -*-

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

(defvar modal-mode-map (make-sparse-keymap)
  "Global keymap for Modal")

(defvar modal-insert-state-map (let ((keymap (make-keymap))) keymap)
  "Keymap for Modal insert state.")

(defvar modal-visual-state-map (let ((keymap (make-keymap)))
                                 (suppress-keymap keymap t) keymap)
  "Keymap for Modal visual state.")

(defvar modal-motion-state-map (let ((keymap (make-keymap)))
                                 (suppress-keymap keymap t) keymap)
  "Keymap for Modal motion state.")

(defvar modal-normal-state-map
  (let ((keymap (make-sparse-keymap)))
    (suppress-keymap keymap t) keymap)
  "Keymap for Modal normal state.")



(defvar modal--normal-state-maps nil
  "Modal map for major mode")

(defvar modal--motion-state-maps nil
  "Modal map for major mode")

(defvar modal--visual-state-maps nil
  "Modal map for major mode")

(defvar modal--insert-state-maps nil
  "Modal map for major mode")

(defun modal--apply-mode-maps()
  "Apply mode map"
  (let ((normal-state-map (or (cdr (assoc major-mode modal--normal-state-maps))
                              modal-normal-state-map))
        (motion-state-map (or (cdr (assoc major-mode modal--motion-state-maps))
                              modal-motion-state-map))
        (visual-state-map (or (cdr (assoc major-mode modal--visual-state-maps))
                              modal-visual-state-map))
        (insert-state-map (or (cdr (assoc major-mode modal--insert-state-maps))
                              modal-insert-state-map)))
    (push `(modal-normal-state-mode . ,normal-state-map) minor-mode-overriding-map-alist)
    (push `(modal-motion-state-mode . ,motion-state-map) minor-mode-overriding-map-alist)
    (push `(modal-visual-state-mode . ,visual-state-map) minor-mode-overriding-map-alist)
    (push `(modal-insert-state-mode . ,insert-state-map) minor-mode-overriding-map-alist)))

(add-hook 'after-change-major-mode-hook #'modal--apply-mode-maps)

(defun modal--define-key(map key def)
  "define key."
  (let* ((def (if (listp def)
                  (if (keywordp (car def)) def (cons :command def))
                (list :command def)))
         (ignore (plist-get def
                            :ignore))
         (command (plist-get def
                             :command))
         (name (plist-get def
                          :which-key)))
    (unless ignore (define-key map (read-kbd-macro key) command))
    (when name                          ;
      (with-eval-after-load 'which-key  (which-key-add-keymap-based-replacements map key `(,name .
                                                                                                 ,command))))))

(defun modal-set-key(state key def)
  "Set state key."
  (let ((map (cond ((eq state 'normal) modal-normal-state-map)
                   ((eq state 'motion) modal-motion-state-map)
                   ((eq state 'visual) modal-visual-state-map)
                   ((eq state 'insert) modal-insert-state-map))))
    (modal--define-key map key def)))


(defun modal-normal-set-key(key def)
  "Set normal state state key."
  (modal-set-key 'normal key def))
(defun modal-motion-set-key(key def)
  "Set motion state state key."
  (modal-set-key 'motion key def))
(defun modal-visual-set-key(key def)
  "Set visual state key."
  (modal-set-key 'visual key def))
(defun modal-insert-set-key(key def)
  "Set insert state key."
  (modal-set-key 'insert key def))

(defun modal-set-key-for-mode (mode state key def)
  "Set state key for major mode."
  (let* ((normal-state-map (cdr (assoc mode modal--normal-state-maps)))
         (motion-state-map (cdr (assoc mode modal--motion-state-maps)))
         (visual-state-map (cdr (assoc mode modal--visual-state-maps)))
         (insert-state-map (cdr (assoc mode modal--insert-state-maps))))
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
      (push (cons mode motion-state-map) modal--motion-state-maps))
    (unless insert-state-map
      (setq insert-state-map (make-sparse-keymap))
      (set-keymap-parent insert-state-map modal-insert-state-map)
      (push (cons mode motion-state-map) modal--motion-state-maps))
    (let ((map (cond ((eq state 'normal) normal-state-map)
                     ((eq state 'motion) motion-state-map)
                     ((eq state 'visual) visual-state-map)
                     ((eq state 'insert) insert-state-map))))
      (modal--define-key map key def))))

(defun modal-normal-set-key-for-mode(mode key def)
  "Set normal state key for major mode."
  (modal-set-key-for-mode mode 'normal key def))
(defun modal-motion-set-key-for-mode(mode key def)
  "Set motion state key for major mode."
  (modal-set-key-for-mode mode 'motion key def))
(defun modal-visual-set-key-for-mode(mode key def)
  "Set visual state key for major mode."
  (modal-set-key-for-mode mode 'visual key def))
(defun modal-insert-set-key-for-mode(mode key def)
  "Set insert state key for major mode."
  (modal-set-key-for-mode mode 'insert key def))

(provide 'modal-key)
;;; modal-key.el ends here
