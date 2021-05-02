;;; modal-core.el --- modal core                     -*- lexical-binding: t; -*-

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


(defvar modal--visual-mode-maps nil
  "Modal map for major mode")

(defvar modal--normal-mode-maps nil
  "Modal map for major mode")

(defvar modal--motion-mode-maps nil
  "Modal map for major mode")

(defun modal--apply-mode-maps()
  "Apply mode map"
  (let ((normal-mode-map (or (cdr (assoc major-mode modal--normal-mode-maps))
                             modal-normal-state-map))
        (motion-mode-map (or (cdr (assoc major-mode modal--motion-mode-maps))
                             modal-motion-state-map))
        (visual-mode-map (or (cdr (assoc major-mode modal--visual-mode-maps))
                             modal-visual-state-map)))
    (push `(modal-normal-mode . ,normal-mode-map) minor-mode-overriding-map-alist)
    (push `(modal-motion-mode . ,motion-mode-map) minor-mode-overriding-map-alist)
    (push `(modal-visual-mode . ,visual-mode-map) minor-mode-overriding-map-alist)))

(add-hook 'after-change-major-mode-hook #'modal--apply-mode-maps)

(define-minor-mode modal-normal-mode "Modal normal state."
  nil
  " ModalNormal"
  modal-normal-state-map
  (when modal-normal-mode               ;
    (modal-insert-mode -1)
    (modal-motion-mode -1)
    (modal-visual-mode -1)))

(define-minor-mode modal-motion-mode "Modal motion state."
  nil
  " ModalMotion"
  modal-motion-state-map
  (when modal-motion-mode               ;
    (modal-insert-mode -1)
    (modal-normal-mode -1)
    (modal-visual-mode -1)))

(define-minor-mode modal-visual-mode "Modal motion state."
  nil
  " ModalMotion"
  modal-visual-state-map
  (when modal-visual-mode               ;
    (modal-insert-mode -1)
    (modal-normal-mode -1)
    (modal-motion-mode -1)
    (add-hook 'post-command-hook #'modal--visual-mode-post-command-handler nil t)
    (add-hook 'deactivate-mark-hook #'modal--exit-visual nil t))
  (unless modal-visual-mode             ;
    (remove-hook 'post-command-hook #'modal--visual-mode-post-command-handler t)
    (remove-hook 'deactivate-mark-hook #'modal--exit-visual t)))


(defun modal--exit-visual()
  (deactivate-mark)
  (modal--quit))

(defun modal--visual-mode-post-command-handler
    (&optional
     command)
  (when modal-visual-mode               ;
    (let ((command (or command
                       this-command)))
      (when (or (eq command #'keyboard-quit)
                deactivate-mark
                (not (region-active-p)))
        (deactivate-mark)
        (modal--quit)))))

(define-minor-mode modal-insert-mode "Modal insert state."
  nil
  " ModalInsert"
  modal-insert-state-map
  (when modal-insert-mode               ;
    (modal-normal-mode -1)
    (modal-motion-mode -1)
    (modal-visual-mode -1)
    (add-hook 'post-command-hook #'modal--insert-mode-post-command-handler nil t))
  (unless modal-insert-mode             ;
    (remove-hook 'post-command-hook #'modal--insert-mode-post-command-handler t)))

(defun modal--insert-mode-post-command-handler
    (&optional
     command)
  (when modal-insert-mode               ;
    (let ((command (or command
                       this-command)))
      (when  (eq command #'keyboard-quit)
        (modal--quit) ))))


(defun modal--switch-visual-state()
  (modal--switch-state 'visual))

;;;; define mode
(defun modal--enable ()
  "Enable Modal mode"
  (if (derived-mode-p 'special-mode)
      (modal-motion-mode 1)
    (modal-normal-mode 1))
  (add-hook 'modal-normal-mode-hook #'modal--refresh-cursor nil t)
  (add-hook 'modal-motion-mode-hook #'modal--refresh-cursor nil t)
  (add-hook 'modal-visual-mode-hook #'modal--refresh-cursor nil t)
  (add-hook 'modal-insert-mode-hook #'modal--refresh-cursor nil t)
  (add-hook 'activate-mark-hook #'modal--switch-visual-state nil t))

(defun modal--disable ()
  "Disable Modal mode"
  (modal-normal-mode -1)
  (modal-motion-mode -1)
  (modal-insert-mode -1)
  (modal-visual-mode -1)
  (remove-hook 'activate-mark-hook #'modal--switch-visual-state t))


(define-minor-mode modal-mode "Toggle `modal-mode` minor mode."
  nil
  " Modal"
  modal-mode-map
  (if modal-mode                        ;
      (modal--enable)
    (modal--disable)))

(defun modal--current-state ()
  "Current state."
  (cond ((bound-and-true-p modal-insert-mode) 'insert)
        ((bound-and-true-p modal-normal-mode) 'normal)
        ((bound-and-true-p modal-motion-mode) 'motion)
        ((bound-and-true-p modal-visual-mode) 'visual)))

(defun modal--switch-state (state)
  "Switch state."
  (cond ((equal state 'normal)
         (modal-normal-mode 1))
        ((equal state 'motion)
         (modal-motion-mode 1))
        ((equal state 'insert)
         (modal-insert-mode 1))
        ((equal state 'visual)
         (modal-visual-mode 1))))

(defun modal--quit()
  "Quit insert or visual mode"
  (if (seq-contains-p modal-motion-mode-list major-mode (lambda (item mode)
                                                          (derived-mode-p item)))
      (modal-motion-mode 1)
    (modal-normal-mode 1)))


(defun modal--refresh-cursor()
  "Change cursor color."
  (let ((cursor-style (cond ((bound-and-true-p modal-normal-mode)
                             (or modal-normal-cursor
                                 `(bar . ,(face-foreground 'link))))
                            ((bound-and-true-p modal-motion-mode)
                             (or modal-motion-cursor
                                 `(bar . ,(face-foreground 'success))))
                            ((bound-and-true-p modal-visual-mode)
                             (or modal-visual-cursor
                                 `(bar . ,(face-foreground 'warning))))
                            ((bound-and-true-p modal-insert-mode)
                             (or modal-insert-cursor
                                 `(bar . ,(face-foreground 'error))))
                            (t `(bar . ,(face-foreground 'default))))))
    (setq-local cursor-type (car cursor-style))
    (set-cursor-color (cdr cursor-style))))

(defun modal--window-state-change-handler
    (&rest
     args)
  "Update modal state when Window state change."
  (modal--refresh-cursor))

(defun modal-mode--minibuffer-setup()
  "Set modal-mode state when minibuffer avtive."
  (setq-local modal-normal-mode nil))

(defun modal--global-enable()
  "Enable Modal global mode"
  (add-hook 'window-state-change-functions #'modal--window-state-change-handler)
  (add-hook 'minibuffer-setup-hook #'modal-mode--minibuffer-setup))

(defun modal--global-disable()
  "Disable Modal global mode"
  (remove-hook 'window-state-change-functions #'modal--window-state-change-handler)
  (remove-hook 'minibuffer-setup-hook #'modal-mode--minibuffer-setup))

(defun modal--turn-on()
  (unless (minibufferp)
    (modal-mode 1)))

(define-globalized-minor-mode modal-global-mode modal-mode
  modal--turn-on
  (if modal-mode (modal--global-enable)
    (modal--global-disable )))

(provide 'modal-core)
;;; modal-core.el ends here
