;;; modal-core.el --- modal core                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Maf

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

(define-minor-mode modal-normal-state-mode "Modal normal state."
  nil
  " ModalNormal"
  modal-normal-state-map
  (when modal-normal-state-mode         ;
    (modal-insert-state-mode -1)
    (modal-motion-state-mode -1)
    (modal-visual-state-mode -1)))

(define-minor-mode modal-motion-state-mode "Modal motion state."
  nil
  " ModalMotion"
  modal-motion-state-map
  (when modal-motion-state-mode         ;
    (modal-insert-state-mode -1)
    (modal-normal-state-mode -1)
    (modal-visual-state-mode -1)))

(define-minor-mode modal-visual-state-mode "Modal motion state."
  nil
  " ModalMotion"
  modal-visual-state-map
  (when modal-visual-state-mode         ;
    (modal-insert-state-mode -1)
    (modal-normal-state-mode -1)
    (modal-motion-state-mode -1)
    (add-hook 'post-command-hook #'modal--visual-state-mode-post-command-handler nil t)
    (add-hook 'deactivate-mark-hook #'modal--switch-to-default-state nil t))
  (unless modal-visual-state-mode       ;
    (remove-hook 'post-command-hook #'modal--visual-state-mode-post-command-handler t)
    (remove-hook 'deactivate-mark-hook #'modal--switch-to-default-state t)
    (deactivate-mark t)))

(defun modal--visual-state-mode-post-command-handler
    (&optional
     command)
  (when modal-visual-state-mode         ;
    (let ((command (or command
                       this-command)))
      (when (eq command #'keyboard-quit)
        (deactivate-mark)
        (modal--switch-to-default-state)))))

(define-minor-mode modal-insert-state-mode "Modal insert state."
  nil
  " ModalInsert"
  modal-insert-state-map
  (when modal-insert-state-mode         ;
    (modal-normal-state-mode -1)
    (modal-motion-state-mode -1)
    (modal-visual-state-mode -1)
    (add-hook 'post-command-hook #'modal--insert-state-mode-post-command-handler nil t))
  (unless modal-insert-state-mode       ;
    (remove-hook 'post-command-hook #'modal--insert-state-mode-post-command-handler t)))

(defun modal--insert-state-mode-post-command-handler
    (&optional
     command)
  (when modal-insert-state-mode         ;
    (let ((command (or command
                       this-command)))
      (when (eq command #'keyboard-quit)
        (modal--switch-to-default-state) ))))


(defun modal--switch-visual-state()
  (modal--switch-state 'visual))

;;;; define mode
(defun modal--enable ()
  "Enable Modal mode"
  (modal--switch-to-default-state)
  (modal--refresh-cursor)
  (modal--update-indicator)
  (add-hook 'activate-mark-hook #'modal--switch-visual-state nil t))

(defun modal--disable ()
  "Disable Modal mode"
  (modal-normal-state-mode -1)
  (modal-motion-state-mode -1)
  (modal-insert-state-mode -1)
  (modal-visual-state-mode -1)
  (modal--refresh-cursor)
  (modal--update-indicator)
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
  (cond ((bound-and-true-p modal-insert-state-mode) 'insert)
        ((bound-and-true-p modal-normal-state-mode) 'normal)
        ((bound-and-true-p modal-motion-state-mode) 'motion)
        ((bound-and-true-p modal-visual-state-mode) 'visual)))

(defun modal--switch-state (state)
  "Switch state."
  (cond ((equal state 'normal)
         (modal-normal-state-mode 1))
        ((equal state 'motion)
         (modal-motion-state-mode 1))
        ((equal state 'insert)
         (modal-insert-state-mode 1))
        ((equal state 'visual)
         (modal-visual-state-mode 1)))
  (modal--refresh-cursor)
  (modal--update-indicator))

(defun modal--switch-to-default-state()
  "Quit insert or visual mode"
  (if (seq-contains-p modal-motion-mode-list major-mode (lambda (item mode)
                                                          (derived-mode-p item)))
      (modal--switch-state 'motion)
    (modal--switch-state 'normal))
  (deactivate-mark))



(defun modal--refresh-cursor()
  "Change cursor color."
  (let ((cursor-style (cond ((bound-and-true-p modal-normal-state-mode)
                             (or modal-normal-cursor
                                 `((bar . 3) . ,(face-foreground 'default))))
                            ((bound-and-true-p modal-motion-state-mode)
                             (or modal-motion-cursor
                                 `((bar . 3) . ,(face-foreground 'success))))
                            ((bound-and-true-p modal-visual-state-mode)
                             (or modal-visual-cursor
                                 `((bar . 2) . ,(face-foreground 'warning))))
                            ((bound-and-true-p modal-insert-state-mode)
                             (or modal-insert-cursor
                                 `((bar . 2) . ,(face-foreground 'error))))
                            (t `(bar . ,(face-foreground 'default))))))
    (setq-local cursor-type (car cursor-style))
    (set-cursor-color (cdr cursor-style))))


(defvar modal--indicator nil
  "Modal indicator")

(defun modal--render-indicator()
  (let* ((current-state (modal--current-state))
         (state-text (cdr (assoc current-state modal-indicator-alist)))
         (state-face (intern (format "modal-indicator-%s" (symbol-name current-state)))))
    (when current-state (propertize (format "%s" state-text) 'face state-face))))


(defun modal--update-indicator ()
  (let ((indicator (modal--render-indicator)))
    (setq-local modal--indicator indicator)))

(defun modal-indicator ()
  "Indicator showing current mode."
  (or modal--indicator
      (modal--update-indicator)))

(defun modal--window-state-change-handler
    (&rest
     args)
  "Update modal state when Window state change."
  (modal--refresh-cursor))

(defun modal-mode--minibuffer-setup()
  "Set modal-mode state when minibuffer avtive."
  (modal--switch-state 'insert))

(defun modal--global-enable()
  "Enable Modal global mode"
  (unless window-system (modal-esc-mode 1))
  (add-hook 'window-state-change-functions #'modal--window-state-change-handler)
  (add-hook 'minibuffer-setup-hook #'modal-mode--minibuffer-setup))

(defun modal--global-disable()
  "Disable Modal global mode"
  (unless window-system (modal-esc-mode -1))
  (remove-hook 'window-state-change-functions #'modal--window-state-change-handler)
  (remove-hook 'minibuffer-setup-hook #'modal-mode--minibuffer-setup))

(defun modal--turn-on()
  (modal-mode 1))

(define-globalized-minor-mode modal-global-mode modal-mode
  modal--turn-on
  (if modal-mode (modal--global-enable)
    (modal--global-disable )))

(defun modal-diagnose()
  (interactive)
  (message
   "modal global state: %s\nmodal state: %s\nnormal state: %s\nvisual state: %s\ninsert state: %s\nmotion state: %s"
   modal-global-mode modal-mode modal-normal-state-mode modal-visual-state-mode
   modal-insert-state-mode modal-motion-state-mode))

(provide 'modal-core)
;;; modal-core.el ends here
