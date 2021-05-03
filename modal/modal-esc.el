;;; modal-esc.el --- fix tui escape key              -*- lexical-binding: t; -*-

;; Copyright (C) 2021  meetcw

;; Author: meetcw <meetcw@outlook.com>
;; Keywords: terminals

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
(defvar modal-esc-delay 0.1)
(defvar modal-esc-mode nil)

(defun modal-esc-mode
    (&optional
     arg)
  (cond ((or
          (null arg)
          (eq arg 0))
         (modal-esc-mode (if modal-esc-mode -1 +1)))
        ((> arg 0)
         (unless modal-esc-mode
           (setq modal-esc-mode t)
           (add-hook 'after-make-frame-functions #'modal-init-esc)
           (mapc #'modal-init-esc (frame-list))))
        ((< arg 0)
         (when modal-esc-mode (remove-hook 'after-make-frame-functions #'modal-init-esc)
               (mapc #'modal-deinit-esc (frame-list))
               (setq modal-esc-mode nil)))))

(defun modal-init-esc (frame)
  (with-selected-frame frame (let ((term (frame-terminal frame)))
                               (when (not (terminal-parameter term 'modal-esc-map))
                                 (let ((modal-esc-map (lookup-key input-decode-map [?\e])))
                                   (set-terminal-parameter term 'modal-esc-map modal-esc-map)
                                   (define-key input-decode-map [?\e]
                                     `(menu-item "" ,modal-esc-map
                                                 :filter ,#'modal-esc)))))))

(defun modal-deinit-esc (frame)
  (with-selected-frame frame (let ((term (frame-terminal frame)))
                               (when (terminal-live-p term)
                                 (let ((modal-esc-map (terminal-parameter term 'modal-esc-map)))
                                   (when modal-esc-map (define-key input-decode-map [?\e]
                                                        modal-esc-map)
                                         (set-terminal-parameter term 'modal-esc-map nil)))))))

(defun modal-esc (map)
  (if (and (let ((keys (this-single-command-keys)))
             (and (> (length keys) 0)
                  (= (aref keys (1- (length keys))) ?\e)))
           (sit-for modal-esc-delay))
      (prog1 [escape]
        (when defining-kbd-macro (end-kbd-macro)
              (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
              (start-kbd-macro t t))) map))


(provide 'modal-esc)
;;; modal-esc.el ends here
