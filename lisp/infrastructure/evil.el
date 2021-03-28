;;; evil.el --- evil                                 -*- lexical-binding: t; -*-

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

(use-package
  evil
  :ensure t
  :custom                               ;
  (evil-want-minibuffer nil)
  (evil-want-keybinding nil)
  (evil-want-integration t)
  :init (setq evil-undo-system 'undo-tree)
  (setq evil-want-C-i-jump nil)
  (when evil-want-C-i-jump (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward))
  (setq evil-emacs-state-cursor
        '("#51afef"
          bar))
  (setq evil-normal-state-cursor
        '("#51afef"
          box))
  (setq evil-visual-state-cursor
        '("#fe9865"
          box))
  (setq evil-insert-state-cursor
        '("#f96060"
          bar))
  (setq evil-replace-state-cursor
        '("#fd6698"
          hollow))
  (setq evil-operator-state-cursor
        '("#98ce65"
          hollow))
  (add-hook 'after-init-hook (lambda ()
                               (user/leader-key "wh" '(evil-window-left :name "left window"))
                               (user/leader-key "wj" '(evil-window-down :name "down window"))
                               (user/leader-key "wk" '(evil-window-up :name "up window"))
                               (user/leader-key "wl" '(evil-window-right :name "right window"))))
  :config                               ;
  (defun user/evil-shift-left-visual ()
    (interactive)
    (call-interactively 'evil-shift-left)
    (evil-normal-state)
    (evil-visual-restore))
  (defun user/evil-shift-right-visual ()
    (interactive)
    (call-interactively 'evil-shift-right)
    (evil-normal-state)
    (evil-define-key 'visual 'global ">" 'user/evil-shift-right-visual)
    (evil-define-key 'visual 'global "<" 'user/evil-shift-left-visual)
    (evil-define-key 'normal 'global (kbd "C-S-v") 'evil-visual-block)
    ;; (evil-mode 1)
    (evil-visual-restore)))

(use-package
  evil-collection
  :ensure t
  :after evil
  :config                               ;
  (evil-collection-init))

(use-package
  evil-surround
  :ensure t
  :after evil
  :config                               ;
  (global-evil-surround-mode 1))


(use-package
  evil-exchange
  :ensure t
  :after evil
  :config                               ;
  (evil-exchange-cx-install))

(use-package
  evil-terminal-cursor-changer
  :ensure t
  :after evil
  :unless (display-graphic-p)
  :config                               ;
  (evil-terminal-cursor-changer-activate))

(provide 'infrastructure/evil)
;;; evil.el ends here
