;;; misc.el --- misc                                 -*- lexical-binding: t; -*-

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
  disable-mouse
  :ensure t
  ;; :disabled
  :config (when (boundp 'evil-mode)
            (mapc #'disable-mouse-in-keymap (list evil-motion-state-map evil-normal-state-map
                                                  evil-visual-state-map evil-insert-state-map)))
  (global-disable-mouse-mode))

(use-package
  sis                                   ; 自动切换输入法
  :ensure t
  :config                                  ;
  (setq sis-respect-prefix-and-buffer nil) ;开启会导致 which-key 翻页失效
  (cond ((eq system-type 'darwin)
         (if (executable-find "macism")
             (sis-ism-lazyman-config "com.apple.keylayout.ABC" "com.apple.inputmethod.SCIM.ITABC")
           (message
            "SIS need to install macism. use ‘brew tap laishulu/macism;brew install macism’ to install it.")))
        ((eq system-type 'gnu/linux)
         (sis-ism-lazyman-config "1" "2" 'fcitx)))
  (sis-global-respect-mode t))


(use-package
  xclip
  :ensure t
  :if (not (display-graphic-p))
  :config (xclip-mode 1))

(use-package
  popwin                                ; 使用弹出窗口显示部分Buffer
  :ensure t
  :disabled
  :config                               ;
  (popwin-mode 1))

(use-package openwith
  :ensure t
  :disabled)

(use-package
  buffer-move                           ; 交换两个window的buffer
  :ensure t
  :defer t
  :init                                 ;
  (user/leader-key "b <left>" '(buf-move-left :name "move to left window"))
  (user/leader-key "b <down>" '(buf-move-down :name "move to down window"))
  (user/leader-key "b <up>" '(buf-move-up :name "move to up window"))
  (user/leader-key "b <right>" '(buf-move-right :name "move to right window"))
  (setq buffer-move-stay-after-swap t)
  (setq buffer-move-behavior 'move))

(provide 'misc)
;;; misc.el ends here
