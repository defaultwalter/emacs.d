;;; edit.el --- edit                                 -*- lexical-binding: t; -*-

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
  format-all                            ;格式化代码，支持多种格式
  :ensure t
  :defer t
  :init (user/leader-key "cf" '(format-all-buffer :name "format")))

(use-package
  drag-stuff
  :ensure t
  :defer t
  :after evil
  :bind (:map evil-visual-state-map
              ("K" . drag-stuff-up)
              ("J" . drag-stuff-down)
              :map evil-normal-state-map
              ("K" . drag-stuff-up)
              ("J" . drag-stuff-down))
  :config                               ;
  (drag-stuff-global-mode 1))

(use-package
  expand-region                         ;选择区域
  :ensure t
  :defer t
  :after evil
  :bind (:map evil-normal-state-map
              ("<S-return>" . er/expand-region)
              ("S-RET" . er/expand-region)))

(use-package
  hungry-delete                         ; 可以删除前面所有的空白字符
  :ensure t
  :defer t
  :custom (hungry-delete-join-reluctantly t)
  :hook (prog-mode . hungry-delete-mode))

(use-package
  smart-comment                         ;注释插件
  :ensure t
  :defer t
  :bind ("C-/" . smart-comment)
  :init (user/leader-key "cc" '(smart-comment :name "comment")))

(use-package
  auto-sudoedit                         ;自动请求sudo权限
  :if (or (eq system-type 'gnu/linux)
          (eq system-type 'darwin))
  :ensure t
  :config (auto-sudoedit-mode 1))

(use-package
  undo-tree                             ;撤销重做可视化
  :ensure t
  :config (global-undo-tree-mode))

(provide 'edit)
;;; edit.el ends here
