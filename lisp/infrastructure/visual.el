;;; visual.el --- visual                             -*- lexical-binding: t; -*-

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
  highlight-indent-guides               ;高亮缩进
  :ensure t
  :defer t
  :custom                               ;
  (highlight-indent-guides-suppress-auto-error t)
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-character ?┊)
  :hook ((prog-mode conf-mode) . highlight-indent-guides-mode)
  :config                               ;
  (unless (display-graphic-p)
    (set-face-foreground 'highlight-indent-guides-character-face "black")))

(use-package
  rainbow-mode
  :ensure t
  :defer 1
  :hook (prog-mode . rainbow-mode)
  :config
  ;; 默认的会文本属性背景色显示颜色，会与高亮行插件冲突，通过重写这个方法，调换前景与背景色来解决这个问题
  (defun rainbow-colorize-match (color &optional match)
    "Return a matched string propertized with a face whose
  background is COLOR. The foreground is computed using
  `rainbow-color-luminance', and is either white or black."
    (let* ((match (or match
                      0))
           (color-string
            (buffer-substring-no-properties
             (match-beginning match)
             (match-end match))))
      ;; (message "color : %s" color-string)
      (put-text-property (match-beginning match)
                         (match-end match) 'face
                         `(:foreground ,color)))))

(use-package
  rainbow-delimiters                    ;彩虹括号
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package
  highlight-parentheses                 ;高亮当前括号
  :ensure t
  :disabled
  :defer t
  :custom (hl-paren-highlight-adjacent t)
  (hl-paren-colors '("cyan"))           ; 设置高亮括号颜色
  :hook (prog-mode . highlight-parentheses-mode))

(use-package
  diff-hl
  :ensure t
  :defer 1
  :config                               ;
  (global-diff-hl-mode))

(use-package
  beacon                                ; 跳转后,显示光标位置
  :if (display-graphic-p)
  :ensure t
  :disabled
  :config (beacon-mode t))

(provide 'visual)
;;; visual.el ends here
