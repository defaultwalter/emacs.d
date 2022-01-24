;;; note.el ---                                      -*- lexical-binding: t; -*-

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

(use-package
  org
  :ensure org-plus-contrib
  :defer t
  :init                                 ;
  (setq org-preview-latex-image-directory (expand-file-name "ltximg/" user-emacs-directory))
  (setq org-hide-emphasis-markers t) ; 隐藏强调符号（加粗，下划线等等）
  (setq org-pretty-entities nil)       ; 可以显示上标下标
  (setq org-edit-src-content-indentation 2) ; 设置代码内容缩进
  (setq org-src-preserve-indentation nil)
  (setq org-src-tab-acts-natively t)
  ;; (setq org-fontify-done-headline t) ; 标题状态为 Done 的时候修改标题样式
  (setq org-hide-leading-stars t)       ; 隐藏标题多余的星号
  (setq org-startup-folded 'nofold)     ; 是否默认开启折叠
  (setq org-cycle-separator-lines 2)
  (setq org-return-follows-link t)      ; 回车链接跳转
  (setq org-image-actual-width nil) ; 图片宽度
  ;; (setq org-html-head-include-default-style nil) ;默认导出不要包含样式
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (add-hook 'org-mode-hook (lambda ()
                             (setq prettify-symbols-alist '(("#+BEGIN_SRC" . "  ")
                                                            ("#+END_SRC" . "  ")
                                                            ("#+begin_src" . "  ")
                                                            ("#+end_src" . "  ")
                                                            ("#+BEGIN_QUOTE" . "  ")
                                                            ("#+END_QUOTE" . "  ")
                                                            ("#+begin_quote" . "  ")
                                                            ("#+end_quote" . "  ")))
                             ;; (setq prettify-symbols-unprettify-at-point 'right-edge)
                             ;; (prettify-symbols-mode 1)
                             (setq truncate-lines nil)
                             (org-display-inline-images t t) ; 显示图片
                             
                             ;; (org-indent-mode 1) ; 缩进模式 (和 truncate-lines 同时使用会导致 wrap-prefix 背景色总是使用默认的背色)
                             (visual-fill-column-mode 1)
                             (org-align-tags t)
                             (add-hook 'before-save-hook (lambda()
                                                           ;; 保存时 对齐 tag
                                                           (org-align-tags t)) nil 'local)))

  (defun +org-rename-buffer()
    (interactive)
    (when-let ((title (pcase (org-collect-keywords '("TITLE"))
                        (`(("TITLE" . ,val))
                         (org-link-display-format (car val)))))
               (filename (when buffer-file-name (file-name-nondirectory buffer-file-name))))
      (rename-buffer (format "%s<%s>" title filename) t))
    (add-hook 'after-save-hook #'+org-rename-buffer nil t))

  (add-hook 'org-mode-hook #'+org-rename-buffer)

  (setq-default org-confirm-babel-evaluate nil)
  :config                               ;
  (require 'ob-dot)
  (setq-default org-plantuml-exec-mode 'plantuml)
  (setq-default org-plantuml-jar-path "")
  (require 'ob-plantuml)
  (require 'ob-python)
  (require 'ob-shell)
  (require 'ob-java)
  (require 'ob-js)
  (require 'ob-python)
  (require 'ob-latex)
  (require 'ox-freemind)
  (require 'org-tempo))

(use-package
  org-appear;自动切换预览元素
  :ensure t
  :custom ;
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  :hook (org-mode . org-appear-mode))

(use-package
  org-fragtog;自动切换预览 Latex 公式
  :ensure t
  :hook (org-mode . org-fragtog-mode))

(use-package
  org-superstar
  :ensure t
  :defer t
  :hook (org-mode . org-superstar-mode)
  :custom                               ;
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("✿" "❖" "●" "◉" "◍" "◎" "○" "◌"))
  (org-superstar-prettify-item-bullets t)
  (org-superstar-item-bullet-alist '((?* . ?*)
                                     (?+ . ?+)
                                     (?- . ?-)))
  (org-superstar-special-todo-items t)
  (org-superstar-todo-bullet-alist '(("TODO" . ?☐)
                                     ("DONE" . ?☑)))
  :hook (org-mode . org-superstar-mode)
  :init                                 ;
  (setq org-superstar-prettify-item-bullets t))

(use-package
  org-download
  :ensure t
  :defer 10
  :custom ;
  (org-download-image-dir "./Assets")
  (org-download-file-format-function +org-download-file-format-default)
  :init;
  (defun +org-download-file-format-default (filename)
    "It's affected by `org-download-timestamp'."
    (when-let ((filename filename)
               (extension (file-name-extension filename)))
      (if extension
          (concat
           (format-time-string org-download-timestamp)
           extension)
        (concat
         (format-time-string org-download-timestamp)
         "00")))))


(use-package
  visual-fill-column                    ;设置正文宽度
  :ensure t
  :defer t
  :commands (visual-fill-column-mode)
  :config                               ;
  (setq-default visual-fill-column-width 100)
  (setq-default visual-fill-column-center-text t))


(defcustom machine:note-directory (expand-file-name "notes" temporary-file-directory)
  "Note root directory"
  :type 'string
  :group 'machine)
(unless (file-directory-p machine:note-directory)
  (mkdir machine:note-directory))

(defcustom machine:agenda-directory (expand-file-name "agenda" temporary-file-directory)
  "Agenda root directory"
  :type 'string
  :group 'machine)
(unless (file-directory-p machine:agenda-directory)
  (mkdir machine:agenda-directory))

(unless (file-exists-p machine:agenda-directory)
  (mkdir machine:agenda-directory))
(setq org-agenda-files (mapcar (lambda (file)
                                 (expand-file-name file machine:agenda-directory))
                               (directory-files machine:agenda-directory nil ".*\.org")))
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))

(use-package
  valign
  :ensure t
  :hook (org-mode . valign-mode))
(provide 'purpose/note)
;;; note.el ends here
