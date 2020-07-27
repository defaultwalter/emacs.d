(use-package
  exec-path-from-shell
  ;; :if (memq window-system '(ns mac))
  :ensure t
  :custom (exec-path-from-shell-check-startup-files nil)
  :config (exec-path-from-shell-initialize))
(use-package
  xclip
  :ensure t
  :if environment/terminal
  :config (xclip-mode 1))

;;;; ==============================================
;;;; 主题外观
;;;; ==============================================

(use-package
  all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package
  doom-modeline
  :ensure t
  :defer t
  :init (doom-modeline-init)
  (setq doom-modeline-height 20   )
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-enable-word-count 10)
  ;; (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (setq doom-modeline-buffer-file-name-style 'relative-to-project )
  (setq doom-modeline-modal-icon nil)
  :hook (after-init . doom-modeline-mode)
  :config)
(use-package
  doom-themes
  ;; :when (display-graphic-p)
  :ensure t
  :init (load-theme 'doom-one t)
  ;; :init (load-theme 'doom-one t)
  :config (set-face-attribute 'fringe nil
                              :foreground "#fc5c59"
                              :background (face-background 'default)))


;;;; ==============================================
;;;; 交互增强
;;;; ==============================================

(use-package
  ivy
  :ensure t
  :defer t
  :custom (ivy-use-virtual-buffers nil)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  :config
  (ivy-mode t))

(use-package
  smex                                  ;将命令按使用频率排序
  :ensure t
  :defer t
  :init                                 ;
  (setq smex-save-file (expand-file-name "smex-items" misc-file-directory)))

(use-package
  ivy-rich                              ; 在 M-x 和帮助中显示文档
  :ensure t
  :defer 1
  :init
  :config (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package
  counsel                               ;基于ivy的命令文件补全工具
  :ensure t
  :defer t
  :init                                 ;
  (w/create-leader-key "f" (lambda()
                             (interactive)
                             (let ((counsel-find-file-ignore-regexp "^\\."))
                               (counsel-find-file))) "find-file" file-map-prefix)
  (w/create-leader-key "a" 'counsel-find-file "find-all-file" file-map-prefix)
  (w/create-leader-key "r" 'counsel-recentf "recent-file" file-map-prefix)
  (w/create-leader-key "b" (lambda()
                             (interactive)
                             (let ((ivy-ignore-buffers '("\\` " "\\`\\*")))
                               (counsel-switch-buffer))) "switch-buffer" buffer-map-prefix)
  (w/create-leader-key "a" 'counsel-switch-buffer "switch-all-buffer" buffer-map-prefix)
  (defalias 'command 'counsel-M-x)
  (evil-leader/set-key "SPC" 'command)
  :bind (("M-x" . counsel-M-x))
  :config)

(use-package
  swiper                                ;基于ivy的增量搜索工具
  :ensure t
  :defer t
  :init                                 ;
  (w/create-leader-key "s" 'swiper "isearch" content-map-prefix)
  (w/create-leader-key "S" 'swiper-all "search-in-buffers" content-map-prefix)
  :bind                                 ;
  ("C-S-s" . swiper-all)
  ("C-s" . swiper))

(use-package
  command-log-mode                      ; 记录历史命令
  :ensure t
  :defer t
  :config (global-command-log-mode))

(use-package
  buffer-move                           ; 交换两个window的buffer
  :ensure t
  :defer t
  :init                                 ;
  (w/create-leader-key "h" 'buf-move-left "buffer-move-to-left-window" buffer-map-prefix)
  (w/create-leader-key "j" 'buf-move-down "buffer-move-to-down-window" buffer-map-prefix)
  (w/create-leader-key "k" 'buf-move-up "buffer-move-to-up-window" buffer-map-prefix)
  (w/create-leader-key "l" 'buf-move-right "buffer-move-to-right-window" buffer-map-prefix)
  (setq buffer-move-stay-after-swap t)
  (setq buffer-move-behavior 'move))

(use-package
  windresize                            ;调整window大小
  :ensure t
  :defer t
  :init                                 ;
  (w/create-leader-key "r" 'windresize "resize-window" window-map-prefix))

(use-package
  projectile                            ;project 插件
  :ensure t
  :custom                               ;
  (projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" misc-file-directory))

  ;; (projectile-indexing-method 'native)
  (projectile-sort-order 'access-time)
  (projectile-find-dir-includes-top-level t)
  :config (projectile-mode +1))

(use-package
  counsel-projectile                    ;projectile 使用 counsel前端
  :ensure t
  :after (counsel projectile)
  :custom                               ;
  (counsel-projectile-sort-files t)
  (counsel-projectile-sort-directories t)
  (counsel-projectile-sort-buffers t)
  (counsel-projectile-sort-projects t)
  :init (w/create-leader-key "p" 'counsel-projectile-switch-project "switch-project"
                             project-map-prefix)
  (w/create-leader-key "f" 'counsel-projectile-find-file "find-project-file" project-map-prefix)
  (w/create-leader-key "d" 'counsel-projectile-find-dir "find-project-dir" project-map-prefix)
  (w/create-leader-key "s" 'counsel-projectile-grep "search-in-project" project-map-prefix)
  (w/create-leader-key "S" 'counsel-projectile-git-grep "search-in-project-by-git"
                       project-map-prefix)
  (w/create-leader-key "c" 'projectile-kill-buffers "close-project" project-map-prefix)
  (w/create-leader-key "i" 'projectile-project-info "project-info" project-map-prefix)
  (w/create-leader-key "R" 'projectile-clear-known-projects "clear-project-cache"
                       project-map-prefix)
  :config (counsel-projectile-mode t))

(use-package
  neotree
  :ensure t
  :init                                 ;
  (defun neotree-project ()
    "Open NeoTree using the project root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (if project-dir (progn (neotree-dir project-dir)
                             (neotree-find file-name))
        (neotree-dir "~"))))
  (w/create-leader-key "f" 'neotree-project "file-tree" window-map-prefix))

(use-package
  multi-term
  :ensure t
  :defer t
  :custom ;
  (multi-term-dedicated-select-after-open-p t) ;打开后光标定位到 Terminal Window
  (multi-term-buffer-name "Terminal")
  :init (w/create-leader-key "t" 'multi-term-dedicated-toggle "toggle-terminal" window-map-prefix)
  :bind ("C-`" . multi-term-dedicated-toggle))

(use-package
  diff-hl
  :ensure t
  :defer t
  :config (global-diff-hl-mode))

;;;; ==============================================
;;;; 编辑增强
;;;; ==============================================


(use-package
  smart-comment                         ;注释插件
  :ensure t
  :bind ("C-/" . smart-comment)
  :init (w/create-leader-key "c" 'smart-comment "comment" content-map-prefix))

(use-package
  ace-jump-mode                         ; 根据字符在文档中跳转
  :ensure t
  :defer t
  :init (w/create-leader-key "c" 'ace-jump-char-mode "jump-to-char" goto-map-prefix)
  (w/create-leader-key "l" 'ace-jump-line-mode "jump-to-line" goto-map-prefix)
  (w/create-leader-key "w" 'ace-jump-word-mode "jump-to-word" goto-map-prefix))

(use-package
  hungry-delete                         ; 可以删除前面所有的空白字符
  :ensure t
  :defer t
  :hook (prog-mode . hungry-delete-mode))

(use-package
  beacon                                ; 跳转后,显示光标位置
  :if (display-graphic-p)
  :ensure t
  :config (beacon-mode t))

(use-package
  rainbow-delimiters                    ;彩虹括号
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package
  highlight-parentheses                 ;高亮当前括号
  :ensure t
  :defer t
  :custom (hl-paren-highlight-adjacent t)
  (hl-paren-colors '("red"))            ; 设置高亮括号颜色
  :hook (prog-mode . highlight-parentheses-mode))

(use-package
  highlight-indent-guides               ;高亮缩进
  :ensure t
  :defer t
  :init (setq highlight-indent-guides-method 'fill)
  (setq highlight-indent-guides-auto-odd-face-perc 15)
  (setq highlight-indent-guides-auto-even-face-perc 15)
  (setq highlight-indent-guides-auto-character-face-perc 20)
  (setq highlight-indent-guides-auto-enabled nil)
  :hook (prog-mode . highlight-indent-guides-mode))

(use-package
  undo-tree                             ;撤销重做可视化
  :ensure t
  :defer t
  :config (global-undo-tree-mode))


(use-package
  format-all                            ;格式化代码，支持多种格式
  :ensure t
  :defer t
  :init (w/create-leader-key "f" 'format-all-buffer "format" content-map-prefix))

(use-package
  auto-sudoedit                         ;自动请求sudo权限
  :if (or environment/linux
          environment/mac)
  :ensure t
  :defer 5
  :config (auto-sudoedit-mode 1))

(provide 'core/global)
