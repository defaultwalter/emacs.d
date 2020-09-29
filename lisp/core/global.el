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
  (setq doom-modeline-enable-word-count t) ;字数统计
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (setq doom-modeline-buffer-file-name-style 'auto)
  ;; (setq doom-modeline-minor-modes t)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-color-icon nil)
  (setq doom-modeline-modal-icon t)
  (doom-modeline-mode 1)
  ;; :hook (after-init . doom-modeline-mode)
  :config )
(use-package
  doom-themes
  ;; :when (display-graphic-p)
  :ensure t
  :init
  :config                               ;
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (set-face-attribute 'fringe nil
                      :foreground "#fc5c59")
  ;; 设置垂直窗口边框(目前发现只在终端有效)
  (set-face-inverse-video-p 'vertical-border nil)
  (set-face-background 'vertical-border (face-background 'default)))

;; (use-package
;;   minimal-theme
;;   ;; :when (display-graphic-p)
;;   :ensure t
;;   :config (doom-themes-treemacs-config)
;;   (load-theme 'minimal-light t))

(use-package
  solaire-mode
  :ensure t
  :hook                                 ;
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config                               ;
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))
(use-package
  all-the-icons
  :ensure t
  :init (setq all-the-icons-scale-factor 0.9))

;;;; ==============================================
;;;; 交互增强
;;;; ==============================================

;; (use-package
;;   disable-mouse
;;   :ensure t
;;   :config                               ;
;;   (global-disable-mouse-mode))

(use-package
  ivy
  :ensure t
  :defer t
  :custom (ivy-use-virtual-buffers nil)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  :config (ivy-mode t))

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
  transpose-frame
  :ensure t
  :defer t
  :init (w/create-leader-key "t" 'transpose-frame "transpose-window" window-map-prefix))

(use-package
  projectile                            ;project 插件
  :ensure t
  :custom                               ;
  (projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" misc-file-directory))
  (projectile-track-known-projects-automatically nil)
  ;; (projectile-indexing-method 'native)
  (projectile-sort-order 'access-time)
  (projectile-find-dir-includes-top-level t)
  :init (w/create-leader-key "c" 'projectile-kill-buffers "close-project" project-map-prefix)
  (w/create-leader-key "i" 'projectile-project-info "project-info" project-map-prefix)
  (w/create-leader-key "R" 'projectile-clear-known-projects "clear-project-cache"
                       project-map-prefix)
  :config (projectile-mode +1))

(use-package
  counsel-projectile                    ;projectile 使用 counsel前端
  :ensure t
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
  :config (counsel-projectile-mode t))


(use-package
  treemacs
  :ensure t
  :init (w/create-leader-key "f" 'treemacs "file-tree" view-map-prefix))
(use-package
  treemacs-evil
  :ensure t
  :after (evil treemacs))
(use-package
  treemacs-projectile
  :after(projectile treemacs)
  :ensure t)
;; (use-package
;;   multi-term
;;   :ensure t
;;   :defer t
;;   :custom                                      ;
;;   (multi-term-dedicated-select-after-open-p t) ;打开后光标定位到 Terminal Window
;;   (multi-term-buffer-name "Terminal")
;;   :init (w/create-leader-key "t" 'multi-term-dedicated-toggle "toggle-terminal" window-map-prefix)
;;   :bind ("C-`" . multi-term-dedicated-toggle))


(use-package
  multi-vterm
  :ensure t
  :init (defun toggle-termianl()
          (interactive)
          (let ((project-dir (projectile-project-root)))
            (if project-dir (multi-vterm-projectile)
              (multi-vterm-dedicated-toggle))))
  (w/create-leader-key "t" 'toggle-termianl "toggle-terminal" view-map-prefix)
  :bind ("C-`" . toggle-termianl)
  :config (add-hook 'vterm-mode-hook #'evil-insert-state)
  (define-key vterm-mode-map [return]                      #'vterm-send-return)
  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

(use-package
  diff-hl
  :ensure t
  :config                               ;
  (global-diff-hl-mode))

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
  :config (auto-sudoedit-mode 1))

(use-package
  memory-usage
  :ensure t
  :defer t)
(use-package
  rainbow-mode
  :ensure t
  :config
  ;; 默认的会文本属性背景色显示颜色，会与高亮行插件冲突，通过重写这个方法，调换前景与背景色来解决这个问题
  (defun rainbow-colorize-match (color &optional match)
    "Return a matched string propertized with a face whose
background is COLOR. The foreground is computed using
`rainbow-color-luminance', and is either white or black."
    (let ((match (or match
                     0)))
      (put-text-property (match-beginning match)
                         (match-end match) 'face
                         `((:background ,(if (> 0.5 (rainbow-x-color-luminance color))
                                             (face-foreground 'default)
                                           (face-background 'default)))
                           (:foreground ,color))))))

(use-package
  benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(provide 'core/global)
