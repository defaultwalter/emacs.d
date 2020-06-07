;;;; ==============================================
;;;; 主题外观
;;;; ==============================================

(use-package
  doom-themes
  ;; :when (display-graphic-p)
  :ensure t
  :init (load-theme 'doom-one t)
  :config (set-face-attribute 'fringe nil
                              :foreground "#fc5c59"
                              :background (face-background 'default)))

(use-package
  doom-modeline
  :ensure t
  :init (doom-modeline-init)
  (setq doom-modeline-height 20 doom-modeline-bar-width 3 doom-modeline-icon nil
        doom-modeline-enable-word-count 10
        ;; doom-modeline-icon (display-graphic-p)
        doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode)
        doom-modeline-buffer-file-name-style 'relative-to-project doom-modeline-modal-icon nil)
  :hook (after-init . doom-modeline-mode)
  :config )

;;;; ==============================================
;;;; 交互增强
;;;; ==============================================

(use-package
  fcitx                                 ; 自动切换中英输入法
  :if (eq system-type 'gnu/linux)
  :ensure t
  :config (fcitx-default-setup))

(use-package
  ivy
  :ensure t
  :defer t
  :custom (ivy-use-virtual-buffers nil)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  :config
  ;; 移除M-x前缀
  (setq-default ivy-initial-inputs-alist nil)
  (ivy-mode t))

(use-package
  smex                                  ;将命令按使用频率排序
  :ensure t
  :defer t)
(use-package
  ivy-rich                              ; 在 M-x 和帮助中显示文档
  :ensure t
  :init
  :config (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package
  counsel                               ;基于ivy的命令文件补全工具
  :ensure t
  :defer t
  :init (w/bind-to-map file-map "f"
                       (lambda()
                         (interactive)
                         (let ((counsel-find-file-ignore-regexp "^\\."))
                           (counsel-find-file)))
                       "find-file")
  (w/bind-to-map file-map "a" counsel-find-file "find-all-file")
  (w/bind-to-map file-map "a" counsel-recentf "recent-file")
  (w/bind-to-map buffer-map "b"
                 (lambda()
                   (interactive)
                   (let ((ivy-ignore-buffers '("\\` " "\\`\\*")))
                     (counsel-switch-buffer)))
                 "switch-buffer")
  (w/bind-to-map buffer-map "a" counsel-switch-buffer "switch-all-buffer")
  :config (defalias 'command 'counsel-M-x)
  (evil-leader/set-key "SPC" 'command))

(use-package
  swiper                                ;基于ivy的增量搜索工具
  :ensure t
  :defer t
  :init (w/bind-to-map content-map "s" swiper "isearch")
  (w/bind-to-map content-map "S" swiper-all "search-in-buffers")
  :bind ("C-S-s" . swiper-all)
  ("C-s" . swiper))

(use-package
  command-log-mode                      ; 记录历史命令
  :ensure t
  :config (global-command-log-mode))

(use-package
  buffer-move                           ; 交换两个window的buffer
  :ensure t
  :defer t
  :init (w/bind-to-map buffer-map "h" buf-move-left "buffer-move-to-left-window")
  (w/bind-to-map buffer-map "j" buf-move-down "buffer-move-to-down-window")
  (w/bind-to-map buffer-map "k" buf-move-up "buffer-move-to-up-window")
  (w/bind-to-map buffer-map "l" buf-move-right "buffer-move-to-right-window")
  (setq buffer-move-stay-after-swap t)
  (setq buffer-move-behavior 'move))

(use-package
  windresize                            ;调整window大小
  :ensure t
  :defer t
  :init (w/bind-to-map window-map "r" windresize "resize-window"))


;; (use-package
;;   which-key
;;   :ensure t
;;   :defer nil
;;   :custom (which-key-enable-extended-define-key t)
;;   :init (setq which-key-idle-delay 0.5)
;;   (setq which-key-idle-secondary-delay 0)
;;   (setq which-key-sort-order 'which-key-key-order)
;;   (setq which-key-enable-extended-define-key t)
;;   :config (which-key-mode t)
;;   (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
;;   (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
;;   (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
;;   (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil))))

(use-package
  projectile                            ;project 插件
  :ensure t
  :init
  :config (projectile-mode +1)
  (counsel-projectile-mode t))

(use-package
  counsel-projectile                    ;projectile 使用 counsel前端
  :ensure t
  :after (counsel projectile)
  :init (w/bind-to-map project-map "p" counsel-projectile-switch-project "switch-project")
  (w/bind-to-map project-map "f" counsel-projectile-find-file "find-project-file")
  (w/bind-to-map project-map "d" counsel-projectile-find-dir "find-project-dir")
  (w/bind-to-map project-map "s" counsel-projectile-grep "search-in-project")
  (w/bind-to-map project-map "S" counsel-projectile-git-grep "search-in-project-by-git")
  (w/bind-to-map project-map "c" projectile-kill-buffers "close-project")
  (w/bind-to-map project-map "i" projectile-project-info "project-info")
  (w/bind-to-map project-map "R" projectile-clear-known-projects "clear-project-cache")
  :config (counsel-projectile-mode t))

(use-package
  magit                                 ;git 插件
  :ensure t
  :defer t
  :init (w/bind-to-map project-map "g" magit-status "git"))

(use-package
  evil-magit                            ;magit使用evil按键绑定
  :ensure t
  :init (setq evil-magit-state 'normal))

;;;; ==============================================
;;;; 编辑增强
;;;; ==============================================

(use-package
  evil
  :ensure t
  :custom (evil-want-minibuffer nil)
  :init (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;;
  (setq evil-emacs-state-cursor '("#ffb1ef" bar))
  (setq evil-normal-state-cursor '("#55b1ef" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("#c46bbc" bar))
  (setq evil-replace-state-cursor '("#c46bbc" hollow-rectangle))
  (setq evil-operator-state-cursor '("#c46bbc" hollow))
  (setq evil-want-C-i-jump nil)
  (when evil-want-C-i-jump (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward))
  :bind (:map evil-insert-state-map     ;
              ("M-h" . evil-backward-char)
              ("M-j" . evil-next-line)
              ("M-k" . evil-previous-line)
              ("M-l" . evil-forward-char)
              ("M-w" . evil-forward-word-begin)
              ("M-e" . evil-forward-word-end)
              ("M-b" . evil-backward-word-begin)
              ("M-B" . evil-backward-WORD-begin))
  :config (evil-mode t))
;; (use-package
;;   evil-collection
;;   :after evil
;;   :ensure t
;;   :config (evil-collection-init))
(use-package
  evil-surround
  :ensure t
  :config (global-evil-surround-mode 1))
(use-package
  evil-leader
  :ensure t
  :after evil
  :init (setq evil-leader/in-all-states t)
  :config
  ;;
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key "f" 'file-map)
  (evil-leader/set-key "b" 'buffer-map)
  (evil-leader/set-key "w" 'window-map)
  (evil-leader/set-key "c" 'content-map)
  (evil-leader/set-key "g" 'goto-map)
  (evil-leader/set-key "p" 'project-map)
  (evil-leader/set-key "m" 'major-map)
  (evil-leader/set-key "h" 'help-map)
  (global-evil-leader-mode t)
  (w/bind-to-map window-map "h" evil-window-left "focus-left-window")
  (w/bind-to-map window-map "j" evil-window-down "focus-down-window")
  (w/bind-to-map window-map "k" evil-window-up "focus-up-window")
  (w/bind-to-map window-map "l" evil-window-right "focus-right-window")
  ;; 这里有个BUG，需要重新开启 evil-mode 否则在 *Messages* buffer 中，evil-leader 无效。
  ;; https://github.com/cofi/evil-leader/issues/10
  (evil-mode t))

(use-package
  evil-terminal-cursor-changer
  :ensure t
  :unless (display-graphic-p)
  :after evil
  :config (evil-terminal-cursor-changer-activate))

(use-package
  smart-comment                         ;注释插件
  :ensure t
  :bind ("C-/" . smart-comment)
  :init (w/bind-to-map content-map "c" smart-comment "comment"))

(use-package
  ace-jump-mode                         ; 根据字符在文档中跳转
  :ensure t
  :defer t
  :init (w/bind-to-map goto-map "j" ace-jump-char-mode "jump-to-char")
  (w/bind-to-map goto-map "l" ace-jump-line-mode "jump-to-line")
  (w/bind-to-map goto-map "w" ace-jump-word-mode "jump-to-word"))


(use-package
  hungry-delete                         ; 可以删除前面所有的空白字符
  :ensure t
  :defer t
  :hook (prog-mode . hungry-delete-mode))

(use-package
  beacon                                ; 跳转后 显示光标位置
  :ensure t
  :config (beacon-mode t))

(use-package
  rainbow-delimiters                    ;彩虹括号
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight parentheses
(use-package
  highlight-parentheses                 ;高亮括号
  :ensure t
  :defer t
  :hook (prog-mode . highlight-parentheses-mode))
;; Highlight indent
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

(provide 'core/global)
