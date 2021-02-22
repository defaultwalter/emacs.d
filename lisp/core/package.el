(use-package
  exec-path-from-shell
  ;; :if (memq window-system '(ns mac))
  :ensure t
  :custom (exec-path-from-shell-check-startup-files nil)
  :config (exec-path-from-shell-initialize))

(use-package
  xclip
  :ensure t
  :if (not (display-graphic-p))
  :config (xclip-mode 1))

;;;; ==============================================
;;;; 交互增强
;;;; ==============================================

(use-package
  which-key
  :ensure t
  :custom                               ;
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-allow-multiple-replacements t)
  :config                               ;
  (which-key-mode t)
  (add-to-list 'which-key-replacement-alist '(("ESC" . nil) . ("esc" . nil)))
  (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("tab" . nil)))
  (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("return" . nil)))
  (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("delete" . nil)))
  (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))
  (add-to-list 'which-key-replacement-alist '(("left" . nil) . ("left" . nil)))
  (add-to-list 'which-key-replacement-alist '(("right" . nil) . ("right" . nil)))
  (add-to-list 'which-key-replacement-alist '(("up" . nil) . ("up" . nil)))
  (add-to-list 'which-key-replacement-alist '(("down" . nil) . ("down" . nil))))


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
    (evil-visual-restore))
  (evil-define-key 'visual 'global ">" 'user/evil-shift-right-visual)
  (evil-define-key 'visual 'global "<" 'user/evil-shift-left-visual)
  (evil-define-key 'normal 'global (kbd "C-S-v") 'evil-visual-block)
  ;; (evil-mode 1)
  )

(use-package
  evil-collection
  :ensure t
  :after evil
  :custom                               ;
  (evil-collection-company-use-tng nil)
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

(defmacro user/leader-key (KEY &optional DEFAULTS)
  `(when (boundp 'evil-leader-mode)
     (let* ((DEFAULTS  (if (commandp ,DEFAULTS)
                           (list :command ,DEFAULTS)
                         (if (commandp (car ,DEFAULTS))
                             (cons :command ,DEFAULTS) ,DEFAULTS)))
            (ignore (plist-get DEFAULTS
                               :ignore))
            (name (plist-get DEFAULTS
                             :name))
            (mode (plist-get DEFAULTS
                             :mode))
            (command (plist-get DEFAULTS
                                :command)))
       (when name (if mode (which-key-add-major-mode-key-based-replacements mode (concat "SPC" " "
                                                                                         ,KEY) name)
                    (which-key-add-key-based-replacements (concat "SPC" " " ,KEY) name)))
       (when command (if mode (evil-leader/set-key-for-mode mode ,KEY command)
                       (evil-leader/set-key ,KEY command))))))
(use-package
  evil-leader
  :ensure t
  :after evil
  :config                               ;
  (evil-leader/set-leader "<SPC>")
  ;; (setq evil-leader/in-all-states t)
  (global-evil-leader-mode t)
  (evil-mode t) ; 为了让 scratch/message 等buffer 能够使用 evil-leader，必须在 evil-leader 后启用 evil
  (user/leader-key "f"
                   '(:ignore t
                             :name "file"))
  (user/leader-key "ff" '(find-file :name "find file"))
  (user/leader-key "fs" '(save-buffer :name "save file"))
  (user/leader-key "fS" '(save-some-buffers :name "save all files"))
  (user/leader-key "fr" '(recentf-open-files :name "recent file"))
  (user/leader-key "f." '((lambda()
                            (interactive)
                            (dired user-config-directory)) :name "open configuration"))
  (user/leader-key "b"
                   '(:ignore t
                             :name "buffer"))
  (user/leader-key "bb" '(switch-to-buffer :name "switch buffer"))
  (user/leader-key "bs" '(save-buffer :name "save buffer"))
  (user/leader-key "bS" '(save-some-buffers :name "save all buffers"))
  (user/leader-key "bk" '(kill-this-buffer :name "kill buffer"))
  (user/leader-key "bK" '(kill-buffer-and-window :name "kill buffer&window"))
  (user/leader-key "bc" '(kill-this-buffer :name "kill buffer"))
  (user/leader-key "bC" '(kill-buffer-and-window :name "kill buffer&window"))
  (user/leader-key "c"
                   '(:ignore t
                             :name "content"))
  (user/leader-key "cc" '(comment-line :name "comment"))
  (user/leader-key "cr" '(comment-or-uncomment-region :name "comment region"))
  (user/leader-key "w"
                   '(:ignore t
                             :name "window"))
  (user/leader-key "ws" '(split-window-horizontally :name "split window horizontally"))
  (user/leader-key "wv" '(split-window-vertically :name "split window vertically"))
  (user/leader-key "wm" '(maximize-window :name "maximize window"))
  (user/leader-key "wn" '(minimize-window :name "minimize window"))
  (user/leader-key "wb" '(balance-windows :name "balance window"))
  (user/leader-key "wd" '(delete-window :name "delete window"))
  (user/leader-key "wD" '(delete-other-windows :name "delete other window"))
  (user/leader-key "wc" '(delete-window :name "delete window"))
  (user/leader-key "wC" '(delete-other-windows :name "delete other window"))
  (user/leader-key "h"
                   '(:ignore t
                             :name "help"))
  ;; (user/leader-key "h <return>" '(view-order-manuals :name "manuals"))
  (user/leader-key "h RET" '(view-order-manuals :name ("return" . "manuals")))
  (user/leader-key "hf" '(describe-function :name "describe function"))
  (user/leader-key "hv" '(describe-variable :name "describe variable"))
  (user/leader-key "hk" '(describe-key :name "describe key"))
  (user/leader-key "hc" '(describe-char :name "describe char"))
  (user/leader-key "hm" '(describe-mode :name "describe mode"))
  (user/leader-key "hp" '(describe-package :name "describe package"))
  (user/leader-key "hs" '(describe-symbol :name "describe symbol"))
  (user/leader-key "hw" '(where-is :name "where is"))
  (user/leader-key "h?" '(about-emacs :name "about"))
  (user/leader-key "g"
                   '(:ignore t
                             :name "goto"))
  (user/leader-key "p"
                   '(:ignore t
                             :name "project"))
  (user/leader-key "n"
                   '(:ignore t
                             :name "note"))
  (user/leader-key "na" '(org-agenda :name "agenda"))
  (user/leader-key "m"
                   '(:ignore t
                             :name "mode")))

(use-package
  mini-frame                            ; 居中显示 minibuffer
  :ensure t
  :disabled
  :custom                               ;
  (mini-frame-show-parameters '((top . 0.15)
                                (width . 0.7)
                                (left . 0.5))))
(use-package
  minimap                               ; VSCode 类似的滚动条
  :ensure t
  :disabled
  :if (display-graphic-p)
  :custom (minimap-window-location 'right)
  :config (minimap-mode 1))

(use-package
  ivy
  :ensure t
  :defer t
  :custom (ivy-use-virtual-buffers nil)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  (ivy-use-selectable-prompt t)         ;允许选择输入提示行
  :config (ivy-mode t)
  (define-key ivy-minibuffer-map (kbd "S-RET") 'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "S-<return>") 'ivy-immediate-done))

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
  :init                                 ;
  (user/leader-key "RET" '(counsel-bookmark :name "bookmark"))
  (user/leader-key "ff" '((lambda()
                            (interactive)
                            (let ((counsel-find-file-ignore-regexp "^\\."))
                              (counsel-find-file))) :name "find file"))
  (user/leader-key "fF" '(counsel-find-file :name "find all file"))
  (user/leader-key "fr" '(counsel-recentf :name "recent file"))
  (user/leader-key "bb" '((lambda()
                            (interactive)
                            (let ((ivy-ignore-buffers '("\\` " "\\`\\*")))
                              (counsel-switch-buffer))) :name "switch buffer"))
  (user/leader-key "bB" '(counsel-switch-buffer :name "switch all buffer"))
  (user/leader-key "SPC" '(counsel-M-x :name "command"))
  :bind (("M-x" . counsel-M-x))
  :config)

(use-package
  flx ;; Improves sorting for fuzzy-matched results
  :after ivy
  :defer t
  :init (setq ivy-flx-limit 10000))

(use-package
  prescient
  :ensure t
  :after counsel
  :config (prescient-persist-mode 1))

(use-package
  ivy-prescient
  :ensure t
  :after prescient
  :config (ivy-prescient-mode 1))

(use-package
  swiper                                ;基于ivy的增量搜索工具
  :ensure t
  :defer t
  :init                                 ;
  (user/leader-key "cs" '(swiper :name "swipe"))
  (user/leader-key "cS" '(swiper-all :name "swipe in all buffers"))
  :bind                                 ;
  ("C-S-s" . swiper-all)
  ("C-s" . swiper))

(use-package
  command-log-mode                      ; 记录历史命令
  :ensure t
  :defer t
  :config (global-command-log-mode))

;; 自动完成
(use-package
  company
  :ensure t
  :defer t
  :hook ;; (prog-mode . company-mode)
  (after-init . global-company-mode)
  :init ;; Don't convert to downcase.
  (setq-default company-dabbrev-downcase nil)
  :bind (;; ("C-TAB" . company-complete-common)
         ("C-SPC" . company-complete-common)
         ;;
         :map company-active-map        ;
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-s" . company-filter-candidates)
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)
         ("<return>" . company-complete-selection) ; 终端下无效
         ("RET" . company-complete-selection)      ; 终端下生效
         :map company-search-map                   ;
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)
         ("<return>" . company-complete-selection) ; 终端下无效
         ("RET" . company-complete-selection))     ; 终端下生效
  :custom                                          ;
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.5)
  (company-echo-delay 0.2)
  (company-show-numbers t)
  :config                               ;
  (setq company-selection-default 0)
  (setq company-backends '(;; (:separate company-yasnippet
                           ;;            company-capf)
                           (company-capf company-dabbrev-code company-keywords company-files)
                           (company-dabbrev)))
  (setq company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)))

(use-package
  company-box
  :ensure t
  :requires company
  :hook (company-mode . company-box-mode)
  :init                                 ;
  (setq company-box-show-single-candidate t)
  :config)

(use-package
  company-prescient
  :ensure t
  :after company
  :hook (company-mode . company-prescient-mode))

(use-package
  undo-tree                             ;撤销重做可视化
  :ensure t
  :config (global-undo-tree-mode))

(use-package
  buffer-move                           ; 交换两个window的buffer
  :ensure t
  :defer t
  :init                                 ;
  (user/leader-key "b C-h" '(buf-move-left :name "move to left window"))
  (user/leader-key "b C-j" '(buf-move-down :name "move to down window"))
  (user/leader-key "b C-k" '(buf-move-up :name "move to up window"))
  (user/leader-key "b C-l" '(buf-move-right :name "move to right window"))
  (setq buffer-move-stay-after-swap t)
  (setq buffer-move-behavior 'move))

(use-package
  windresize                            ;调整window大小
  :ensure t
  :defer t
  :init                                 ;
  (user/leader-key "wr" '(windresize :name "resize window")))

(use-package
  ace-window                            ; 窗口跳转
  :ensure t
  :defer t
  :init                                 ;
  (user/leader-key "ww" '(ace-window :name "select window"))
  :config (setq aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f ?g)))

(use-package
  transpose-frame
  :ensure t
  :defer t
  :init (user/leader-key "wt" '(transpose-frame :name "transpose")))

(use-package
  projectile                            ;project 插件
  :ensure t
  :custom       ;
  ;; (projectile-track-known-projects-automatically nil)
  ;; (projectile-indexing-method 'native)
  (projectile-sort-order 'access-time)
  (projectile-find-dir-includes-top-level t)
  :init                                 ;
  (user/leader-key "pk" '(project-kill-buffers :name "close all project buffers"))
  (user/leader-key "pi" '(projectile-project-info :name "project info"))
  (user/leader-key "pd" '(projectile-remove-known-project :name "remove project"))
  :config (projectile-mode +1))

(use-package
  counsel-projectile                    ;projectile 使用 counsel前端
  :ensure t
  :custom                               ;
  (counsel-projectile-sort-files t)
  (counsel-projectile-sort-directories t)
  (counsel-projectile-sort-buffers t)
  (counsel-projectile-sort-projects t)
  :init (user/leader-key "pp" '(counsel-projectile-switch-project :name "switch project"))
  (user/leader-key "pf" '(counsel-projectile-find-file :name "find file in project"))
  (user/leader-key "ps" '(counsel-projectile-git-grep :name "search in project by git"))
  (user/leader-key "pS" '(counsel-projectile-grep :name "search in project"))
  :config (counsel-projectile-mode t))

(use-package
  magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :init                                 ;
  (user/leader-key "pg" '(magit-status :name "git")))
(use-package
  neotree
  :ensure t
  :defer t
  :custom                               ;
  (neo-smart-open t)
  (neo-autorefresh t)
  (neo-window-width 35)
  (neo-mode-line-type 'none)
  ;; (neo-vc-integration 'face); 和doom主题冲突
  (neo-hide-cursor t)
  :bind                                 ;
  ("C-<tab>" . neotree-toggle)
  ("C-TAB" . neotree-toggle)
  :init                                 ;
  (user/leader-key "fv" '(neotree-toggle :name "file view")))


(use-package
  disable-mouse
  :ensure t
  ;; :disabled
  :config (when (boundp 'evil-mode)
            (mapc #'disable-mouse-in-keymap (list evil-motion-state-map evil-normal-state-map
                                                  evil-visual-state-map evil-insert-state-map)))
  (global-disable-mouse-mode))


(use-package
  diff-hl
  :ensure t
  :defer 1
  :config                               ;
  (global-diff-hl-mode))

;;;; ==============================================
;;;; 编辑增强
;;;; ==============================================

(use-package
  smartparens
  :ensure t
  :config                               ;
  (require 'smartparens-config)
  (smartparens-global-strict-mode))

(use-package
  smart-comment                         ;注释插件
  :ensure t
  :defer t
  :bind ("C-/" . smart-comment)
  :init (user/leader-key "cc" '(smart-comment :name "comment")))

(use-package
  avy
  :ensure t
  :defer t
  :init                                 ;
  (with-eval-after-load "evil" (evil-define-key 'normal 'global "gc" 'avy-goto-char)))

(use-package
  hungry-delete                         ; 可以删除前面所有的空白字符
  :ensure t
  :defer t
  :custom (hungry-delete-join-reluctantly t)
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
  :disabled
  :defer t
  :custom (hl-paren-highlight-adjacent t)
  (hl-paren-colors '("cyan"))           ; 设置高亮括号颜色
  :hook (prog-mode . highlight-parentheses-mode))


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
  format-all                            ;格式化代码，支持多种格式
  :ensure t
  :defer t
  :init (user/leader-key "cf" '(format-all-buffer :name "format")))

(use-package
  auto-sudoedit                         ;自动请求sudo权限
  :if (or (eq system-type 'gnu/linux)
          (eq system-type 'darwin))
  :ensure t
  :config (auto-sudoedit-mode 1))

(use-package
  memory-usage
  :ensure t
  :defer t)
(use-package
  popwin                                ; 使用弹出窗口显示部分Buffer
  :ensure t
  :config                               ;
  (popwin-mode 1))

(use-package
  evil-multiedit                        ; 多光标
  :ensure t
  :bind (:map evil-normal-state-map
              ("C-M-*" . evil-multiedit-match-all)
              ("C-M-]" . evil-multiedit-match-and-next)
              ("C-M-[" . evil-multiedit-match-and-prev)
              :map evil-visual-state-map
              ("M-*" . evil-multiedit-match-all)
              ("C-M-]" . evil-multiedit-match-and-next)
              ("C-M-[" . evil-multiedit-match-and-prev)
              :map  evil-multiedit-state-map
              ("RET" . evil-multiedit-toggle-or-restrict-region)
              ("<return>" . evil-multiedit-toggle-or-restrict-region)
              :map evil-multiedit-state-map
              ("C-n" . evil-multiedit-next)
              ("C-p" . evil-multiedit-prev)
              :map evil-multiedit-insert-state-map
              ("C-n" . evil-multiedit-next)
              ("C-p" . evil-multiedit-prev)))

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
  sis                                   ; 自动切换输入法
  :ensure t
  :config                               ;
  (cond ((eq system-type 'darwin)
         (if (executable-find "macism")
             (sis-ism-lazyman-config "com.apple.keylayout.ABC" "com.apple.inputmethod.SCIM.ITABC")
           (message
            "SIS need to install macism. use ‘brew tap laishulu/macism;brew install macism’ to install it.")))
        ((eq system-type 'gnu/linux)
         (sis-ism-lazyman-config "1" "2" 'fcitx)))
  (sis-global-respect-mode t))

;;;; ==============================================
;;;; 主题外观
;;;; ==============================================

(use-package
  doom-modeline
  :ensure t
  :defer t
  :init (doom-modeline-init)
  (setq doom-modeline-height 20)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-enable-word-count t) ;字数统计
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (setq doom-modeline-buffer-file-name-style 'auto)
  ;; (setq doom-modeline-minor-modes t)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-modal-icon t)
  (doom-modeline-mode 1))

(use-package
  doom-themes
  :ensure t
  :init
  :custom                               ;
  (doom-themes-neotree-file-icons t)
  :custom-face                          ;
  (font-lock-comment-face ((t
                            (:slant italic))))
  (neo-root-dir-face ((t
                       (:extend t))))
  :hook (server-after-make-frame . (lambda()
                                     (load-theme 'doom-one t)))
  :config                               ;
  (load-theme 'doom-one t)
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ;; (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package
  dashboard
  :ensure t
  :config                               ;
  (user/leader-key "b d" '(dashboard-refresh-buffer :name "dashboard"))
  (setq dashboard-startup-banner (expand-file-name "dashboard-banner.txt" user-config-directory))
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '())
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons `(   ;
                                      () ;
                                      () ;
                                      () ;
                                      ((,(all-the-icons-octicon "calendar"
                                                                :height 0.9
                                                                :v-adjust 0.0)
                                        " Open agenda                         " "" (lambda
                                                                                     (&rest
                                                                                      _)
                                                                                     (call-interactively
                                                                                      'org-agenda))
                                        nil "" "")
                                       ("" "SPC n a   " "" (lambda
                                                             (&rest
                                                              _)
                                                             (call-interactively 'org-agenda))
                                        default "" ""))
                                      () ;
                                      ((,(all-the-icons-octicon "file-text"
                                                                :height 0.9
                                                                :v-adjust 0.0)
                                        " Open recently file                  " "" (lambda
                                                                                     (&rest
                                                                                      _)
                                                                                     (call-interactively
                                                                                      'counsel-recentf))
                                        nil "" "")
                                       ("" "SPC f r   " "" (lambda
                                                             (&rest
                                                              _)
                                                             (call-interactively 'counsel-recentf))
                                        default "" ""))
                                      () ;
                                      ((,(all-the-icons-octicon "briefcase"
                                                                :height 0.9
                                                                :v-adjust 0.0)
                                        " Open project                        " "" (lambda
                                                                                     (&rest
                                                                                      _)
                                                                                     (call-interactively
                                                                                      'counsel-projectile))
                                        nil "" "")
                                       ("" "SPC p p   " "" (lambda
                                                             (&rest
                                                              _)
                                                             (call-interactively
                                                              'counsel-projectile)) default "" ""))
                                      () ;
                                      ((,(all-the-icons-octicon "star"
                                                                :height 0.9
                                                                :v-adjust 0.0)
                                        " Jump to bookmark                    " "" (lambda
                                                                                     (&rest
                                                                                      _)
                                                                                     (call-interactively
                                                                                      'counsel-bookmark))
                                        nil "" "")
                                       ("" "SPC return" "" (lambda
                                                             (&rest
                                                              _)
                                                             (call-interactively 'counsel-bookmark))
                                        default "" ""))
                                      ()   ;
                                      ()   ;
                                      ())) ;
  ;;
  (setq dashboard-page-separator "")
  (setq dashboard-set-footer nil)
  (setq dashboard-items-default-length 20)
  ;; C/S mode use dashboard as default buffer
  (dashboard-setup-startup-hook))

(use-package
  solaire-mode
  :ensure t
  :if (display-graphic-p)
  :hook                                 ;
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config                               ;
  (set-face-background 'solaire-mode-line-face nil)
  (set-face-background 'solaire-mode-line-inactive-face nil)
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))


(use-package
  all-the-icons
  :ensure t
  :init (setq all-the-icons-scale-factor 0.9))


(use-package
  visual-fill-column                    ;设置正文宽度
  :ensure t
  :defer t
  :commands (visual-fill-column-mode)
  :config                               ;
  (setq-default visual-fill-column-width 100)
  (setq-default visual-fill-column-center-text t))

(provide 'core/package)
