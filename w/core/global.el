(use-package
  evil
  :ensure t
  :custom (evil-want-minibuffer nil)
  :init
  ;;
  (setq evil-emacs-state-cursor '("#ffb1ef" bar))
  (setq evil-normal-state-cursor '("#55b1ef" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("#c46bbc" bar))
  (setq evil-replace-state-cursor '("#c46bbc" hollow-rectangle))
  (setq evil-operator-state-cursor '("#c46bbc" hollow))
  (bind-to-map window-map "h" 'evil-window-left "switch-to-left-window")
  (bind-to-map window-map "j" 'evil-window-down "switch-to-down-window")
  (bind-to-map window-map "k" 'evil-window-up "switch-to-up-window")
  (bind-to-map window-map "l" 'evil-window-right "switch-to-right-window")
  :config (evil-mode t))
;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))
(use-package
  evil-leader
  :ensure t
  :after evil
  :init (setq evil-leader/in-all-states t)
  :config
  ;;
  (evil-leader/set-key "a" 'application-map)
  (evil-leader/set-key "b" 'buffer-map)
  (evil-leader/set-key "c" 'comment-map)
  (evil-leader/set-key "d" 'debug-map)
  (evil-leader/set-key "e" 'error-map)
  (evil-leader/set-key "f" 'file-map)
  (evil-leader/set-key "j" 'jump-map)
  (evil-leader/set-key "h" 'help-map)
  (evil-leader/set-key "i" 'insert-map)
  (evil-leader/set-key "p" 'project-map)
  (evil-leader/set-key "s" 'search-map)
  (evil-leader/set-key "w" 'window-map)
  (evil-leader/set-leader "SPC")
  (global-evil-leader-mode t)
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
  smex
  :ensure t
  :defer t)

(use-package
  ivy
  :ensure t
  :defer t
  :custom (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  :init ;;
  :config
  ;; Remove M-x prefix
  (setq-default ivy-initial-inputs-alist nil)
  (ivy-mode t))
;; 在 M-x 和帮助中显示文档
(use-package
  ivy-rich
  :ensure t
  :init
  :config (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
(use-package
  counsel
  :ensure t
  :defer t
  :init (defalias 'command-list 'counsel-M-x)
  (evil-leader/set-key "SPC" #'command-list)
  (bind-to-map 'file-map "r" 'counsel-recentf)

  :bind
  :config )

(use-package
  swiper
  :ensure t
  :defer t
  :init (bind-to-map search-map "s" 'swiper "search")
  (bind-to-map search-map "a" 'swiper-all "search-all-buffers")
  :bind ("C-s" . swiper))

(use-package
  command-log-mode
  :ensure t
  :config (global-command-log-mode))
;; 交换Window buffer
(use-package
  buffer-move
  :ensure t
  :defer t
  :init (bind-to-map window-map "H" 'buf-move-left "window-move-left")
  (bind-to-map window-map "J" 'buf-move-down "window-move-down")
  (bind-to-map window-map "K" 'buf-move-up "window-move-up")
  (bind-to-map window-map "L" 'buf-move-right "window-move-right")
  (setq buffer-move-stay-after-swap t)
  (setq buffer-move-behavior 'move))
(use-package
  ace-jump-mode
  :ensure t
  :after evil-leader
  :defer t
  :init (bind-to-map jump-map "j" 'ace-jump-mode "jump-to")
  (bind-to-map jump-map "l" 'ace-jump-line-mode "jump-to-line")
  (bind-to-map jump-map "c" 'ace-jump-mode "jump-to-char")
  (bind-to-map jump-map "w" 'ace-jump-mode "jump-to-word"))

(use-package
  which-key
  :ensure t
  :init (setq which-key-idle-delay 0)
  (setq which-key-idle-secondary-delay 0)
  (setq which-key-sort-order 'which-key-description-order)
  :config (which-key-declare-prefixes "SPC SPC" "command")
  (which-key-declare-prefixes "SPC a" "application")
  (which-key-declare-prefixes "SPC b" "buffer")
  (which-key-declare-prefixes "SPC c" "comment")
  (which-key-declare-prefixes "SPC e" "error")
  (which-key-declare-prefixes "SPC d" "debug")
  (which-key-declare-prefixes "SPC f" "file")
  (which-key-declare-prefixes "SPC j" "jump")
  (which-key-declare-prefixes "SPC h" "help")
  (which-key-declare-prefixes "SPC i" "insert")
  (which-key-declare-prefixes "SPC m" "major mode")
  (which-key-declare-prefixes "SPC p" "project")
  (which-key-declare-prefixes "SPC s" "search")
  (which-key-declare-prefixes "SPC w" "window")
  (which-key-mode t))


;; Better delete
(use-package
  hungry-delete
  :ensure t
  :defer t
  :hook (prog-mode . hungry-delete-mode))

;; Auto complete parentheses
(use-package
  autopair
  :ensure t
  :defer t
  :hook (prog-mode . autopair-global-mode))

(use-package
  windresize
  :ensure t
  :defer t
  :init (bind-to-map window-map "r" 'windresize "resize-window"))

(provide 'core/global)
