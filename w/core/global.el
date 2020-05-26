(use-package
  fcitx
  :ensure t
  :config (fcitx-default-setup))

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
  :init (w/leader-set-key "SPC" "X" 'counsel-M-x)
  (w/leader-set-key "r" "recent-file" 'counsel-recentf @file-map)
  :config )

(use-package
  swiper
  :ensure t
  :defer t
  :init (w/leader-set-key "s" "search" 'swiper @search-map)
  (w/leader-set-key "S" "search-all-buffers" 'swiper-all @search-map)
  :bind ("C-S-s" . swiper-all)
  ("C-s" . swiper))

(use-package
  smart-comment
  :ensure t
  :bind ("C-/" . smart-comment)
  :init (w/leader-set-key "c" "comment" 'smart-comment @content-map))

(use-package
  command-log-mode
  :ensure t
  :config (global-command-log-mode))
;; 交换Window buffer
(use-package
  buffer-move
  :ensure t
  :defer t
  :init (w/leader-set-key "H" "window-move-left" 'buf-move-left @buffer-map)
  (w/leader-set-key "J" "window-move-down" 'buf-move-down @buffer-map)
  (w/leader-set-key "K" "window-move-up" 'buf-move-up @buffer-map)
  (w/leader-set-key "L" "window-move-right" 'buf-move-right @buffer-map)
  (setq buffer-move-stay-after-swap t)
  (setq buffer-move-behavior 'move))
(use-package
  ace-jump-mode
  :ensure t
  :after evil-leader
  :defer t
  :init (w/leader-set-key "j" "jump-to-char" 'ace-jump-mode @jump-map)
  (w/leader-set-key "l" "jump-to-line" 'ace-jump-line-mode @jump-map)
  (w/leader-set-key "w" "jump-to-word" 'ace-jump-word-mode @jump-map))


;; Better delete
(use-package
  hungry-delete
  :ensure t
  :defer t
  :hook (prog-mode . hungry-delete-mode))


(use-package
  windresize
  :ensure t
  :defer t
  :init (w/leader-set-key "r" "resize-window" 'windresize @window-map))

(provide 'core/global)
