
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
  :config (evil-mode 1))
;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))
(use-package
  evil-leader
  :ensure t
  :after evil
  :config
  ;;
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
    ;;
    "w" #'evil-window-map)
  (global-evil-leader-mode))

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
  (evil-leader/set-key "SPC" #'counsel-M-x
    "f f" #'find-file
    "f r" #'ivy-recentf
    "b b" #'switch-to-buffer
    "b k" #'kill-buffer)
  :config
  ;; Remove M-x prefix
  (setq-default ivy-initial-inputs-alist nil)
  (ivy-mode t))

(use-package
  counsel
  :ensure t
  :defer t
  :bind
  ;;
  ("M-x" . counsel-M-x)
  ("C-x c-f" . counsel-find-file)
  ("C-h v" . counsel-describe-variable)
  ("C-h f" . counsel-describe-function)
  ("C-h k" . describe-key)
  :config (evil-leader/set-key
            ;;
            "? f" #'counsel-describe-function
            ;;
            "? v" #'counsel-describe-variable
            ;;
            "? k" #'describe-key
            ;;
            "? b" #'counsel-descbinds))

(use-package
  swiper
  :ensure t
  :defer t
  :bind ("C-s" . swiper))

(use-package ace-jump-mode
  :ensure t
  :defer t)
(use-package command-log-mode
  :ensure t
  :config
  (global-command-log-mode))

(use-package
  ace-jump-mode
  :ensure t
  :after evil-leader
  :defer t
  :init (evil-leader/set-key "j j" 'ace-jump-mode
            "j l" 'ace-jump-line-mode
            "j c" 'ace-jump-char-mode
            "j w" 'ace-jump-word-mode))

(use-package
  which-key
  :ensure t
  :init (setq which-key-idle-delay 0)
  (setq which-key-idle-secondary-delay 0)
  :config (which-key-mode t))


;; Better delete
(use-package
  hungry-delete
  :ensure t
  :defer t
  :hook (prog-mode . hungry-delete-mode))

;; Highlight indent
(use-package
  highlight-indent-guides
  :ensure t
  :defer t
  :init (setq highlight-indent-guides-method 'character)
  ;; (setq highlight-indent-guides-character ?\|)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-delay 0)
  (setq highlight-indent-guides-auto-character-face-perc 7)
  :hook (prog-mode . highlight-indent-guides-mode))
;; Auto complete parentheses
(use-package
  autopair
  :ensure t
  :defer t
  :hook (prog-mode . autopair-global-mode))
;; Rainbow parentheses
(use-package
  rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))
;; Highlight parentheses
(use-package
  highlight-parentheses
  :ensure t
  :defer t
  :hook (prog-mode . highlight-parentheses-mode))

(provide 'core/global)
