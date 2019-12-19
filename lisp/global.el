;; ==============================================
;; Theme
;; ==============================================
(use-package
  doom-themes
  :when (display-graphic-p)
  :ensure t
  :init (load-theme 'doom-one t)
  :config (defun on-frame-open (frame)
            (if (not (display-graphic-p frame))
                (set-face-background 'default "unspecified-bg" frame)))
  (on-frame-open (selected-frame))
  (add-hook 'after-make-frame-functions 'on-frame-open))
;; Modeline theme
(use-package
  doom-modeline
  :ensure t
  :init (doom-modeline-init)
  (setq doom-modeline-height 25 doom-modeline-bar-width 3 doom-modeline-icon nil
        doom-modeline-enable-word-count 10 doom-modeline-icon (display-graphic-p)
        doom-modeline-buffer-file-name-style 'file-name)
  :hook (after-init . doom-modeline-mode)
  :config (set-face-attribute 'mode-line nil
                              :background nil)
  (set-face-attribute 'mode-line-inactive nil
                      :background nil))

;; ==============================================
;; Evil mode
;; ==============================================
(use-package
  evil
  :ensure t
  :custom (evil-want-minibuffer t)
  :init
  ;;
  (setq evil-emacs-state-cursor '("#ffb1ef" bar))
  (setq evil-normal-state-cursor '("#55b1ef" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("#c46bbc" bar))
  (setq evil-replace-state-cursor '("#c46bbc" hollow-rectangle))
  (setq evil-operator-state-cursor '("#c46bbc" hollow))
  :config (evil-mode 1))
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


;; ==============================================
;; Ivy Counsel Swiper
;; ==============================================
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
  (evil-leader/set-key "SPC" #'counsel-M-x "f f" #'find-file "f r" #'ivy-recentf "b b"
    #'switch-to-buffer "b k" #'kill-buffer)
  :config
  (ivy-mode t))

(use-package
  counsel
  :ensure t
  :defer t
  :bind
  ;;
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
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


;; Project
;; ==============================================
(use-package
  projectile
  :ensure t
  :defer nil
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (evil-leader/set-key "p" 'projectile-command-map)
  :config
  ;;
  (projectile-mode +1)
  (counsel-projectile-mode t))

(use-package
  counsel-projectile
  :ensure t
  :after (counsel projectile)
  :config (counsel-projectile-mode t))

(use-package
  magit
  :ensure t
  :defer t
  :init (evil-leader/set-key "g" 'magit-status))


;; ==============================================
;; Dired file manager
;; ==============================================
(use-package
  dired
  :ensure nil
  :init
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; Auto refresh Dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  ;; Quickly copy/move file in Dired
  (setq dired-dwim-target t)

  ;; Move files to trash when deleting
  (setq delete-by-moving-to-trash t)
  :config
  ;; Reuse same dired buffer, so doesn't create new buffer each time
  (put 'dired-find-alternate-file 'disabled nil)
  (add-hook 'dired-mode-hook
            (lambda ()
              (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)))
  (add-hook 'dired-mode-hook
            (lambda ()
              (local-set-key (kbd "RET") #'dired-find-alternate-file)))
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "^")
                (lambda ()
                  (interactive)
                  (find-alternate-file "..")))
              (dired-hide-details-mode t))))





;; ==============================================
;; Default Enhancement
;; ==============================================

(use-package
  windresize
  :ensure t
  :defer t)

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
  :init (setq which-key-idle-delay 1.0)
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

(use-package
  neotree
  :ensure t
  :defer t
  :init (setq neo-create-file-auto-open nil neo-auto-indent-point nil neo-autorefresh nil
              neo-mode-line-type 'none neo-window-width 24 neo-show-updir-line nil neo-theme 'nerd
                                        ; fallback
              neo-banner-message nil neo-confirm-create-file #'off-p neo-confirm-create-directory
              #'off-p neo-show-hidden-files nil neo-keymap-style 'concise neo-show-hidden-files t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :config (add-hook 'neotree-mode-hook
                    (lambda ()
                      (define-key evil-normal-state-local-map (kbd "j") 'neotree-next-line)
                      (define-key evil-normal-state-local-map (kbd "k") 'neotree-previous-line)
                      (define-key evil-normal-state-local-map (kbd "h") 'neotree-select-up-node)
                      (define-key evil-normal-state-local-map (kbd "l") 'neotree-quick-look)
                      (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                      (define-key evil-normal-state-local-map (kbd "r") 'neotree-rename-node)
                      (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)
                      (define-key evil-normal-state-local-map (kbd "c") 'neotree-copy-node)
                      (define-key evil-normal-state-local-map (kbd "y")
                        'neotree-copy-filepath-to-yank-ring)
                      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                      (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-change-root)
                      (define-key evil-normal-state-local-map (kbd "DEL")
                        'neotree-hidden-file-toggle))))
(provide 'global)
