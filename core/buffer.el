;; Theme
(use-package 
  doom-themes 
  :ensure t 
  :config (load-theme 'doom-one t))

(use-package 
  expand-region 
  :ensure t 
  :after evil-leader 
  :config (evil-leader/set-key "m m" 'er/expand-region) 
  (evil-leader/set-key "m f" 'er/mark-defun) 
  (evil-leader/set-key "m p" 'mark-page) 
  (evil-leader/set-key "m i p" 'er/mark-inside-pairs) 
  (evil-leader/set-key "m o p" 'er/mark-outside-pairs) 
  (evil-leader/set-key "m i q" 'er/mark-inside-quotes) 
  (evil-leader/set-key "m o q" 'er/mark-outside-quotes))

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))
(use-package 
  autopair 
  :ensure t 
  :config (autopair-global-mode))

;; Modeline
;; (use-package 
;;   spaceline-config 
;;   :ensure spaceline 
;;   :config (if window-system 
;;               (setq powerline-default-separator 'wave) 
;;             (setq powerline-default-separator 'utf-8)) 
;;   (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state) 
;;   ;; (spaceline-toggle-minor-modes-off)
;;   (spaceline-spacemacs-theme))
(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode)
      :config (setq doom-modeline-height 25
               doom-modeline-icon nil))
;; Auto complete
(use-package 
  company 
  :defer 1 
  :ensure t 
  :init
  ;; Don't convert to downcase.
  (setq-default company-dabbrev-downcase nil) 
  :config (global-company-mode t) 
  :bind (("C-SPC" . company-complete) :map company-active-map ("C-n" . company-select-next) 
         ("TAB" . nil) 
         ("C-p" . company-select-previous)))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config (require 'lsp-clients)
  (setq lsp-print-io t))
(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))
(use-package company-lsp
  :ensure t
  :after (company lsp-mode)
  :commands company-lsp)
(use-package 
  highlight-parentheses 
  :ensure t 
  :hook (prog-mode . highlight-parentheses-mode))
(use-package 
  rainbow-delimiters 
  :ensure t 
  :hook (prog-mode . rainbow-delimiters-mode))

;; Better delete
(use-package 
  hungry-delete 
  :ensure t 
  :defer 1 
  :config (global-hungry-delete-mode))
;; Ace jump
(use-package 
  ace-jump-mode 
  :ensure t 
  :after evil-leader 
  :config (evil-leader/set-key "j" 'ace-jump-mode))
(use-package 
  which-key 
  :ensure t 
  :defer 1 
  :config (setq which-key-idle-delay 0.01) 
  (setq which-key-idle-secondary-delay 0) 
  (which-key-mode t))

(use-package 
  projectile 
  :ensure t 
  :after evil-leader 
  :config (projectile-mode) 
  (evil-leader/set-key "p" 'projectile-command-map) 
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package 
  smex 
  :ensure t)
(use-package 
  ivy 
  :ensure t 
  :config (setq ivy-use-virtual-buffers t) 
  (setq ivy-count-format "(%d/%d) ") 
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode t))
(use-package 
  counsel 
  :ensure t 
  :bind ("M-x" . counsel-M-x) 
  ("C-x C-f" . counsel-find-file) 
  ("C-h v" . counsel-describe-variable) 
  ("C-h f" . counsel-describe-function))
(use-package 
  counsel-projectile 
  :ensure t 
  :after projectile 
  :config (counsel-projectile-mode t))
(use-package 
  swiper 
  :ensure t 
  :bind ("C-s" . swiper) 
  :config (evil-leader/set-key "s" 'swiper))
;; (use-package 
;;   dashboard 
;;   :ensure t 
;;   :config (setq dashboard-items '((recents  . 5) 
;;                                   (projects . 5) 
;;                                   (agenda . 5) 
;;                                   (bookmarks . 5) 
;;                                   (registers . 5))) 
;;   (setq dashboard-startup-banner "~/.emacs.d/logo.png") 
;;   (dashboard-setup-startup-hook))
(use-package 
  flycheck 
  :ensure t 
  :init (global-flycheck-mode))
;; macOS 
(use-package
  exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config (exec-path-from-shell-initialize))

(provide 'buffer)

;;; modeline.el ends here
