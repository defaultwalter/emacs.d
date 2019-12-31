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

(provide 'core/project)