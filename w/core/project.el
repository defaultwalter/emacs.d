(use-package
  projectile
  :ensure t
  :defer t
  :init
  ;; (defalias 'project 'projectile-command-map)
  ;; (evil-leader/set-key "p" 'project)
  :config
  ;;
  (projectile-mode +1)
  (counsel-projectile-mode t))

(use-package
  counsel-projectile
  :ensure t
  :after (counsel projectile)
  :init
  (bind-to-map project-map "p" 'counsel-projectile "open-porject")
  :config (counsel-projectile-mode t))

(use-package
  magit
  :ensure t
  :defer t
  :init (bind-to-map project-map "g" 'magit-status "git"))
(use-package
  evil-magit
  :ensure t
  :init (setq evil-magit-state 'normal))
(use-package
  treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package
  treemacs-magit
  :after (treemacs magit)
  :ensure t)

(provide 'core/project)
