(use-package
  projectile
  :ensure t
  :init
  :config
  ;;
  (projectile-mode +1)
  (counsel-projectile-mode t))

(use-package
  counsel-projectile
  :ensure t
  :after (counsel projectile)
  :init
  (w/leader-set-key "f" "open-project" 'counsel-projectile-find-file @project-map)
  (w/leader-set-key "p" "switch-project" 'counsel-projectile-switch-project @project-map)
  (w/leader-set-key "d" "project-files" 'projectile-dired @project-map)
  (w/leader-set-key "D" "project-files-other-window" 'projectile-dired-other-window @project-map)
  (w/leader-set-key "C-s" "search-in-project" 'counsel-projectile-git-grep @project-map)
  (w/leader-set-key "s" "search-in-project" 'counsel-projectile-grep @project-map)
  (w/leader-set-key "c" "close-project" 'projectile-kill-buffers @project-map)
  (w/leader-set-key "S" "open-shell-in-project" 'projectile-run-shell @project-map)
  (w/leader-set-key "i" "project-info" 'projectile-project-info @project-map)
  (w/leader-set-key "r" "clear-project-cache" 'projectile-clear-known-projects @project-map)
  :config (counsel-projectile-mode t))

(use-package
  magit
  :ensure t
  :defer t
  :init
  (w/leader-set-key "g" "git" 'magit-status @view-map))
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
