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
              (local-set-key (kbd "RET") #'dired-find-alternate-file)
              (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file))))



(use-package
  treemacs
  :ensure t
  :defer t
  :init
  (w/leader-set-key "f" "file-explorer" 'treemacs @view-map)
  :config (progn
            (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
                  treemacs-deferred-git-apply-delay      0.5 treemacs-directory-name-transformer
                  #'identity treemacs-display-in-side-window        t treemacs-eldoc-display t
                  treemacs-file-event-delay              5000 treemacs-file-extension-regex
                  treemacs-last-period-regex-value treemacs-file-follow-delay             0.2
                  treemacs-file-name-transformer         #'identity treemacs-follow-after-init t
                  treemacs-git-command-pipe              "" treemacs-goto-tag-strategy
                  'refetch-index treemacs-indentation                   2
                  treemacs-indentation-string            " " treemacs-is-never-other-window nil
                  treemacs-max-git-entries               5000 treemacs-missing-project-action 'ask
                  treemacs-no-png-images                 nil treemacs-no-delete-other-windows t
                  treemacs-project-follow-cleanup        nil treemacs-persist-file (expand-file-name
                                                                                    ".cache/treemacs-persist"
                                                                                    user-emacs-directory)
                  treemacs-position                      'left treemacs-recenter-distance 0.1
                  treemacs-recenter-after-file-follow    nil treemacs-recenter-after-tag-follow nil
                  treemacs-recenter-after-project-jump   'always
                  treemacs-recenter-after-project-expand 'on-distance treemacs-show-cursor nil
                  treemacs-show-hidden-files             t treemacs-silent-filewatch nil
                  treemacs-silent-refresh                nil treemacs-sorting 'alphabetic-asc
                  treemacs-space-between-root-nodes      t treemacs-tag-follow-cleanup            t
                  treemacs-tag-follow-delay 1.5 treemacs-width                         35)

            ;; The default width and height of the icons is 22 pixels. If you are
            ;; using a Hi-DPI display, uncomment this to double the icon size.
            ;;(treemacs-resize-icons 44)
            (treemacs-follow-mode t)
            (treemacs-filewatch-mode t)
            (treemacs-fringe-indicator-mode t)
            (pcase (cons (not (null (executable-find "git")))
                         (not (null treemacs-python-executable)))
              (`(t . t)
               (treemacs-git-mode 'deferred))
              (`(t . _)
               (treemacs-git-mode 'simple)))))

(use-package
  treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package
  treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(provide 'core/file-manager)
