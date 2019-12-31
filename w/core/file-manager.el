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

(provide 'core/file-manager)