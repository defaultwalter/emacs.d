(use-package 
  rust-mode 
  :ensure t)
(use-package 
  cargo 
  :ensure t 
  :hook (rust-mode . cargo-minor-mode))
;; (use-package 
;;   racer 
;;   :ensure t
;;   :hook (rust-mode . racer-mode)
;;   (racer-mode . eldoc-mode))
;; (use-package lsp-rust
;;   :ensure t
;;   :after (lsp-mode)
;;   :init (setq lsp-rust-rls-command '("rustup" "run" "stable" "rls"))
;;   :hook (rust-mode . lsp-rust-enable))
(use-package 
  flycheck-rust 
  :ensure t 
  :after flycheck 
  :config (with-eval-after-load 'rust-mode (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(add-hook 'rust-mode-hook #'lsp)
(provide 'rust-config)
