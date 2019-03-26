;;; package -- rust-config
;;; Commentary:
;;; Code:
(use-package
  rust-mode
  :ensure t
  :after (lsp-ui))
(use-package
  cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))
(use-package
  flycheck-rust
  :ensure t
  :after flycheck
  :config (with-eval-after-load 'rust-mode (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; (add-hook 'rust-mode-hook #'lsp)
(provide 'rust-config)
;;; rust-config.el ends here
