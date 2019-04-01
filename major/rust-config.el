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

;; (add-hook 'rust-mode-hook #'lsp)
(provide 'rust-config)
;;; rust-config.el ends here
