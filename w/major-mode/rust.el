;;; package -- rust-config
;;; Commentary:
;;; Code:
(use-package
  rust-mode
  :ensure t
  :defer t
  :hook (rust-mode .
                   (lambda ()
                     (lsp-deferred)
                     (dap-mode 1)
                     (dap-ui-mode 1)))
  :config )
(use-package
  cargo
  :ensure t
  :defer t
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))
;; (add-hook 'rust-mode-hook #'lsp)
(provide 'major-mode/rust)
;;; rust-config.el ends here
