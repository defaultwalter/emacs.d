(use-package 
  rust-mode 
  :ensure t)
(use-package 
  cargo 
  :ensure t 
  :hook (rust-mode . cargo-minor-mode))
(use-package 
  racer 
  :ensure t
  :hook (rust-mode . racer-mode)
  (racer-mode . eldoc-mode))
(use-package 
  flycheck-rust 
  :ensure t 
  :after flycheck 
  :config (with-eval-after-load 'rust-mode (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
(provide 'rust-config)
