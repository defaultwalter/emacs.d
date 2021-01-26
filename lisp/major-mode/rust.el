;;; package -- rust-config
;;; Commentary:
;;; Code:
(use-package
  rust-mode
  :ensure t
  :defer t
  :init )
(use-package
  cargo
  :ensure t
  :defer t
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))
(provide 'major-mode/rust)
;;; rust-config.el ends here
