;;; package -- python-config
;;; Commentary:
;;; Code:
(use-package
  dap-python
  :after (lsp-mode dap-mode))
(add-hook 'python-mode 'flycheck-mode)
(provide 'python-config)
;;; python-config.el ends here
