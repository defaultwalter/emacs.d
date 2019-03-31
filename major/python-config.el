;;; package -- python-config
;;; Commentary:
;;; Code:
(use-package
  dap-python
  :after (lsp-mode dap-mode))
(add-hook 'python-mode 'flycheck-mode)
(require 'ob-python)
(provide 'python-config)
;;; python-config.el ends here
