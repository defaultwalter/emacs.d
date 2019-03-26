;;; package -- program-config
;;; Commentary:
;;; Code:
(use-package
  flycheck
  :ensure t
  :commands global-flycheck-mode
  :init (global-flycheck-mode))
(use-package lsp-mode
  :ensure t
  :commands lsp
  :config (require 'lsp-clients)
  (setq lsp-print-io t)
  :hook (prog-mode . lsp))
(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))
(use-package company-lsp
  :ensure t
  :after (company lsp-mode)
  :commands company-lsp)

(provide 'program-config)
;;; program-config.el ends here
