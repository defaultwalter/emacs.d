;;; package -- program-config
;;; Commentary:
;;; Code:
(use-package
  flymake
  :ensure t)
(use-package
  flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))
(use-package
  lsp-mode
  :ensure t
  :config (require 'lsp-clients)
  (setq lsp-print-io t)
  :hook (prog-mode . lsp))
(use-package
  lsp-ui
  :ensure t
  :after (lsp-mode lsp-ui)
  :commands lsp-ui-mode
  :custom (lsp-ui-doc-position (quote top))
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable t)
  :hook (lsp-mode . lsp-ui-mode))
(use-package
  dap-mode
  :ensure t
  :config
  (dap-mode 1)
  (dap-ui-mode 1))
(use-package
  company-lsp
  :ensure t
  :after (company lsp-mode)
  :commands company-lsp)

(use-package
  ivy-yasnippet
  :ensure t
  :after (yasnippet)
  :bind ("C-." . ivy-yasnippet))
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(global-prettify-symbols-mode)
(provide 'program-config)
;;; program-config.el ends here
