;;; package -- program-config
;;; Commentary:
;;; Code:
(use-package
  flycheck
  :ensure t)
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
  :custom (lsp-ui-doc-position (quote at-point))
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-sideline-enable nil)
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
  yasnippet
  :ensure t
  :config (yas-reload-all)
  :hook (prog-mode . yas-minor-mode))
(use-package
  yasnippet-snippets
  :ensure t
  :after (yasnippet))

(use-package
  ivy-yasnippet
  :ensure t
  :after (yasnippet evil-leader))
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'linum-mode)

(provide 'program-config)
;;; program-config.el ends here
