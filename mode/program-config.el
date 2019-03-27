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
  :config (require 'lsp-clients)
  (setq lsp-print-io t)
  :hook (prog-mode . lsp))
(use-package lsp-ui
  :ensure t
  :after (lsp-mode lsp-ui)
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))
(use-package company-lsp
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
  :after (yasnippet evil-leader)
  :config (evil-leader/set-key "y" 'ivy-yasnippet))


(provide 'program-config)
;;; program-config.el ends here
