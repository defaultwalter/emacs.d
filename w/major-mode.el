;;; package -- program-config
;;; Commentary:
;;; Code:
(use-package
  flycheck
  :ensure t
  :defer t)

(use-package
  lsp-mode
  :ensure t
  :defer t
  :init (setq lsp-prefer-flymake nil)
  :config (require 'lsp-clients)
  (setq lsp-print-io t))
(use-package
  lsp-ui
  :ensure t
  :defer t
  :after lsp-mode
  :commands lsp-ui-mode
  :custom (lsp-ui-doc-position (quote at-point))
  (lsp-ui-doc-use-webkit -1)
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-border "gray12")
  :hook (lsp-mode . lsp-ui-mode))
(use-package
  dap-mode
  :ensure t
  :defer t)


;; Auto complete
(use-package
  company
  :ensure t
  :init
  ;; Don't convert to downcase.
  (setq-default company-dabbrev-downcase nil)
  :config (global-company-mode t)
  :bind (("C-." . company-complete)
         ;;
         :map company-active-map ("C-n" . company-select-next)
         ("C-p" . company-select-previous)))
(use-package
  company-statistics
  :ensure t
  :after company
  :config (company-statistics-mode))
(use-package
  company-lsp
  :ensure t
  :after (company lsp-mode)
  :commands company-lsp
  :config (push 'company-lsp company-backends))



(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(global-prettify-symbols-mode)

(require 'major-mode/rust)
(require 'major-mode/python)
(require 'major-mode/java)
(require 'major-mode/elisp)
(require 'major-mode/graphviz)
(require 'major-mode/plantuml)
(require 'major-mode/org)

(provide 'major-mode)
;;; program-config.el ends here
