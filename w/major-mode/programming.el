;;; package -- programming
;;; Commentary:
;;; Code:


;; Auto complete parentheses
(use-package
  autopair
  :ensure t
  :defer t
  :hook (prog-mode . autopair-mode))
(use-package
  flycheck
  :ensure t
  :defer t
  :hook (lsp-mode . flycheck-mode))

(use-package
  lsp-mode
  :ensure t
  :defer t
  :init ;; (setq lsp-prefer-flymake nil)
  :config (require 'lsp-clients)
  (setq lsp-print-io t))
(use-package
  lsp-ui
  :ensure t
  :defer t
  :after lsp-mode
  :commands lsp-ui-mode
  :custom (lsp-ui-doc-position (quote at-point))
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-border "orange")
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
  (add-hook 'after-init-hook 'global-company-mode)
  :bind (("TAB" . company-indent-or-complete-common)
         ;;
         :map company-active-map ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-a" . company-select-next)))

(use-package
  company-box
  :ensure t
  :defer t
  :hook (company-mode . company-box-mode))

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

(provide 'major-mode/programming)
