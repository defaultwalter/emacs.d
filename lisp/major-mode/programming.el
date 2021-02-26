;;; package -- programming
;;; Commentary:
;;; Code:


(use-package
  yasnippet
  :ensure t
  :defer t
  :hook (prog-mode . yas-minor-mode)
  :config                               ;
  (use-package
    yasnippet-snippets
    :ensure t
    :config (yas-reload-all)))

(use-package
  ivy-yasnippet
  :ensure t
  :after (yasnippet ivy)
  :defer t)

(use-package
  lsp-mode
  :ensure t
  :defer t
  :custom                               ;
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-auto-configure t)
  (lsp-log-io nil)
  (lsp-print-performance t)
  (lsp-file-watch-threshold 2000)
  (lsp-completion-provider :capf)
  (lsp-enable-snippet t)
  (lsp-idle-delay 0.5)
  ;; (lsp-keymap-prefix "SPC m l")
  ;; (lsp-eldoc-enable-hover nil)
  (lsp-enable-completion-at-point t)
  (lsp-keep-workspace-alive nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-semantic-highlighting nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-folding nil)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  :hook ((lsp-mode . lsp-enable-which-key-integration)))
(use-package
  lsp-ui
  :ensure t
  :after lsp-mode
  :custom                               ;
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-border "#777777")
  :hook (lsp-mode . lsp-ui-mode))

(use-package
  flycheck
  :ensure t
  :defer t
  :hook (lsp-mode . flycheck-mode))


;; (use-package
;;   company-tabnine
;;   :ensure t
;;   :config                               ;
;;   (add-to-list 'company-backends #'company-tabnine))

(use-package
  dap-mode
  :ensure t
  :defer t
  :config                               ;
  (require 'dap-python)
  (require 'dap-lldb))

(use-package
  quickrun
  :ensure t
  :defer t)


(add-hook 'prog-mode-hook (lambda ()
                            (hs-minor-mode t)
                            ;; (prettify-symbols-mode t)
                            ;; lsp-mode 会自动配置company-mode
                            ;; (company-mode t)
                            ))
(provide 'major-mode/programming)
