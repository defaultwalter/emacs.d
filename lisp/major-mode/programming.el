;;; package -- programming
;;; Commentary:
;;; Code:


;; Auto complete parentheses
;; (use-package
;;   autopair
;;   :ensure t
;;   :defer t
;;   :hook (prog-mode . autopair-mode))

(use-package
  yasnippet
  :ensure t
  :defer t
  :hook (prog-mode . yas-minor-mode)
  :config                               ;
  (use-package
    yasnippet-snippets
    :ensure t
    :defer t
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
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :init  ;
  ;; (defun m/watcher(s n o w) (message "new value %s|%s" n w))
  ;; (add-variable-watcher 'lsp-enable-snippet 'm/watcher)
  (setq lsp-auto-configure t)
  (setq lsp-log-io nil)
  (setq lsp-file-watch-threshold 2000)
  (setq lsp-prefer-capf t)
  (setq lsp-enable-snippet t)
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-enable-completion-at-point t)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-enable-semantic-highlighting nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-text-document-color nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  :config                               ;
  )
(use-package
  lsp-ui
  :ensure t
  :after lsp-mode
  :custom                               ;
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-border "#999999")
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
