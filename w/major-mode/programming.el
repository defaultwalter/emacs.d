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
  flycheck
  :ensure t
  :defer t
  :hook (lsp-mode . flycheck-mode))

(defun w/bind-lsp-map-for-mode (mode)
  (w/create-leader-key-for-mode mode "f" 'lsp-format-buffer "format" major-map-prefix)
  (w/create-leader-key-for-mode mode "r" 'lsp-rename "rename" major-map-prefix)
  (w/create-leader-key-for-mode mode "d" 'lsp-ui-peek-find-definitions "peek-definitions"
                                goto-map-prefix)
  (w/create-leader-key-for-mode mode "r" 'lsp-ui-peek-find-references "peek-references"
                                goto-map-prefix)
  (w/create-leader-key-for-mode mode "i" 'lsp-ui-peek-find-implementation "peek-implementation"
                                goto-map-prefix))



(defun w/bind-dap-map-for-mode (mode)
  (setq debug-map-prefix (w/create-leader-keymap-for-mode mode "d" debug-map "debug"
                                                          major-map-prefix))
  (w/create-leader-key-for-mode mode "h" 'dap-hydra "control"  debug-map-prefix)
  (w/create-leader-key-for-mode mode "d" 'dap-debug "debug" debug-map-prefix)
  (w/create-leader-key-for-mode mode "r" 'dap-debug-restart "debug-restart" debug-map-prefix)
  (w/create-leader-key-for-mode mode "l" 'dap-debug-last "debug-last" debug-map-prefix)
  (setq breakpoint-map-prefix (w/create-leader-keymap-for-mode mode "b" breakpoint-map "breakpoint"
                                                               major-map-prefix))
  (w/create-leader-key-for-mode mode "a" 'dap-breakpoint-add "add-breakpoint" breakpoint-map-prefix)
  (w/create-leader-key-for-mode mode "c" 'dap-breakpoint-condition "set-condition"
                                breakpoint-map-prefix)
  (w/create-leader-key-for-mode mode "t" 'dap-breakpoint-toggle "toggle-breakpoint"
                                breakpoint-map-prefix)
  (w/create-leader-key-for-mode mode "b" 'dap-breakpoint-toggle "toggle-breakpoint"
                                breakpoint-map-prefix)
  (w/create-leader-key-for-mode mode "d" 'dap-breakpoint-delete "delete-breakpoint"
                                breakpoint-map-prefix)
  (w/create-leader-key-for-mode mode "D" 'dap-breakpoint-delete-all "delete-all-breakpoint"
                                breakpoint-map-prefix))


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
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-border "orange")
  :hook (lsp-mode . lsp-ui-mode))

(use-package
  dap-mode
  :ensure t
  :defer t
  :config (require 'dap-python)
  (require 'dap-java))
;; Auto complete
(use-package
  company
  :ensure t
  :init
  ;; Don't convert to downcase.
  (setq-default company-dabbrev-downcase nil)
  (add-hook 'after-init-hook 'global-company-mode)
  :bind (("C-<tab>" . company-complete-common)
         ;;
         :map company-active-map        ;
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-s" . company-filter-candidates)
         ("<tab>" . company-complete-selection)
         ("<return>" . company-complete-selection)
         :map company-filter-map        ;
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("<tab>" . company-complete-selection)
         ("<return>" . company-complete-selection)
         :map company-search-map        ;
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("<tab>" . company-complete-selection)
         ("<return>" . company-complete-selection)      )
  :custom                               ;
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  ;; (company-show-numbers t)
  :config (setq company-backends '((company-files ; files & directory
                                    company-keywords ; keywords
                                    company-capf company-yasnippet)
                                   (company-abbrev company-dabbrev))))
(use-package
  company-box
  :ensure t
  :defer t
  :hook (company-mode . company-box-mode)
  :config )

(use-package
  company-statistics
  :ensure t
  :after company
  :custom                               ;
  (company-statistics-file (expand-file-name "company-statistics-cache.el" misc-file-directory))
  :config (company-statistics-mode))
(use-package
  company-lsp
  :ensure t
  :after (company lsp-mode)
  :commands company-lsp
  :config (push 'company-lsp company-backends))

(use-package
  yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config                               ;
  (yas-reload-all))
(use-package
  yasnippet-snippets
  :ensure t
  :after yasnippet)

(add-hook 'prog-mode-hook
          (lambda ()
            ( hs-minor-mode t)
            (linum-mode t)
            (prettify-symbols-mode t)))

(provide 'major-mode/programming)
