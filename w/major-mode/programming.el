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

(defun w/bind-lsp-map-for-mode (mode)
  (let* ((mode-map (w/get-mode-map mode))
         (lsp-content-map (make-sparse-keymap))
         (goto-map (lookup-key mode-map "g")))
    ;; (setq lsp-content-map (copy-keymap content-map))
    ;; (set-keymap-parent lsp-content-map content-map)
    (define-prefix-command 'lsp-content-map 'lsp-content-map "Content Map")
    ;; (w/create-keymap lsp-content-map "Content Map")
    (set-keymap-parent lsp-content-map content-map)
    (w/bind-to-map mode-map "c" 'lsp-content-map )
    (w/bind-to-map  lsp-content-map "f" lsp-format-buffer "format")
    (w/bind-to-map  lsp-content-map "r" lsp-rename "rename")
    (w/bind-to-map  goto-map "d" 'lsp-ui-peek-find-definitions "peek-definitions")
    (w/bind-to-map  goto-map "r" 'lsp-ui-peek-find-references "peek-references")
    (w/bind-to-map  goto-map "i" 'lsp-ui-peek-find-implementation "peek-implementation")))



(defun w/bind-dap-map-for-mode (mode)
  (let* ((mode-map (w/get-mode-map mode))
         (major-map (lookup-key mode-map "m"))
         (debug-map (make-sparse-keymap))
         (breakpoint-map (make-sparse-keymap)))
    (define-prefix-command 'debug-map 'debug-map "Debug Map")
    (define-prefix-command 'breakpoint-map 'breakpoint-map "Breakpoint Map")
    (w/bind-to-map major-map "d" 'debug-map)
    (w/bind-to-map major-map "b" 'breakpoint-map)
    (w/bind-to-map debug-map "h" 'dap-hydra "control")
    (w/bind-to-map debug-map "d" 'dap-debug "debug")
    (w/bind-to-map debug-map "r" 'dap-debug-restart "debug-restart")
    (w/bind-to-map debug-map "l" 'dap-debug-last "debug-last")
    (w/bind-to-map breakpoint-map "a" 'dap-breakpoint-add "add-breakpoint")
    (w/bind-to-map breakpoint-map "c" 'dap-breakpoint-condition "set-condition")
    (w/bind-to-map breakpoint-map "t" 'dap-breakpoint-toggle "toggle-breakpoint")
    (w/bind-to-map breakpoint-map "b" 'dap-breakpoint-toggle "toggle-breakpoint")
    (w/bind-to-map breakpoint-map "d" 'dap-breakpoint-delete "delete-breakpoint")
    (w/bind-to-map breakpoint-map "D" 'dap-breakpoint-delete-all "delete-all-breakpoint")))


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
  :bind (("C-<tab>" . company-indent-or-complete-common)
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
         ("<return>" . company-complete-selection)      ))
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
