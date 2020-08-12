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
  (w/create-leader-key-for-mode mode "f" 'lsp-format-buffer "format" content-map-prefix)
  (w/create-leader-key-for-mode mode "r" 'lsp-rename "rename" content-map-prefix)
  (w/create-leader-key-for-mode mode "h" 'lsp-ui-doc-glance "show-document" view-map-prefix)
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
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :init ;;
  ;; (setq lsp-auto-configure nil)
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

  (add-hook 'lsp-completion-mode-hook (lambda ()
                                        (when lsp-completion-mode
                                          (set (make-local-variable 'company-backends)
                                               (remq 'company-capf company-backends)))))
  :config (require 'lsp-clients))
(use-package
  lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :custom (lsp-ui-doc-position (quote at-point))
  ;; (lsp-ui-doc-use-webkit t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-border "orange")
  :hook (lsp-mode . lsp-ui-mode))


;; Auto complete
(use-package
  company
  :ensure t
  :defer t
  :hook (prog-mode . company-mode)
  :init ;; Don't convert to downcase.
  (setq-default company-dabbrev-downcase nil)
  :bind (("C-<tab>" . company-complete-common)
         ;;
         :map company-active-map        ;
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-s" . company-filter-candidates)
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)
         ("<return>" . company-complete-selection) ; 终端下无效
         ("RET" . company-complete-selection)      ; 终端下生效
         :map company-filter-map                   ;
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)
         ("<return>" . company-complete-selection) ; 终端下无效
         ("RET" . company-complete-selection)      ; 终端下生效
         :map company-search-map                   ;
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)
         ("<return>" . company-complete-selection) ; 终端下无效
         ("RET" . company-complete-selection)      ; 终端下生效
         )
  :custom                               ;
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :config                               ;
  (setq company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           (company-dabbrev)))
  (setq company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)))

(use-package
  company-statistics
  :ensure t
  :after company
  :hook (company-mode . company-statistics-mode)
  :custom                               ;
  (company-statistics-file (expand-file-name "company-statistics-cache.el" misc-file-directory)))
;; (use-package
;;   company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode)
;;   :init                                 ;
;;   (setq company-box-show-single-candidate t)
;;   :config)

;; 根据文档推荐使用company-capf
;; (use-package
;;   company-lsp
;;   :ensure t
;;   :after (company lsp-mode)
;;   :commands company-lsp
;;   :init (push '(company-lsp company-yasinppet) company-backends))

(use-package
  yasnippet
  :ensure t
  :defer t
  :hook (company-mode . yas-minor-mode)
  :config                               ;
  (yas-reload-all))
(use-package
  yasnippet-snippets
  :ensure t
  :config (yas-reload-all))

(use-package
  dap-mode
  :ensure t
  :defer t
  :config (require 'dap-python)
  (require 'dap-java))

(use-package
  quickrun
  :ensure t
  :defer t)

(use-package
  highlight-indent-guides               ;高亮缩进
  :ensure t
  :init                                 ;
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-odd-face-perc 1)
  (setq highlight-indent-guides-auto-even-face-perc 3)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-auto-character-face-perc 5)
  :hook (prog-mode . highlight-indent-guides-mode))

(add-hook 'prog-mode-hook (lambda ()
                            (hs-minor-mode t)
                            (linum-mode t)
                            ;; (prettify-symbols-mode t)
                            ;; lsp-mode 会自动配置company-mode
                            ;; (company-mode t)
                            ))
(provide 'major-mode/programming)
