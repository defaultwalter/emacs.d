;;; package -- java-config
;;; Commentary:
;;; Code:
;; (use-package
;;   lsp-java
;;   :ensure t
;;   :after (lsp))

(use-package
  lsp-java
  :ensure t
  :hook (java-mode .
                   (lambda ()
                     (lsp)
                     (dap-mode 1)
                     (dap-ui-mode 1)))
  :config (eval-after-load "evil-leader" (progn (evil-leader/set-key-for-mode 'java-mode "d"
                                                  'lsp-ui-doc-glance))))
;; (use-package
;;   dap-java
;;   :after (lsp-java dap-mode))
;; (use-package
;;   lsp-java-treemacs
;;   :after (treemacs))
(provide 'lang/java)
;;; java-config.el ends here
