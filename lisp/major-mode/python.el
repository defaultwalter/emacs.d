;;; package -- python-config
;;; Commentary:
;;; Code:

(w/bind-lsp-map-for-mode 'python-mode)
(w/bind-dap-map-for-mode 'python-mode)

(use-package
  lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-executable (executable-find "python-language-server")))
(provide 'major-mode/python)
;;; python-config.el ends here
