;;; package -- python-config
;;; Commentary:
;;; Code:

(use-package
  lsp-python-ms
  :ensure t
  :defer t
  :init (setq lsp-python-ms-executable (executable-find "python-language-server")))
(provide 'major-mode/python)
;;; python-config.el ends here
