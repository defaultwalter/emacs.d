;;; package -- python-config
;;; Commentary:
;;; Code:

(setq @python-mode-map (make-sparse-keymap))
(define-prefix-command '@python-mode-map '@python-mode-map "Major")
(evil-leader/set-key-for-mode 'python-mode "m" '@python-mode-map)
;; (use-package
;;   dap-python
;;   :after (lsp-mode dap-mode))

(provide 'major-mode/python)
;;; python-config.el ends here
