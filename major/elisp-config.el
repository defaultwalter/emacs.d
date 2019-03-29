;;; package -- elisp-config
;;; Commentary:
;;; Code:
(add-hook 'emacs-lisp-mode-hook (lambda () (setq prettify-symbols-alist
                                            '(
                                              ("lambda" . #x3bb)))
                                  (prettify-symbols-mode)))
(provide 'elsip-config)
;;; java-config.el ends here
