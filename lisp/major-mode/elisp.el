;;; package -- elisp-config
;;; Commentary:
;;; Code:
(use-package
  elisp-format
  :ensure t
  :defer t
  :init                                 ;
  (user/leader-key "cf" '(elisp-format-buffer :name "format elisp" :mode emacs-lisp-mode)))
(provide 'major-mode/elisp)
;;; java-config.el ends here
