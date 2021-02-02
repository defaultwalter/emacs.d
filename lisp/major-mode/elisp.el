;;; package -- elisp-config
;;; Commentary:
;;; Code:
(use-package
  elisp-format
  :ensure t
  :defer t
  :init                                 ;
  (user/leader-key emacs-lisp-mode-map "cf" '(elisp-format-buffer :which-key "format elisp")))
(provide 'major-mode/elisp)
;;; java-config.el ends here
