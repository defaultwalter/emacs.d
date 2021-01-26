;;; package -- elisp-config
;;; Commentary:
;;; Code:
(use-package
  elisp-format
  :ensure t
  :defer t
  :init                                 ;
  (maf/leader-key emacs-lisp-mode-map "cf" '(elisp-format-buffer :which-key "format")))
(provide 'major-mode/elisp)
;;; java-config.el ends here
