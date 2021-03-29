;;; package -- elisp-config
;;; Commentary:
;;; Code:
(use-package
  elisp-format
  :ensure t
  :defer t
  :init                                 ;
  (modal-set-leader-key emacs-lisp-mode-map "cf" '(elisp-format-buffer :name "format elisp")))
(provide 'major-mode/elisp)
;;; java-config.el ends here
