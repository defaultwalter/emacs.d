;;; package -- elisp-config
;;; Commentary:
;;; Code:
(use-package
  elisp-format
  :ensure t
  :defer t
  :init                                 ;
  (modal-leader-set-key-for-mode 'emacs-lisp-mode "cf" '(elisp-format-buffer :which-key "format elisp"))
  )


(provide 'major-mode/elisp)
;;; java-config.el ends here
