;;; package -- elisp-config
;;; Commentary:
;;; Code:
(use-package
  elisp-format
  :ensure t
  :defer t
  :init          ;(w/leader-set-key-for-mode 'emacs-lisp-mode "m" nil)
  (w/leader-set-key-for-mode 'emacs-lisp-mode "f" "format" 'elisp-format-buffer @content-map))
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (setq prettify-symbols-alist '(("lambda" . #x3bb)))))
(provide 'major-mode/elisp)
;;; java-config.el ends here
