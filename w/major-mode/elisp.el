;;; package -- elisp-config
;;; Commentary:
;;; Code:
(setq @emacs-lisp-mode-map (make-sparse-keymap))
(define-prefix-command '@emacs-lisp-mode-map '@emacs-lisp-mode-map "Elisp")
(evil-leader/set-key-for-mode 'emacs-lisp-mode "m" '@emacs-lisp-mode-map)
(use-package
  elisp-format
  :ensure t
  :defer t
  :init (bind-to-map '@emacs-lisp-mode-map "f" 'elisp-format-buffer "format"))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq prettify-symbols-alist '(("lambda" . #x3bb)))))

(provide 'major-mode/elisp)
;;; java-config.el ends here
