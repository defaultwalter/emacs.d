;;; package -- elisp-config
;;; Commentary:
;;; Code:
(use-package
  parinfer
  :ensure t
  :disabled
  :hook (emacs-lisp-mode . parinfer-mode)
  :init (setq parinfer-extensions '(defaults ; should be included.
                                     pretty-parens ; different paren styles for different modes.
                                     evil          ; If you use Evil.
                                     lispy ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
                                     paredit ; Introduce some paredit commands.
                                     smart-tab ; C-b & C-f jump positions and smart shift with tab & S-tab.
                                     smart-yank)) ; Yank behavior depend on mode.
  )
(use-package
  elisp-format
  :ensure t
  :defer t
  :init                                 ;
  (maf/leader-key emacs-lisp-mode-map "cf" '(elisp-format-buffer :which-key "format")))
(provide 'major-mode/elisp)
;;; java-config.el ends here
