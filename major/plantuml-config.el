;;; package -- plantuml-config
;;; Commentary:
;;; Code:
(use-package
  plantuml-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  :custom (org-plantuml-jar-path "/Users/baiyan/.emacs.d/plantuml.jar"))
(provide 'plantuml-config)
;;; plantuml-config.el ends here
