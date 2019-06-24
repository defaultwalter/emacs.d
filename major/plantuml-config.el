;;; package -- plantuml-config
;;; Commentary:
;;; Code:
(use-package
  plantuml-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
                       ;; (setq-default plantuml-default-exec-mode 'jar)
                       ;; (setq plantuml-jar-path "/Users/baiyan/.emacs.d/plantuml.jar")
                       ;; (add-hook 'org-mode-hook (lambda ()
                       ;;                            (plantuml-set-exec-mode "jar")))
  (setq plantuml-server-url "http://localhost:30000")
  :custom (org-plantuml-jar-path "/Users/baiyan/.emacs.d/plantuml.jar")
  )
(provide 'plantuml-config)
;;; plantuml-config.el ends here
