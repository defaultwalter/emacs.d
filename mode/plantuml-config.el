(use-package 
  plantuml-mode 
  :ensure t 
  :config (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)) 
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)) 
  (org-babel-do-load-languages 'org-babel-load-languages '(;; other Babel languages
                                                           (plantuml . t))))
(provide 'plantuml-config)
