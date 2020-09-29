;;; package -- org-config
;;; Commentary:
;;; Code:

(use-package
  org
  :ensure org-plus-contrib
  :config ;
  ;; (require 'ob-python)
  ;; (require 'ob-shell)
  ;; (require 'ob-plantuml)
  ;; (require 'ob-java)
  ;; (require 'ob-js)
  ;; (require 'ob-python)
  ;; (require 'ob-latex)
  ;; (require 'ob-dot)
  ;; (require 'ox-freemind)
  ;; (require 'org-tempo)

  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (add-hook 'org-mode-hook (lambda ()
                             (if (fboundp 'org-display-inline-images)
                                 (org-display-inline-images t t))
                             (if (fboundp 'org-indent-mode)
                                 (org-indent-mode))
                             (setq truncate-lines nil)))
  (setq org-image-actual-width '(100 200 300))
  (setq-default org-agenda-files '("~/Nutstore Files/GTD"))
  (setq-default org-confirm-babel-evaluate nil)
  (require 'org-tempo))


(use-package
  evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config                               ;
  (add-hook 'evil-org-mode-hook (lambda ()
                                  (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(provide 'major-mode/org)
;;; org-config.el ends here
