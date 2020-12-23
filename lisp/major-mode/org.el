;;; package -- org-config
;;; Commentary:
;;; Code:

(use-package
  org
  :ensure org-plus-contrib
  :custom-face          ;
  ;; (org-block ((t
  ;;              (:background nil
  ;;                           :slant italic))))
  ;; (org-block-begin-line ((t
  ;;                         (:background nil
  ;;                                      :slant italic))))
  ;; (org-block-end-line ((t
  ;;                       (:background nil
  ;;                                    :slant italic))))
  :config                               ;
  (require 'ob-dot)
  ;; (require 'ob-plantuml)
  (require 'ob-python)
  (require 'ob-shell)
  (require 'ob-java)
  (require 'ob-js)
  (require 'ob-python)
  (require 'ob-latex)
  (require 'ox-freemind)
  (require 'org-tempo)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (add-hook 'org-mode-hook (lambda ()
                             (if (fboundp 'org-display-inline-images)
                                 (org-display-inline-images t t))
                             (if (fboundp 'org-indent-mode)
                                 (org-indent-mode))
                             (setq truncate-lines nil)
                             (setq left-margin-width 2)
                             (setq right-margin-width 5)))
  ;; (setq org-image-actual-width '(100 200 300))
  (setq-default org-confirm-babel-evaluate nil))

(use-package
  ob-plantuml
  :init (setq-default org-plantuml-exec-mode 'plantuml)
  (setq-default org-plantuml-jar-path ""))

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

(use-package
  org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :init                                 ;
  (setq org-superstar-prettify-item-bullets nil))


(provide 'major-mode/org)
;;; org-config.el ends here
