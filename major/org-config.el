;;; package -- org-config
;;; Commentary:
;;; Code:
(use-package org
  :ensure org-plus-contrib
  :config (require 'ox-freemind))
(setq-default org-agenda-files '("~/Documents/GTD"))
(add-hook 'org-mode-hook (lambda ()
                           (set-face-attribute 'org-table nil
                                               :font (font-spec :name
                                                                "-*-Source Code Pro-ultrabold-italic-normal-*-*-*-*-*-m-0-iso10646-1"
                                                                :weight 'normal
                                                                :slant 'normal
                                                                :size 15))
                           (if (fboundp 'org-display-inline-images)
                               (org-display-inline-images t t))

                           (if (fboundp 'org-indent-mode)
                               (org-indent-mode))
                           (setq truncate-lines nil)))
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(use-package ob-python)
(use-package ob-shell)
(use-package ob-plantuml)
(use-package ob-java)
(use-package ob-js)
(use-package ob-python)
(provide 'org-config)
;;; org-config.el ends here
