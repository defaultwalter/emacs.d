(setq-default org-agenda-files '("~/Documents/GTD"))
(add-hook 'org-mode-hook (lambda () 
                           (set-face-attribute 'org-table nil 
                                               :font (font-spec :name
                                                                "-*-Source Code Pro-ultrabold-italic-normal-*-*-*-*-*-m-0-iso10646-1" 
                                                                :weight 'normal 
                                                                :slant 'normal 
                                                                :size 15)) 
                           (org-display-inline-images t t)
                           (org-indent-mode) 
                           (setq truncate-lines nil)))
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(provide 'org-config)
