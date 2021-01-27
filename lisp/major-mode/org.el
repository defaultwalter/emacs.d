;;; package -- org-config
;;; Commentary:
;;; Code:


(use-package
  org
  :ensure org-plus-contrib
  :defer t
  :hook (org-mode . org-superstar-mode)
  :custom-face                          ;
  :init                                 ;
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (add-hook 'org-mode-hook (lambda ()
                             (setq truncate-lines nil)
                             (setq org-ellipsis " ······▾")
                             (setq org-edit-src-content-indentation 2)
                             (setq org-src-preserve-indentation nil)
                             (setq org-hide-emphasis-markers t)
                             (setq org-fontify-done-headline t)
                             (setq org-src-tab-acts-natively t)
                             (setq org-cycle-separator-lines 2)
                             (setq org-startup-folded 'content)
                             (setq org-hide-leading-stars t)
                             (setq org-pretty-entities t)
                             (setq-local prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
                                                                  ("#+END_SRC" . "†")
                                                                  ("#+begin_src" . "†")
                                                                  ("#+end_src" . "†")))
                             (setq-local prettify-symbols-unprettify-at-point 'right-edge)
                             (org-display-inline-images t t)
                             (org-indent-mode 1)
                             (prettify-symbols-mode 1)
                             (visual-fill-column-mode 1)))
  ;; (setq org-image-actual-width '(100 200 300))
  (setq-default org-confirm-babel-evaluate nil)
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
  (require 'org-tempo))


(use-package
  ob-plantuml
  :init (setq-default org-plantuml-exec-mode 'plantuml)
  (setq-default org-plantuml-jar-path ""))

(use-package
  evil-org
  :ensure t
  :after org
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode))
  :config                  ;
  ;; (add-hook 'evil-org-mode-hook (lambda ()
  ;;                                 (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package
  org-superstar
  :ensure t
  :defer t
  :custom                               ;
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
  :hook (org-mode . org-superstar-mode)
  :init                                 ;
  (setq org-superstar-prettify-item-bullets nil))


(use-package
  org-roam
  :ensure t
  :defer t
  :custom                               ;
  (org-roam-buffer "*Relationship*")
  (org-roam-directory maf/note-directory)
  (org-roam-index-file "Index.org")
  (org-roam-dailies-directory "Journal")
  (org-roam-title-sources '(headline))
  (org-roam-tag-sources '(vanilla all-directories))
  (org-roam-capture-templates '(("d" "default" plain #'org-roam-capture--get-point "%?"
                                 :file-name "%<%y%m%d%H%M%S>-${slug}"
                                 :head "* ${title} \n"
                                 :unnarrowed t)))
  (org-roam-capture-immediate-template '("d" "default" plain #'org-roam-capture--get-point "%?"
                                         :file-name "%<%y%m%d%H%M%S>-${slug}"
                                         :head "* ${title} \n"
                                         :unnarrowed t))
  (org-roam-dailies-capture-templates '(("d" "default" entry #'org-roam-capture--get-point "* %?"
                                         :file-name "Daily/%<%Y-%m-%d>"
                                         :head "* %<%A, %d %B %Y> \n")))
  :init                                 ;
  (maf/leader-key "nd" '(org-roam-dailies-today :which-key "today"))
  (maf/leader-key "nf" '(org-roam-find-file :which-key "find note"))
  (maf/leader-key org-mode-map "nv" '(org-roam :which-key "view"))
  (maf/leader-key org-mode-map "ng" '(org-roam-graph :which-key "graph"))
  (maf/leader-key org-mode-map "ni" '(org-roam-insert :which-key "insert node"))
  :config                               ;
  (require 'org-roam-protocol))

(use-package
  deft
  :ensure t
  :after org
  :custom (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory maf/note-directory)
  :config                               ;
  (maf/leader-key "nn" '(deft :which-key "list")))

(use-package
  org-roam-server
  :ensure t
  :defer t
  :custom                               ;
  (org-roam-server-host maf/note-server-host )
  (org-roam-server-port maf/note-server-port )
  (org-roam-server-authenticate nil)
  (org-roam-server-export-inline-images t)
  (org-roam-server-serve-files nil)
  (org-roam-server-served-file-extensions '("pdf" "mp4" "ogv"))
  (org-roam-server-network-poll t)
  (org-roam-server-network-arrows nil)
  (org-roam-server-network-label-truncate t)
  (org-roam-server-network-label-truncate-length 60 )
  (org-roam-server-network-label-wrap-length 20)
  :init                                 ;
  (maf/leader-key "ns" '((lambda ()
                           (interactive)
                           (when (not (bound-and-true-p org-roam-server-mode))
                             (org-roam-server-mode t))
                           (browse-url (format "http://%s:%s" org-roam-server-host
                                               org-roam-server-port))) :which-key "server"))
  :config )

(setq org-agenda-files (mapcar (lambda (file)
                                 (expand-file-name file maf/agenda-directory))
                               (directory-files maf/agenda-directory nil ".*\.org")))
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))


(provide 'major-mode/org)
;;; org-config.el ends here
