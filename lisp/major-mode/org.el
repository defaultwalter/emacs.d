;;; package -- org-config
;;; Commentary:
;;; Code:
(use-package
  org
  :ensure org-plus-contrib
  :custom-face                          ;
  :init                                 ;
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (add-hook 'org-mode-hook (lambda ()
                             (org-display-inline-images t t)
                             (org-indent-mode 1)
                             (setq truncate-lines nil)
                             (setq org-hide-emphasis-markers t)
                             (setq org-fontify-done-headline t)
                             (setq org-hide-leading-stars t)
                             (setq org-pretty-entities t)
                             ;; (setq left-margin-width 2)
                             ;; (setq right-margin-width 5)
                             (setq-local prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
                                                                  ("#+END_SRC" . "†")
                                                                  ("#+begin_src" . "†")
                                                                  ("#+end_src" . "†")))
                             (setq-local prettify-symbols-unprettify-at-point 'right-edge)
                             (prettify-symbols-mode 1)))
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

;; (use-package
;;   evil-org
;;   :ensure t
;;   :after org
;;   :hook (org-mode . evil-org-mode)
;;   :config                  ;
;;   ;; (add-hook 'evil-org-mode-hook (lambda ()
;;   ;;                                 (evil-org-set-key-theme)))
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys))

(use-package
  org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :init                                 ;
  (setq org-superstar-prettify-item-bullets nil))


(use-package
  org-roam
  :ensure t
  :hook (after-init . org-roam-mode)
  :custom                               ;
  (org-roam-buffer "*Relationship*")
  (org-roam-directory "~/Storage/Nutstore/Notes")
  (org-roam-index-file "Index.org")
  (org-roam-dailies-directory "Daily")
  (org-roam-title-sources '((title headline) alias))
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
  :config                               ;
  (setq note-map-prefix (w/create-leader-keymap "n" note-mode-map "note"))
  (w/create-leader-key "n" 'org-roam-jump-to-index "index" note-map-prefix)
  (w/create-leader-key "d" 'org-roam-dailies-find-today "today" note-map-prefix)
  (w/create-leader-key "f" 'org-roam-find-file "find-file" note-map-prefix)
  (w/create-leader-key-for-mode 'org-mode "l" 'org-roam "backlink" note-map-prefix)
  (w/create-leader-key-for-mode 'org-mode "g" 'org-roam-graph "graph" note-map-prefix)
  (w/create-leader-key-for-mode 'org-mode "i" 'org-roam-insert "insert" note-map-prefix)
  (w/create-leader-key-for-mode 'org-mode "I" 'org-roam-insert-immediate "insert-immediate"
                                note-map-prefix)
  (require 'org-roam-protocol))
(use-package
  deft
  :ensure t
  :after org
  :bind ("C-c n d" . deft)
  :custom (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Storage/Nutstore/Notes"))

(use-package
  org-roam-server
  :ensure t
  :config (setq org-roam-server-host "127.0.0.1" org-roam-server-port 8010
                org-roam-server-authenticate nil org-roam-server-export-inline-images t
                org-roam-server-serve-files nil org-roam-server-served-file-extensions '("pdf" "mp4"
                                                                                         "ogv")
                org-roam-server-network-poll t org-roam-server-network-arrows nil
                org-roam-server-network-label-truncate t
                org-roam-server-network-label-truncate-length 60
                org-roam-server-network-label-wrap-length 20))

(setq org-agenda-files '("~/Storage/Nutstore/Notes/GTD/inbox.org"
                         "~/Storage/Nutstore/Notes/GTD/task.org"
                         "~/Storage/Nutstore/Notes/GTD/project.org"
                         "~/Storage/Nutstore/Notes/GTD/someday.org"))
(setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))

(provide 'major-mode/org)
;;; org-config.el ends here
