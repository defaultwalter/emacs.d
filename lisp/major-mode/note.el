(use-package
  org-roam
  :ensure t
  :hook (after-init . org-roam-mode)
  :custom                               ;
  (org-roam-directory "~/Storage/Nutstore/Notes")
  (org-roam-index-file "Index.org")
  (org-roam-title-sources '((title headline) alias))
  (org-roam-tag-sources '(vanilla))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate)))
  :config                               ;
  (require 'org-roam-protocol)
  (setq org-roam-capture-templates '(("d" "default" plain #'org-roam-capture--get-point "%?"
                                      :file-name "%<%Y%m%d%H%M%S>"
                                      :head "* ${title} \n\n"
                                      :unnarrowed t)))
  (setq org-roam-capture-immediate-template '("d" "default" plain #'org-roam-capture--get-point "%?"
                                              :file-name "%<%Y%m%d%H%M%S>"
                                              :head "* ${title} \n\n"
                                              :unnarrowed t)))

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


(provide 'major-mode/note)
