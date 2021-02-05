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
  (setq org-preview-latex-image-directory (expand-file-name "ltximg/"
                                                             user-emacs-directory))
  (setq org-hide-emphasis-markers t) ; 隐藏强调符号（加粗，下划线等等）
  (setq org-pretty-entities t)       ; 可以显示上标下标
  (setq org-ellipsis " ······▾")     ;设置折叠标识
  (setq org-edit-src-content-indentation 0) ; 设置代码内容缩进
  (setq org-src-preserve-indentation nil)
  (setq org-src-tab-acts-natively t)
  ;; (setq org-fontify-done-headline t) ; 标题状态为 Done 的时候修改标题样式
  (setq org-hide-leading-stars t)       ; 隐藏标题多余的星号
  (setq org-startup-folded 'nofold)     ; 是否默认开启折叠
  (setq org-cycle-separator-lines 2)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (add-hook 'org-mode-hook (lambda ()
                             (setq truncate-lines nil)
                             (org-display-inline-images t t) ; 显示图片
                             (org-indent-mode 1) ; 缩进模式
                             (visual-fill-column-mode 1)
                             (add-hook 'before-save-hook (lambda()
                                                           ;; 保存时 对齐 tag
                                                           (org-align-tags t)) nil 'local)))
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
  :commands (org-roam org-roam-dailies-today org-roam-db-clear org-roam-db-build-cache)
  :custom                               ;
  (org-roam-buffer "*Relationship*")
  (org-roam-directory user/note-directory)
  (org-roam-index-file "Index.org")
  (org-roam-dailies-directory "Journal")
  (org-roam-title-sources '(headline))
  (org-roam-tag-sources '(vanilla))
  (org-roam-capture-templates '(("d" "default" plain #'org-roam-capture--get-point "%?"
                                 :file-name "%<%y%m%d%H%M%S>-${slug}"
                                 :head "* ${title} :Default:\n"
                                 :unnarrowed t)))
  (org-roam-capture-immediate-template '("d" "default" plain #'org-roam-capture--get-point "%?"
                                         :file-name "%<%y%m%d%H%M%S>-${slug}"
                                         :head "* ${title} :Default:\n"
                                         :unnarrowed t))
  (org-roam-dailies-capture-templates '(("d" "default" entry #'org-roam-capture--get-point "* %?"
                                         :file-name "Journal/%<%Y-%m-%d>"
                                         :head "* %<%Y/%m/%d %A> :Journal:\n")))
  :init                                 ;
  (user/leader-key "nd" '(org-roam-dailies-today :name "today"))
  (user/leader-key "nf" '(org-roam-find-file :name "find note"))
  (user/leader-key "n DEL" '(org-roam-db-clear :name ("delete" . "delete cache")))
  (user/leader-key "n RET" '(org-roam-db-build-cache :name ("return" . "build cache")))
  (user/leader-key "nv" '(org-roam :name "view" :mode org-mode))
  (user/leader-key "ng" '(org-roam-graph :name "graph" :mode org-mode))
  (user/leader-key "ni" '(org-roam-insert :name "insert node" :mode org-mode))
  :config                               ;
  (require 'org-roam-protocol))

(use-package
  deft
  :ensure t
  :defer t
  :custom (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory user/note-directory)
  :init                                 ;
  (user/leader-key "nn" '(deft :name "list")))

(use-package
  org-roam-server
  :ensure t
  :defer t
  :custom                               ;
  (org-roam-server-host user/note-server-host )
  (org-roam-server-port user/note-server-port )
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
  (user/leader-key "ns" '((lambda ()
                            (interactive)
                            (when (not (bound-and-true-p org-roam-server-mode))
                              (org-roam-server-mode t))
                            (browse-url (format "http://%s:%s" org-roam-server-host
                                                org-roam-server-port))) :name "server"))
  :config )

(setq org-agenda-files (mapcar (lambda (file)
                                 (expand-file-name file user/agenda-directory))
                               (directory-files user/agenda-directory nil ".*\.org")))
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))

(use-package
  org-tree-slide                        ; 幻灯片
  :ensure t
  :defer t
  :custom (org-tree-slide-header nil)
  :hook (org-tree-slide-mode . (lambda()
                                 (read-only-mode 1)))
  :config                               ;
  (evil-define-key 'normal 'org-tree-slide-mode-map (kbd "C-k") 'org-tree-slide-move-previous-tree)
  (evil-define-key 'normal 'org-tree-slide-mode-map (kbd "C-j") 'org-tree-slide-move-next-tree))

(provide 'major-mode/org)
;;; org-config.el ends here
