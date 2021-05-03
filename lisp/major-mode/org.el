;;; package -- org-config
;;; Commentary:
;;; Code:
(use-package
  org
  :ensure org-plus-contrib
  :defer t
  :hook (org-mode . org-superstar-mode)
  :init                                 ;
  (setq org-preview-latex-image-directory (expand-file-name "ltximg/" user-emacs-directory))
  (setq org-hide-emphasis-markers nil) ; 隐藏强调符号（加粗，下划线等等）
  (setq org-pretty-entities nil)       ; 可以显示上标下标
  (setq org-ellipsis " ✚")             ;设置折叠标识
  (setq org-edit-src-content-indentation 0) ; 设置代码内容缩进
  (setq org-src-preserve-indentation nil)
  (setq org-src-tab-acts-natively t)
  ;; (setq org-fontify-done-headline t) ; 标题状态为 Done 的时候修改标题样式
  (setq org-hide-leading-stars t)       ; 隐藏标题多余的星号
  (setq org-startup-folded 'nofold)     ; 是否默认开启折叠
  (setq org-cycle-separator-lines 2)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (add-hook 'org-mode-hook (lambda ()
                             ;; (setq prettify-symbols-alist '(("#+BEGIN_SRC" . "▿")
                             ;;                                ("#+END_SRC" . "▵")
                             ;;                                ("#+begin_src" . "▿")
                             ;;                                ("#+end_src" . "▵")
                             ;;                                ("#+BEGIN_QUOTE" . "▿")
                             ;;                                ("#+END_QUOTE" . "▵")
                             ;;                                ("#+begin_quote" . "▿")
                             ;;                                ("#+end_quote" . "▵")))
                             ;; (setq prettify-symbols-unprettify-at-point 'right-edge)
                             ;; (prettify-symbols-mode 1)
                             (setq truncate-lines nil)
                             (org-display-inline-images t t) ; 显示图片
                             (org-indent-mode 1) ; 缩进模式
                             (visual-fill-column-mode 1)
                             (org-align-tags t)
                             (add-hook 'before-save-hook (lambda()
                                                           ;; 保存时 对齐 tag
                                                           (org-align-tags t)) nil 'local)))
  (setq org-image-actual-width '(100 200 300 400))
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
  visual-fill-column                    ;设置正文宽度
  :ensure t
  :defer t
  :commands (visual-fill-column-mode)
  :config                               ;
  (setq-default visual-fill-column-width 100)
  (setq-default visual-fill-column-center-text t))

(use-package
  ob-plantuml
  :init (setq-default org-plantuml-exec-mode 'plantuml)
  (setq-default org-plantuml-jar-path ""))

(use-package
  org-superstar
  :ensure t
  :defer t
  :custom                               ;
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("✿" "❖" "●" "◉" "◍" "◎" "○" "◌"))
  (org-superstar-prettify-item-bullets t)
  (org-superstar-item-bullet-alist '((?* . ?*)
                                     (?+ . ?◆)
                                     (?- . ?◈)))
  (org-superstar-special-todo-items t)
  (org-superstar-todo-bullet-alist '(("TODO" . ?☐)
                                     ("DONE" . ?☑)))
  :hook (org-mode . org-superstar-mode)
  :init                                 ;
  (setq org-superstar-prettify-item-bullets t))


(defcustom machine:note-directory (expand-file-name "notes" temporary-file-directory)
  "Note root directory"
  :type 'string
  :group 'machine)
(unless (file-directory-p machine:note-directory)
  (mkdir machine:note-directory))

(defcustom machine:note-server-host "127.0.0.1"
  "Note server host"
  :type 'string
  :group 'machine)

(defcustom machine:note-server-port 10101
  "Note server port"
  :type 'integer
  :group 'machine)


(use-package
  org-roam
  :ensure t
  :defer t
  :commands (org-roam-buffer-toggle-display org-roam-dailies-find-today org-roam-db-clear
                                            org-roam-db-build-cache)
  :hook(org-mode . org-roam-mode)
  :custom                               ;
  (org-roam-title-to-slug-function (lambda (title)
                                     (upcase (org-roam--title-to-slug title))))
  ;; (org-roam-db-update-method 'immediate)
  (org-roam-buffer "*Backlink*")
  ;; (org-roam-buffer-position 'bottom)
  ;; (org-roam-buffer-width 0.3)
  ;; (org-roam-buffer-window-parameters '((no-delete-other-windows . t)
  ;;                                      (mode-line-format "")
  ;;                                      (window-slot . 0)
  ;;                                      (window-side . bottom)))
  (org-roam-directory machine:note-directory)
  (org-roam-index-file "Index.org")
  (org-roam-dailies-directory "Journals")
  (org-roam-title-sources '(headline))
  (org-roam-tag-sources '(vanilla))
  (org-roam-capture-templates '(("d" "default" plain #'org-roam-capture--get-point "%?"
                                 :file-name "${slug}-%<%Y-%m-%d %H:%M:%S>"
                                 :head "* ${title} :Default:\n\n"
                                 :unnarrowed t)))
  (org-roam-capture-immediate-template '("d" "default" plain #'org-roam-capture--get-point "%?"
                                         :file-name "${slug}-%<%Y%m%d%H%M%S>"
                                         :head "* ${title} :Default:\n\n"
                                         :unnarrowed t
                                         :immediate-finish t))
  (org-roam-dailies-capture-templates '(("d" "default" entry #'org-roam-capture--get-point "%?"
                                         :file-name "Journals/%<%Y-%m-%d>"
                                         :head
                                         "* %<%d %B, %Y> :Journal:%<%A>:\n\n** 🍀 晨间日记\n\n*** 昨天发生的事\n\n*** 今天要做的事\n\n*** 一些想法\n\n** 🌟 随手记\n\n"
                                         :unnarrowed t)))
  :custom-face                          ;
  (org-roam-link ((t
                   (:foreground ,(color-lighten-name (face-foreground 'default) 10)
                                :inherit 'org-link))))
  (org-roam-link-current ((t
                           (:inherit 'org-roam-link))))
  :init                                 ;
  (modal-leader-set-key "n d" '(org-roam-dailies-find-today :which-key "today"))
  (modal-leader-set-key "n f" '(org-roam-find-file :which-key "find note"))
  (modal-leader-set-key "n DEL" '(org-roam-db-clear :which-key "delete cache"))
  (modal-leader-set-key "n RET" '(org-roam-db-build-cache :which-key "build cache"))
  (modal-leader-set-key-for-mode 'org-mode "n b" '(org-roam-buffer-toggle-display :which-key
                                                                                  "backlink"))
  (modal-leader-set-key-for-mode 'org-mode "n g" '(org-roam-graph :which-key "graph"))
  (modal-leader-set-key-for-mode 'org-mode "n i" '(org-roam-insert :which-key "insert node"))
  :config                               ;
  (require 'org-roam-protocol))

(use-package
  deft
  :ensure t
  :defer t
  :custom (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory machine:note-directory)
  :init                                 ;
  (modal-leader-set-key "n n" '(deft :which-key "list")))

(use-package
  org-roam-server
  :ensure t
  :defer t
  :custom                               ;
  (org-roam-server-host machine:note-server-host )
  (org-roam-server-port machine:note-server-port )
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
  (modal-leader-set-key "ns" '((lambda ()
                                 (interactive)
                                 (when (not (bound-and-true-p org-roam-server-mode))
                                   (org-roam-server-mode t))
                                 (browse-url (format "http://%s:%s" org-roam-server-host
                                                     org-roam-server-port))) :which-key "server"))
  :config )

(defcustom machine:agenda-directory (expand-file-name "agenda" temporary-file-directory)
  "Agenda root directory"
  :type 'string
  :group 'machine)
(unless (file-directory-p machine:agenda-directory)
  (mkdir machine:agenda-directory))

(unless (file-exists-p machine:agenda-directory)
  (mkdir machine:agenda-directory))
(setq org-agenda-files (mapcar (lambda (file)
                                 (expand-file-name file machine:agenda-directory))
                               (directory-files machine:agenda-directory nil ".*\.org")))
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
  )

(provide 'major-mode/org)
;;; org-config.el ends here
