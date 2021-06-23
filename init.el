;;; package -- core
;;; Commentary:
;;; Code:

(when (version< emacs-version "27")
  ;; Emacs 版本低于 27 时，手动加载‘early-init.el’
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

;;;; Package 初始化
(require 'package)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;; (if (bound-and-true-p with-dump)
;;     (setq load-path dump-load-path)
;;   (package-initialize) )
(unless package--initialized (package-initialize))

(add-to-list 'load-path (expand-file-name "lisp" user-config-directory))
(add-to-list 'load-path (expand-file-name "modal" user-config-directory))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 启用 use-package 统计
(setq use-package-compute-statistics t)
(require 'use-package)
;; 安装 straight.el
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously
                          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                          'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; 加载配置
(require 'core)
(require 'purpose)

;; (make-frame-visible)
(provide 'init)
;;; init.el ends here

