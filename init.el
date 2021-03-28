;;; package -- core
;;; Commentary:
;;; Code:

(when (version< emacs-version "27")
  ;; Emacs版本低于27时，手动加载‘early-init.el’
  (load-file (expand-file-name "early-init.el" user-config-directory)))

;;;; Package初始化
(require 'package)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(if (bound-and-true-p user/with-dump)
    (setq load-path user/dump-load-path)
  (package-initialize) )
(add-to-list 'load-path (expand-file-name "lisp" user-config-directory))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 启用use-package统计
(setq use-package-compute-statistics t)
(require 'use-package)

;;; 加载配置
(require 'core)
(require 'major-mode)

;; (make-frame-visible)
(provide 'init)
;;; init.el ends here
