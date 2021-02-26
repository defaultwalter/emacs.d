;;; package -- core
;;; Commentary:
;;; Code:

(when (version< emacs-version "27")
  ;; Emacs版本低于27时，手动加载‘early-init.el’
  (load-file (expand-file-name "early-init.el" user-config-directory)))

;;;; Package初始化
(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("org" . "http://elpa.emacs-china.org/org/")))

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

(use-package
  gcmh                                  ; 优化GC
  :ensure t
  :config (gcmh-mode 1))

(use-package
  restart-emacs
  :ensure t
  :disabled)

;; 性能统计
(use-package
  benchmark-init
  :ensure t
  :disabled
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; 安装系统包
(use-package
  use-package-ensure-system-package
  :ensure t
  :defer t)

;;;; 加载配置

(require 'core)
(require 'major-mode)

;; (make-frame-visible)
(provide 'init)
;;; init.el ends here
