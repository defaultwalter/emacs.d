;;; package -- core
;;; Commentary:
;;; Code:

(when (version< emacs-version "27")
  ;; Emacs版本低于27时，手动加载‘early-init.el’
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

;;;; Const
(section
  (defconst environment/gui (display-graphic-p)
    "Are we running on a GUI Emacs?")

  (defconst environment/terminal (not (display-graphic-p))
    "Are we running on a Terminal Emacs?")
  (defconst environment/windows (eq system-type 'windows-nt)
    "Are we running on a WinTel system?")
  (defconst environment/linux (eq system-type 'gnu/linux)
    "Are we running on a GNU/Linux system?")
  (defconst environment/mac (eq system-type 'darwin)
    "Are we running on a Mac system?")
  (defconst environment/root (string-equal "root" (getenv "USER"))
    "Are you a ROOT user?")
  (defconst misc-file-directory (expand-file-name "misc" user-emacs-directory)
    "Save automatically generated files.")
  (defconst bin-file-directory (expand-file-name "bin" user-emacs-directory)
    "Binary file."))

;;;; Package初始化
(section
  (require 'package)
  (setq package-user-dir (expand-file-name "packages" user-emacs-directory))
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                           ("melpa" . "http://elpa.emacs-china.org/melpa/")
                           ("org" . "http://elpa.emacs-china.org/org/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  ;; 启用use-package统计
  (setq use-package-compute-statistics t)
  (require 'use-package)
  ;; 安装系统包
  (use-package
    use-package-ensure-system-package
    :ensure t
    :defer t))

;;;; Custom文件配置
(section
  ;; Use `user.el` to save custom config
  (setq custom-file (expand-file-name "custom.el" misc-file-directory))
  ;; Load custom config
  ;; (when (file-exists-p custom-file)
  ;;   (load-file custom-file))
  )


;;;; Machine
(section
  (setq machine-file (expand-file-name "machine.el" user-emacs-directory))
  ;; Load machine config
  (when (file-exists-p machine-file)
    (load-file machine-file)))

;;;; 加载用户配置
(section (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
         (require 'core)
         (require 'major-mode))

(provide 'init)
;;; init.el ends here
