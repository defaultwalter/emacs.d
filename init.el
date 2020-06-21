;;; package -- core
;;; Commentary:
;;; Code:

(when (version< emacs-version "27")
  ;; Emacs版本低于27时，手动加载‘early-init.el’
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(require 'package)
(setq package-user-dir (expand-file-name "packages" user-emacs-directory))
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("org" . "http://elpa.emacs-china.org/org/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-compute-statistics t)

(use-package
  use-package-ensure-system-package
  :ensure t)

(use-package
  exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;; Use `user.el` to save custom config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; Load custom config
(when (file-exists-p custom-file)
  (load-file custom-file))

;; Set config path
(add-to-list 'load-path (expand-file-name "w" user-emacs-directory))

(require 'core)
(require 'major-mode)

(provide 'init)
;;; init.el ends here
