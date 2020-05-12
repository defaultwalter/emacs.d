;;; package -- core
;;; Commentary:
;;; Code:
(require 'package)


(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("org" . "http://elpa.emacs-china.org/org/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(use-package
  exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))
(use-package
  use-package-ensure-system-package
  :ensure t)
;; Use `user.el` to save custom config
(setq custom-file (expand-file-name "user.el" user-emacs-directory))
;; Load custom config
(load-file (expand-file-name "user.el" user-emacs-directory))
;; Set config path
(add-to-list 'load-path (expand-file-name "w" user-emacs-directory))

(require 'core)
(require 'major-mode)



(provide 'init)
;;; init.el ends here
