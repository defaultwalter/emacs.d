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
  :ensure t
  :defer t)

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Set config path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'core)
(require 'major-mode)

(provide 'init)
;;; init.el ends here
