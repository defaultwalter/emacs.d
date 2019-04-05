;;; package -- core
;;; Commentary:
;;; Code:
(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

(load-file  (expand-file-name "core/utils.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "major" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(install-package 'use-package)

(require 'core)
(require 'major)
(load-file (expand-file-name "custom.el" user-emacs-directory))
(provide 'init)
;;; init.el ends here
