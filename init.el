(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/") 
			 ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(package-initialize)
;;

(load-file  (expand-file-name "core/utils.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "mode" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(install-package 'use-package)

(require 'core)
(require 'mode)

(load-file (expand-file-name "custom.el" user-emacs-directory))
