;;; package -- core
;;; Commentary:
;;; Code:
(require 'package)


(setq package-archives '
    (
        ("gnu"   . "http://elpa.emacs-china.org/gnu/")
        ("melpa" . "http://elpa.emacs-china.org/melpa/")
        ("org" . "http://elpa.emacs-china.org/org/")))

(package-initialize)

(load-file
    (expand-file-name "custom.el" user-emacs-directory))
(setq custom-file
    (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path
    (expand-file-name "lisp" user-emacs-directory))

(load (concat user-emacs-directory "lisp/core")
      nil 'nomessage)

(require 'global)
(require 'lang)
;; (require 'wm)
(provide 'init)
;;; init.el ends here
