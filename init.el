;;; package -- core
;;; Commentary:
;;; Code:
(require 'package)


(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("org" . "http://elpa.emacs-china.org/org/")))

(package-initialize)
(eval-when-compile (add-to-list 'load-path (expand-file-name "use-package" user-emacs-directory))
                   (setq
                    ;; Enable package statistics
                    use-package-compute-statistics t
                    ;; Not show load message in message buffer
                    use-package-verbose nil)
                   (require 'use-package))
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
