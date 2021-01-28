;;; package -- core
;;; Commentary:
;;; Code:
;;; use ‘emacs --batch -q -l ~/.emacs.d/dump.e’ create dump file
;;; use ‘emacs --dump-file=$HOME/.cache/emacs/emacs.dump’ load dump file

(require 'package)

(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(defconst user/with-dump t
  "Emacs startup with dump")
(package-initialize)
;; 由于使用dump-file启动不会设置package路径到load-path,这里先保存一下，在init.el 中手动设置。
(setq user/dump-load-path load-path)


(defvar user/dump-packages package-activated-list
  "需要dump的package")
(delete 'multi-vterm user/dump-packages)
(delete 'vterm user/dump-packages)
(delete 'org-plus-contrib user/dump-packages)
;; (defvar user/dump-packages
;;   '(use-package
;;      ivy
;;      counsel
;;      evil
;;      general
;;      which-key
;;      doom-themes
;;      doom-modeline)
;;   "需要dump的package")


(setq evil-want-keybinding nil)
(dolist (package user/dump-packages)
  (require package))

(load-theme 'doom-one t)
(doom-modeline-mode 1)
(add-hook 'after-init-hook (lambda()
                             ;; 使用 dump file 的时候需要手动启用下面两个mode
                             (transient-mark-mode 1)
                             (global-font-lock-mode 1)))
(dump-emacs-portable (expand-file-name "emacs.dump" user-emacs-directory))

(provide 'dump)
