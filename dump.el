;;; package -- core
;;; Commentary:
;;; Code:
;;; use ‘emacs --batch -q -l ~/.emacs.d/dump.el’ create dump file
;;; use ‘emacs --dump-file=path/to/emacs.dump’ load dump file

(require 'package)

(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(defconst with-dump t
  "Emacs startup with dump")
(package-initialize)
;; 由于使用dump-file启动不会设置package路径到load-path,这里先保存一下，在init.el 中手动设置。
(setq dump-load-path load-path)


(defvar dump-packages package-activated-list
  "需要dump的package")
(delete 'multi-vterm dump-packages)
(delete 'vterm dump-packages)
(delete 'vterm-toggle dump-packages)
(delete 'org-plus-contrib dump-packages)
(delete 'org-roam dump-packages)
(delete 'with-editor dump-packages)

(dolist (package dump-packages)
  (require package))

(load-theme 'doom-one t)

(add-hook 'before-init-hook (lambda()
                                (setq load-path dump-load-path)))
(add-hook 'after-init-hook (lambda()
                             ;; 使用 dump file 的时候需要手动启用下面两个mode
                             (transient-mark-mode 1)
                             (global-font-lock-mode 1)))
(dump-emacs-portable (expand-file-name "emacs.dump" user-emacs-directory))

(provide 'dump)
