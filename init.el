;;; package -- core
;;; Commentary:
;;; Code:

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
  "Binary file.")

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
