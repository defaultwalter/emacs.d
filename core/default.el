;;; package -- core
;;; Commentary:
;;; Code:

;; ==============================================
;; Owner Information
;; ==============================================
(setq user-full-name "baiyan")
(setq user-mail-address "oobaiyan@gmail.com")
;; ==============================================
;; Interface Config
;; ==============================================
;; Disable Unnecessary Interface
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
;; (menu-bar-mode   -1)

;; Set window title .
(setq-default frame-title-format "[%m] %f")
(defun set-gui-font()
  "Set gui font."

  (set-fontset-font t nil (font-spec :family "Apple Color Emoji") nil 'append)
  (set-face-attribute 'default nil
                      :font (font-spec :name
                                       "-*-Source Code Pro-ultrabold-italic-normal-*-*-*-*-*-m-0-iso10646-1"
                                       :weight 'normal
                                       :slant 'normal
                                       :size 18))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset (font-spec :name
                                                                     "-*-PingFang SC-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1"
                                                                     :weight 'normal
                                                                     :slant 'normal
                                                                     :size 18))))
(if window-system (set-gui-font))

(setf (frame-parameter nil 'alpha) 99)
;; (set-frame-width (selected-frame) 120)
;; (set-frame-height (selected-frame) 40)
;; Emacs-plus transparent of title bar.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;; Custom fringe
(define-fringe-bitmap 'right-curly-arrow [#b00011111
                                          #b00000001
                                          #b00000001
                                          #b00100001
                                          #b01100001
                                          #b11111111
                                          #b01100000
                                          #b00100000])
(define-fringe-bitmap 'left-curly-arrow [#b11111000
                                         #b10000000
                                         #b10000000
                                         #b10000100
                                         #b10000110
                                         #b11111111
                                         #b00000110
                                         #b00000100])
(set-face-attribute 'fringe nil
                    :foreground "#fc5c59"
                    :background (face-background 'default))
;; ==============================================
;; Behaviour Config
;; ==============================================

(setq scroll-step 1
      scroll-conservatively  10000)
(desktop-save-mode t)
;; Remove useless whitespace before saving a file
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))
;; Make sentences end with a single space
(setq-default sentence-end-double-space nil)
;; When buffer is closed, saves the cursor location
(save-place-mode 1)
;; Disable Shift mark
(setq shift-select-mode nil)
;; Replace selection on insert
(delete-selection-mode 1)
;; Merge system clipboard with Emacs
(setq-default select-enable-clipboard t)
;; Autosave and Backup
(make-directory "~/.emacs.d/autosaves" t)
(make-directory "~/.emacs.d/backups" t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      auto-save-file-name-transforms  '((".*" "~/.emacs.d/autosaves/\\1" t))
      delete-old-versions -1
      version-control t
      vc-make-backup-files t)
;; File Configs
;; Load the newest version of a file
(setq load-prefer-newer t)
;; Detect external file changes and auto refresh file
(global-auto-revert-mode t)
;; Transparently open compressed files
(auto-compression-mode t)
;; Turn Off Cursor Alarms
(setq ring-bell-function 'ignore)
;; Show Keystrokes in Progress Instantly
(setq echo-keystrokes 0.1)
;; Don't Lock Files
(setq-default create-lockfiles nil)
;; Longer History Length
(setq-default history-length 500)
;; Better compilation configurations.
(setq-default compilation-always-kill t) ; kill compilation process before starting another
(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'
(setq-default compilation-scroll-output t)
;; disable recursive minibuffers
(setq-default enable-recursive-minibuffers nil)
;; keep the point out of the minibuffer
(setq-default minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
;; Tab Width
(setq-default tab-width 2)
;; Use space instead tab.
(setq-default indent-tabs-mode nil)
;; Allow delete selection.
(setq-default delete-selection-mode t)
;; Don't show splash screen.
(setq-default inhibit-splash-screen 1)
;; Clear scratch buffer.
(setq-default initial-scratch-message nil)
;; Unset file name handler alist.
(eval-and-compile
  (defun reset-file-name-handler-alist ()
    (setq file-name-handler-alist orig-file-name-handler-alist))

  (defvar orig-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)

  (add-hook 'emacs-startup-hook 'reset-file-name-handler-alist))
(setq exec-path-from-shell-check-startup-files nil)
(recentf-mode t)
(global-auto-revert-mode t)
(global-hl-line-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
;; Not show confrim dialog.
(setq-default use-dialog-box nil)
;; New line auto indent.
(global-set-key (kbd "RET") 'newline-and-indent)
;; Set mouse wheel speed.
(setq-default mouse-wheel-progressive-speed nil)
;; ==============================================
;; Garbage Config
;; ==============================================
;; Avoid garbage collection during startup.
(eval-and-compile
  (defun revert-gc ()
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1))

  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6)

  (add-hook 'emacs-startup-hook 'revert-gc))
;; Garbage collect when Emacs is not in focus.
(add-hook 'focus-out-hook #'garbage-collect)

;; ==============================================
;; Other Config
;; ==============================================
;; Auto Package Update
(use-package
  auto-package-update
  :ensure t
  :config
  (setq auto-package-update-interval 7) ;; in days
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Dired
(use-package dired
  :ensure nil
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; Auto refresh Dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  ;; Quickly copy/move file in Dired
  (setq dired-dwim-target t)

  ;; Move files to trash when deleting
  (setq delete-by-moving-to-trash t)

  ;; Reuse same dired buffer, so doesn't create new buffer each time
  (put 'dired-find-alternate-file 'disabled nil)
  (add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)))
  (add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "RET") #'dired-find-alternate-file)))
  (add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "^")
                                          (lambda () (interactive) (find-alternate-file "..")))
                               (dired-hide-details-mode t))))


;; Coding System
(if (eq system-type 'windows-nt)
    (progn
      (set-clipboard-coding-system 'utf-16-le)
      (set-selection-coding-system 'utf-16-le))
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(provide 'default)
;;; default.el ends here
