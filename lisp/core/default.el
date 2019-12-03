;;; package -- default
;;; Commentary:
;;; Code:

(require 'core/utils)

;; ==============================================
;; set default userinfo
;; ==============================================

(setq user-full-name "baiyan")
(setq user-mail-address "oobaiyan@gmail.com")
;; ==============================================
;; interface
;; ==============================================
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(setq-default frame-title-format "[%m] %f")

(when window-system (set-gui-font)
      (set-frame-parameter nil 'alpha 90)
     
      ;; When buffer is closed, saves the cursor location
      (save-place-mode 1)

      ;; Custom fringe
      (define-fringe-bitmap 'right-curly-arrow [#b00011111 #b00000001 #b00000001 #b00100001
                                                           #b01100001 #b11111111 #b01100000
                                                           #b00100000])
      (define-fringe-bitmap 'left-curly-arrow [#b11111000 #b10000000 #b10000000 #b10000100
                                                          #b10000110 #b11111111 #b00000110
                                                          #b00000100])
      (set-face-attribute 'fringe nil
                          :foreground "#fc5c59"
                          :background (face-background 'default))
      ;; (message "%s" system-type)
      (when (eq system-type 'darwin)
        ;; Emacs-plus transparent of title bar.
        (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
        (add-to-list 'default-frame-alist '(ns-appearance . dark))))

(fset 'yes-or-no-p 'y-or-n-p)

;; New line auto indent.
(global-set-key (kbd "RET") 'newline-and-indent)

(setq
 ;; Disable Shift mark
 shift-select-mode nil
 ;; Load the newest version of a file
 load-prefer-newer t
 ;; Turn Off Cursor Alarms
 ring-bell-function 'ignore
 ;; Show Keystrokes in Progress Instantly
 echo-keystrokes 0.1 version-control t vc-make-backup-files t)


(setq-default
 ;; Tab Width
 tab-width 2
 ;; Use space instead tab.
 indent-tabs-mode nil
 ;; Allow delete selection.
 delete-selection-mode t
 ;; Merge system clipboard with Emacs
 select-enable-clipboard t
 ;; Don't Lock Files
 create-lockfiles nil
 ;; Longer History Length
 history-length 500
 ;; Better compilation configurations.
 compilation-always-kill t compilation-ask-about-save nil compilation-scroll-output t
 ;; disable recursive minibuffers
 enable-recursive-minibuffers nil
 ;; keep the point out of the minibuffer
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face
                                          minibuffer-prompt)
 ;; Don't show splash screen.
 inhibit-splash-screen 1
 ;; Clear scratch buffer.
 initial-scratch-message nil
 ;; Not show confrim dialog.
 use-dialog-box nil
 ;; Set mouse wheel speed.
 mouse-wheel-progressive-speed nil)

;; Replace selection on insert
(delete-selection-mode 1)
;; Detect external file changes and auto refresh file
(global-auto-revert-mode t)
;; Transparently open compressed files
(auto-compression-mode t)

(recentf-mode t)
(global-auto-revert-mode t)
(global-hl-line-mode t)

;; Remove useless whitespace before saving a file
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda()
                              (delete-trailing-whitespace)))
(add-hook 'minibuffer-setup-hook
      (lambda () (setq truncate-lines nil)))

;; Unset file name handler alist.
(eval-and-compile
  (defun reset-file-name-handler-alist ()
    (setq file-name-handler-alist orig-file-name-handler-alist))
  (defvar orig-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook 'reset-file-name-handler-alist))
(setq exec-path-from-shell-check-startup-files nil)







;; ==============================================
;; Autosave and Backup
;; ==============================================
(make-directory "~/.emacs.d/autosaves" t)
(make-directory "~/.emacs.d/backups" t)
(setq
 ;; Backup directory
 backup-directory-alist '(("." . "~/.emacs.d/backups/"))
 ;; Auto save file name
 auto-save-file-name-transforms  '((".*" "~/.emacs.d/autosaves/\\1" t))
 ;; Delete old version
 delete-old-versions -1 )


;; ==============================================
;; Charset
;; ==============================================
(prefer-coding-system 'utf-8-unix)
(set-language-environment "English")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)


;; ==============================================
;; Garbage collect
;; ==============================================
;; Avoid garbage collection during startup.
(eval-and-compile
  (defun revert-gc ()
    (setq gc-cons-threshold 16777216 gc-cons-percentage 0.1))
  (setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
  (add-hook 'emacs-startup-hook 'revert-gc))
;; Garbage collect when Emacs is not in focus.
(add-hook 'focus-out-hook #'garbage-collect)

(provide 'core/default)
;;; default.el ends here
