;;; package -- core
;;; Commentary:
;;; Code:
(setq-default
 tab-width 4
 ;; Use space instead tab.
 indent-tabs-mode nil
 ;; Allow delete selection.
 delete-selection-mode t indent-tabs-mode nil
 ;; Smooth scroll.
 scroll-margin 0 scroll-conservatively 10000 truncate-lines nil
 ;; Don't show splash screen.
 inhibit-splash-screen 1
 ;; Clear scratch buffer.
 initial-scratch-message nil
 ;; Disable ring bell.
 ring-bell-function 'ignore
 ;; Not show confrim dialog.
 use-dialog-box nil
 ;; Enable backup
 backup-by-copying t
 version-control t
 ;; Auto delete backup file
 delete-old-versions t
 backup-directory-alist `((".*" . "~/.emacs.d/cache/backup_files/")) ;
 auto-save-file-name-transforms `((".*" "~/.emacs.d/cache/backup_files/" t))
 auto-save-list-file-prefix   "~/.emacs.d/cache/backup_files/saves-")

;; (global-whitespace-mode)
(menu-bar-mode -1)
(recentf-mode t)
;; Highlight match paren.
(show-paren-mode t)
;; Auto reload file.
(global-auto-revert-mode t)
(global-hl-line-mode t)

;; (cond ((fboundp 'pixel-scroll-mode)
;;        (pixel-scroll-mode)))

(fset 'yes-or-no-p 'y-or-n-p)

;; New line auto indent.
(global-set-key (kbd "RET") 'newline-and-indent)


(define-fringe-bitmap 'right-curly-arrow [#b00000000
                                          #b00000000
                                          #b00101010
                                          #b00000000
                                          #b00001010
                                          #b00000000
                                          #b00000010
                                          #b00000000])
(define-fringe-bitmap 'left-curly-arrow [#b00000000
                                         #b00000000
                                         #b01000000
                                         #b00000000
                                         #b01010000
                                         #b00000000
                                         #b01010100
                                         #b00000000])
(set-face-attribute 'fringe nil
                    :foreground "#fc5c59"
                    :background (face-background 'default))

(add-hook 'prog-mode-hook (lambda()
                            (linum-mode 1)))

(defun set-gui-font()
  "Set gui font."
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

;; If started by gui mode.
(if window-system (progn (setq-default
                          ;; Set window title .
                          frame-title-format "emacs"
                          ;; Set mouse wheel speed.
                          mouse-wheel-progressive-speed nil)
                         ;; Set window transparent.
                         (setf (frame-parameter nil 'alpha) 98)
                         ;; (desktop-save-mode 1)
                         ;; Disable tool bar.
                         (tool-bar-mode -1)
                         ;; Disabe scroll bar.
                         (scroll-bar-mode -1)
                         ;; Set window placment.
                         ;; (set-face-attribute 'default nil
                         ;;                     :height 120)
                         ;; (set-frame-width (selected-frame) 160)
                         ;; (set-frame-height (selected-frame) 40)
                         (set-gui-font)
                         ;; Emacs-plus transparent of title bar.
                         (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
                         (add-to-list 'default-frame-alist '(ns-appearance . dark))) t)

(provide 'default)
;;; default.el ends here
