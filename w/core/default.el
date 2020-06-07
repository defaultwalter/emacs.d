;;; package -- default
;;; Commentary:
;;; Code:

;;;; ==============================================
;;;; 用户信息
;;;; ==============================================

(setq user-full-name "walter")
(setq user-mail-address "ismrwalter@gmail.com")

;;;; ==============================================
;;;; 基础设置
;;;; ==============================================


(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (set-frame-parameter nil 'internal-border-width 10) ; 设置窗口边距
  (set-frame-parameter nil 'alpha 99)                 ;设置窗口透明度
  (w/set-font)                                        ;设置字体
  (w/set-wrap-fringe)                   ;设置自动换行标识
  )
(setq shift-select-mode nil                     ; 禁止双击 shift 选择
      load-prefer-newer t                       ; 加载最新的文件
      ring-bell-function 'ignore                ; 关闭光标警告
      echo-keystrokes 0.1                       ; 显示未完成的按键命令
      scroll-step 1 scroll-conservatively 10000 ; 滚动时保持光标位置
      )


(setq-default tab-width 4                      ; Tab Width
              indent-tabs-mode nil             ; 使用空格代替 Tab
              delete-selection-mode t          ; 允许删除选区
              select-enable-clipboard t        ; 启用系统剪切板
              create-lockfiles nil             ; 不要锁定文件
              history-length 500               ; Minibuffer 历史长度
              enable-recursive-minibuffers nil ; 禁止递归 Minibuffer
              ;; keep the point out of the minibuffer
              minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face
                                                       minibuffer-prompt)
              mouse-wheel-progressive-speed nil ; 设置鼠标滚动速度
              inhibit-splash-screen 1           ; 不显示启动屏幕
              initial-scratch-message nil  ; 将 Scratch 的内容设为空
              use-dialog-box nil           ; 不现实对话框
              frame-title-format "[%m] %f" ; 设置表示格式
              )

;; (desktop-save-mode 1)                 ;自动保存环境
(save-place-mode 1)                   ; 当 buffer 关闭后，保存光标位置
(delete-selection-mode 1)             ; 插入时替换选区
(global-auto-revert-mode t)           ; 开启自动恢复
(auto-compression-mode t)             ; 压缩文件支持
(recentf-mode t)                      ; 开启最近访问的文件
(global-hl-line-mode t)               ; 高亮当前的行

;; 保存文件时移除空格
(add-hook 'before-save-hook
          (lambda()
            (whitespace-cleanup)
            (delete-trailing-whitespace)))
(fset 'yes-or-no-p 'y-or-n-p)           ; 用 y/n 代替 yes/no

;; 换行时自动排版
(global-set-key (kbd "RET") 'newline-and-indent)

;; Unset file name handler alist.
(eval-and-compile
  (defun reset-file-name-handler-alist ()
    (setq file-name-handler-alist orig-file-name-handler-alist))
  (defvar orig-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook 'reset-file-name-handler-alist))
(setq exec-path-from-shell-check-startup-files nil)


;;;; ==============================================
;;;; 备份
;;;; ==============================================
(make-directory "~/.emacs.d/autosaves" t)
(make-directory "~/.emacs.d/backups" t)
(setq
 ;; 启动备份版本控制
 version-control t
 ;; 创建版本控制备份文件
 vc-make-backup-files t
 ;; 删除旧的版本
 delete-old-versions -1
 ;; 设置备份目录
 backup-directory-alist '(("." . "~/.emacs.d/backups/"))
 ;; 设置自动保存文件名
 auto-save-file-name-transforms  '((".*" "~/.emacs.d/autosaves/\\1" t)))


;; ==============================================
;; 字符
;; ==============================================
(prefer-coding-system 'utf-8-unix)
(set-language-environment "English")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)



;;;; ==============================================
;;;; linum mode
;;;; ==============================================
(setq linum-format "%4d ")              ;设置行号格式
(add-hook 'linum-mode-hook
          (lambda()
            (set-face-attribute 'linum nil
                                :foreground "#6f747a")))

;;;; ==============================================
;;;; dired mode
;;;; ==============================================
(setq dired-recursive-deletes 'always)       ;递归删除
(setq dired-recursive-copies 'always)        ;递归复制
(setq global-auto-revert-non-file-buffers t) ;自动刷新
(setq auto-revert-verbose nil)               ;不显示多余的信息
(setq dired-dwim-target t)                   ;快速复制和移动
(setq delete-by-moving-to-trash t)           ;删除文件到trash
(put 'dired-find-alternate-file 'disabled nil) ;重新使用已经存在dired buffer
(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") #'dired-find-alternate-file)
            (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)))




;; ==============================================
;; 垃圾回收
;; ==============================================
;; Avoid garbage collection during startup.
(eval-and-compile
  (defun revert-gc ()
    (setq gc-cons-threshold 16777216 gc-cons-percentage 0.1))
  (setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
  (add-hook 'emacs-startup-hook 'revert-gc))
;; Garbage collect when Emacs is not in focus.
(add-hook 'focus-out-hook #'garbage-collect)


;; if system is macOS
(when (eq system-type 'darwin)
  ;; Emacs-plus transparent of title bar.
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(provide 'core/default)
;;; default.el ends here
