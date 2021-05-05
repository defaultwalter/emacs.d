;;; package -- default
;;; Commentary:
;;; Code:


;;;; 用户信息

;;;;==================================================
;;;;用户信息
;;;;==================================================
(setq user-full-name "meetcw")
(setq user-mail-address "meetcw@outlook.com")

;;;;==================================================
;;;; Emacs行为
;;;;==================================================

(setq default-directory "~/")

;; UI控件设置
(if (and (eq system-type 'darwin)
         (display-graphic-p))
    (menu-bar-mode 1)
  (menu-bar-mode -1))                    ; 隐藏菜单
(tool-bar-mode -1)                       ; 隐藏工具栏
(scroll-bar-mode -1)                     ; 隐藏滚动条
(setq mouse-highlight nil)               ; 禁止鼠标悬浮高亮
(setq-default frame-title-format " %b ") ; 设置窗口标题格式
(setq-default use-dialog-box nil)        ; 不显示对话框
(set-frame-parameter nil 'alpha 100)     ; 设置窗口透明度
;; (cua-mode 1)                             ; 开启CUA模式
;; 开启窗口UNDO-REDO
(winner-mode t)
;; 编辑备份设置

(defconst backup-file-directory (expand-file-name "backup" user-emacs-directory))
(make-directory backup-file-directory t) ; 创建备份文件目录
(setq backup-directory-alist `(("." . ,backup-file-directory))) ; 设置备份目录
(setq version-control t)                ; 启动备份版本控制
(setq vc-make-backup-files t)           ; 创建版本控制备份文件
(setq delete-old-versions -1)           ; 删除旧的版本
;; 自动保存设置
(setq auto-save-file-name-transforms  `((".*" ,backup-file-directory t))) ;自动保存命名规则
(setq auto-save-list-file-prefix backup-file-directory) ;自动保存文件路径

(setq ring-bell-function 'ignore)         ; 关闭光标警告
(setq shift-select-mode nil)              ; 禁止双击 shift 选择
;; 启动屏幕
(setq-default inhibit-splash-screen 1)    ; 不显示启动屏幕
(setq-default initial-scratch-message "") ; 将 Scratch 的内容设为空

(setq read-process-output-max (* 1024 1024)) ; 设置Emacs每次从进程读取的最大数据量
(fset 'yes-or-no-p 'y-or-n-p)                ; 用 y/n 代替 yes/no
;; (desktop-save-mode 1)                 ; 自动保存环境
(setq ad-redefinition-action 'accept)   ; 禁止redefine warning

;; 保存光标位置
(save-place-mode 1)

;; 保存最近打开的文件
(recentf-mode 1)

;; 滚动行为
(setq scroll-conservatively 10000)               ; 滚动时保持光标位置
(setq scroll-step 1)                             ; 滚动行数
(setq scroll-margin 1)                           ; 滚动光标边距
(setq hscroll-step 1)                            ; 水平滚动列数
(setq hscroll-margin 4)                          ; 水平滚动光标边距
(setq-default mouse-wheel-progressive-speed nil) ; 设置鼠标滚动速度

;; Minibuffer
(savehist-mode 1)                       ; 开启Minibuffer历史记录
(setq echo-keystrokes 0.1)              ; 显示未完成的按键命令
(setq-default history-length 500)       ; Minibuffer 历史长度
(setq-default enable-recursive-minibuffers nil) ; 禁止递归 Minibuffer
;; (setq-default minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt
;;                                                        face))
;; 自动关闭minibuffer
(add-hook 'mouse-leave-buffer-hook (lambda()
(when (and (>= (recursion-depth) 1)
(active-minibuffer-window))
(abort-recursive-edit))))
(setq tramp-default-method "ssh")       ; tramp 默认使用 ssh

(defface ellipsis-face '((t :inherit (font-lock-comment-face default)))
  "Face for ellipsis.")
(defface ellipsis-space-face '((t :inherit (font-lock-comment-face default)))
  "Face for ellipsis space char.")
;; 设置省略符号样式
(set-display-table-slot  standard-display-table 'selective-display (vconcat (mapcar (lambda (c)
                                                                                      (if (eq c 32)
                                                                                          (make-glyph-code
                                                                                           c
                                                                                           'ellipsis-space-face)
                                                                                        (make-glyph-code
                                                                                         c
                                                                                         'ellipsis-face)))
                                                                                    " [••••••] ")))

;; 显示当前所在的括号
(show-paren-mode 1)
(setq-default electric-pair-inhibit-predicate 'electric-pair-default-inhibit) ; 抑制策略
(electric-pair-mode 1)

(defcustom machine:fontsize 12
  "Font size"
  :type 'integer
  :group 'machine)


(defun update-font()
  "Try to config font"
  ;; 设置默认字体
  (let ((frame-font (cond ((member "Source Code Pro" (font-family-list)) "Source Code Pro")
                          ((member "Sarasa Mono SC" (font-family-list)) "Sarasa Mono SC")
                          ((member "Consolas" (font-family-list)) "Consolas")
                          ((member "Menlo" (font-family-list)) "Menlo")
                          ((member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono")
                          ((member "WenQuanYi Micro Hei Mono" (font-family-list))
                           "WenQuanYi Micro Hei Mono"))))
    (set-frame-font (format "%s-%s" frame-font machine:fontsize) t t))

  ;; 中文字体
  (set-fontset-font t 'han (cond ((member "Sarasa Mono SC" (font-family-list)) "Sarasa Mono SC")
                                 ((member "WenQuanYi Micro Hei Mono" (font-family-list))
                                  "WenQuanYi Micro Hei Mono")
                                 ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
                                 ((member "Hei" (font-family-list)) "Hei")
                                 ((member "WenQuanYi Micro Hei Mono" (font-family-list))
                                  "WenQuanYi Micro Hei Mono")))
  ;; 设置Emoji字体
  (set-fontset-font t '(#x1f300 . #x1fad0)
                    (cond ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
                          ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
                          ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
                          ((member "Symbola" (font-family-list)) "Symbola")
                          ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji"))))
(update-font)
;; C/S 模式需要再次设置字体
(add-hook 'server-after-make-frame-hook 'update-font)

;; 主题样式
(defun update-gui()
  "调整主题样式"
  (if (and (eq system-type 'darwin)
           (display-graphic-p))
      (menu-bar-mode 1)
    (menu-bar-mode -1))
  ;; 设置垂直窗口边框(目前发现只在终端有效)
  (set-face-inverse-video-p 'vertical-border nil)
  (set-face-background 'vertical-border (face-background 'default))
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?┊))
  (when (display-graphic-p)
    (set-face-foreground 'vertical-border (face-background 'default)))
  ;; Modeline
  (set-face-background 'mode-line-inactive nil)
  (set-face-background 'mode-line nil)
  (setq underline-minimum-offset 5))

(add-hook 'after-init-hook 'update-gui)
(add-hook 'server-after-make-frame-hook 'update-gui)
;; 自动结束子进程
(setq confirm-kill-processes nil)
;;;;==================================================
;;;; 编辑行为
;;;;==================================================
;; TAB 设置
(setq-default tab-width 4)              ; Tab Width
(setq-default indent-tabs-mode nil)     ; 关闭 Tab 缩进
(setq-default tab-always-indent t)      ; Indent 行为

(setq-default select-enable-clipboard t) ; 启用系统剪切板

(setq display-line-numbers-type t)                    ; 行号类型
(setq-default display-line-numbers-width 5)           ; 行号宽度
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ; 显示行号
;; (add-hook 'text-mode-hook 'display-line-numbers-mode) ; 显示行号
(add-hook 'conf-mode-hook 'display-line-numbers-mode) ; 显示行号
;; (add-hook 'prog-mode-hook 'hl-line-mode)              ; 高亮当前行
;; (add-hook 'text-mode-hook 'hl-line-mode)              ; 高亮当前行
;; (add-hook 'conf-mode-hook 'hl-line-mode) ; 高亮当前行
(delete-selection-mode 1)               ; 插入时替换选区

(setq-default truncate-lines nil)

(add-hook 'before-save-hook '(lambda ()
                               (when (derived-mode-p 'prog-mode)
                                 (whitespace-cleanup)
                                 (delete-trailing-whitespace))))

;;;;==================================================
;;;; 文件
;;;;==================================================
(setq vc-follow-symlinks t)             ; 跟踪符号链接
(setq load-prefer-newer t)              ; 加载最新的文件
(setq-default create-lockfiles nil)     ; 不要锁定文件
(global-auto-revert-mode t)             ; 开启重新加载被修改的文件
(auto-compression-mode t)               ; 压缩文件支持
(global-so-long-mode 1) ; 打开长行文件时可能会导致严重的性能问题，可以自动关闭一些可能会导致这一问题的功能

;; 字符编码
;; (prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
;; 优化 Dired mode
(setq dired-recursive-deletes 'always)       ;递归删除
(setq dired-recursive-copies 'always)        ;递归复制
(setq global-auto-revert-non-file-buffers t) ;自动刷新
(setq auto-revert-verbose nil)               ;不显示多余的信息
(setq dired-dwim-target t)                   ;快速复制和移动
(setq delete-by-moving-to-trash t)           ;删除文件到trash
(put 'dired-find-alternate-file 'disabled nil) ;重新使用已经存在dired buffer
(with-eval-after-load 'dired (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
                      (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
                      (define-key dired-mode-map (kbd "^")
                        (lambda ()
                          (interactive)
                          (find-alternate-file ".."))))

(provide 'core/default)
;;; default.el ends here
