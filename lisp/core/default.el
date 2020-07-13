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


(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(when environment/gui
  ;; (tooltip-mode -1)
  ;; (set-frame-parameter nil 'internal-border-width 10) ; 设置窗口边距
  (set-frame-parameter nil 'alpha 99)   ;设置窗口透明度
  (defun set-font (font size)
    "Set font"
    (set-face-attribute 'default nil
                        :font (format   "%s:pixelsize=%d" font size))
    ;; chinese font
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family font))))
  (when environment/mac (set-font "Sarasa Mono SC" 16))
  (when environment/linux (set-font "Sarasa Mono SC" 20))
                                        ;设置字体

  (defconst right-arrow-bitmap
    [#b00000000                         ;
     #b00000000                         ;
     #b00000000                         ;
     #b00000000                         ;
     #b00000000                         ;
     #b00000000                         ;
     #b00000000                         ;
     #b00000000                         ;
     #b00000000                         ;
     #b00000000                         ;
     #b00000000                         ;
     #b00000000                         ;
     #b00000000                         ;
     #b00101010                         ;
     #b00000000                         ;
     #b00000010                         ;
     #b00000000                         ;
     #b00000010                         ;
     #b00000000                         ;
     #b00000010                         ;
     #b00000000                         ;
     #b00010010                         ;
     #b00100000                         ;
     #b01111110                         ;
     #b00000000                         ;
     #b00000000])

  (defconst left-arrow-bitmap
    [#b00000000                         ;
     #b00000000                         ;
     #b00000000                         ;
     #b00000000                         ;
     #b00000000                         ;
     #b00000000                         ;
     #b00000000                         ;
     #b00000000])
  ;;设置自动换行标识
  (define-fringe-bitmap 'right-curly-arrow right-arrow-bitmap)
  (define-fringe-bitmap 'left-curly-arrow left-arrow-bitmap)  )
(setq shift-select-mode nil             ; 禁止双击 shift 选择
      load-prefer-newer t               ; 加载最新的文件
      ring-bell-function 'ignore        ; 关闭光标警告
      echo-keystrokes 0.1               ; 显示未完成的按键命令
      scroll-conservatively 10000       ; 滚动时保持光标位置
      scroll-step 1                     ;滚动行数/列数
      hscroll-step 1 scroll-margin 5    ;滚动光标边距
      hscroll-margin 10)

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

;; 设置Emacs每次从进程读取的最大数据量
(setq read-process-output-max (* 1024 1024))
;; (desktop-save-mode 1)                 ;自动保存环境

(setq save-place-file (expand-file-name "palces" misc-file-directory))
(save-place-mode 1)                   ; 当 buffer 关闭后，保存光标位置
(delete-selection-mode 1)             ; 插入时替换选区
(global-auto-revert-mode t)           ; 开启自动恢复
(auto-compression-mode t)             ; 压缩文件支持
(setq recentf-save-file (expand-file-name "recentf" misc-file-directory))
(recentf-mode 1)                        ; 开启最近访问的文件
(global-hl-line-mode t)                 ; 高亮当前的行

;; 保存文件时移除空格
(add-hook 'before-save-hook (lambda()
                              (whitespace-cleanup)
                              (delete-trailing-whitespace)))
(fset 'yes-or-no-p 'y-or-n-p)           ; 用 y/n 代替 yes/no

(electric-pair-mode t)
(electric-quote-mode t)
;; 换行时自动排版
(global-set-key (kbd "RET") 'newline-and-indent)

;;;; ==============================================
;;;; 备份
;;;; ==============================================
(defconst backup-file-directory (expand-file-name "backup" misc-file-directory))
(make-directory backup-file-directory t)

;; 启动备份版本控制
(setq version-control t)
;; 创建版本控制备份文件
(setq vc-make-backup-files t)
;; 删除旧的版本
(setq delete-old-versions -1)
;; 设置备份目录
(setq backup-directory-alist `(("." . ,backup-file-directory)))
;; 设置自动保存文件名
(setq auto-save-file-name-transforms  `((".*" ,backup-file-directory t)))
(setq auto-save-list-file-prefix backup-file-directory)

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
(add-hook 'linum-mode-hook (lambda()
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
(with-eval-after-load 'dired (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
                      (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
                      (define-key dired-mode-map (kbd "^")
                        (lambda ()
                          (interactive)
                          (find-alternate-file ".."))))

;; 自动关闭minibuffer
(add-hook 'mouse-leave-buffer-hook (lambda()
                                     (when (and (>= (recursion-depth) 1)
                                                (active-minibuffer-window))
                                       (abort-recursive-edit))))


;; Use `user.el` to save custom config
(setq custom-file (expand-file-name "custom.el" misc-file-directory))
;; Load custom config
(when (file-exists-p custom-file)
  (load-file custom-file))

(setq ad-redefinition-action 'accept)

(provide 'core/default)
;;; default.el ends here
