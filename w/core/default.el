;;; package -- default
;;; Commentary:
;;; Code:

;; ==============================================
;; 用户信息
;; ==============================================

(setq user-full-name "baiyan")
(setq user-mail-address "oobaiyan@gmail.com")

;; ==============================================
;; 基础设置
;; ==============================================
(setq
 ;; 禁止双击 shift 选择
 shift-select-mode nil
 ;; 加载最新的文件
 load-prefer-newer t
 ;; 关闭光标警告
 ring-bell-function 'ignore
 ;; 显示未完成的按键命令
 echo-keystrokes 0.1
 ;; 滚动时保持光标位置
 scroll-step 1 scroll-conservatively 10000)


(setq-default
 ;; Tab Width
 tab-width 4
 ;; 使用空格代替 Tab
 indent-tabs-mode nil
 ;; 允许删除选区
 delete-selection-mode t
 ;; 启用系统剪切板
 select-enable-clipboard t
 ;; 不要锁定文件
 create-lockfiles nil
 ;; Minibuffer 历史长度
 history-length 500
 ;;  ;; 编译选项
 ;;  compilation-always-kill t
 ;;  ;;
 ;;  compilation-ask-about-save nil
 ;;  ;;
 ;;  compilation-scroll-output t
 ;; 禁止递归 Minibuffer
 enable-recursive-minibuffers nil
 ;; keep the point out of the minibuffer
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face
                                          minibuffer-prompt)
 ;; 设置鼠标滚动速度
 mouse-wheel-progressive-speed nil)

;; 当 buffer 关闭后，保存光标位置
(save-place-mode 1)
;; 插入时替换选区
(delete-selection-mode 1)
;; 开启自动恢复
(global-auto-revert-mode t)
;; 压缩文件支持
(auto-compression-mode t)
;; 开启最近访问的文件
(recentf-mode t)
;; 高亮当前的行
(global-hl-line-mode t)

;; 保存文件时移除空格
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook
          (lambda()
            (delete-trailing-whitespace)))
;; 用 y/n 代替 yes/no
(fset 'yes-or-no-p 'y-or-n-p)

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


;; ==============================================
;; 备份
;; ==============================================
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

(provide 'core/default)
;;; default.el ends here
