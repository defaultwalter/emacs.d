;; 设置GC阈值，防止在启动时GC，影响启动速度。
(setq gc-cons-threshold 268435456)      ;256M
;; 禁止启动后激活已经安装的包
(setq package-enable-at-startup nil)
;; Emacs在加载文件时会通过`file-name-handler-alist`加载对应的处理程序。启动时设置为空。启动完成后重新设置。
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))
;; ‘site-run-file’ 包含一些初始值。这个文件会在 ‘~/.emacs’ 之前加载。
(setq site-run-file nil)

;; 移除不需要的GUI元素
(menu-bar-mode -1)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (and (eq system-type 'darwin)
           (display-graphic-p))
  (add-to-list  default-frame-alist '(menu-bar-lines . 0))
  ;; MacOS 某些版本Emacs版本支持标题栏透明
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))


(provide 'early-init)
