(defmacro collect-time
    (&rest
     body)
  "计算表达式执行所花费的时间"
  `(let ((time (current-time))) ,@body (float-time (time-since time))))

;; 移除不需要的GUI元素
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; 禁止启动后激活已经安装的包
(setq package-enable-at-startup nil)
;; ‘site-run-file’ 包含一些初始值。这个文件会在 ‘~/.emacs’ 之前加载。
(setq site-run-file nil)

;; 设置GC阈值，防止在启动时GC，影响启动速度。
(setq gc-cons-threshold (* 1024 1024 128))
(setq garbage-collection-messages t)    ;显示GC信息
(setq gc-cons-percentage 0.01)          ;GC
;;空闲15秒后进行GC
(setq mgc-times 0)
(setq gc-times 0)
(run-with-idle-timer 15 t (lambda()
                            (setq mgc-times (1+ mgc-times))
                            (garbage-collect)))
(add-hook 'focus-out-hook (lambda()
                            (setq mgc-times (1+ mgc-times))
                            (garbage-collect)))
(add-hook 'post-gc-hook (lambda()
                          (setq gc-times (1+ gc-times))
                          (message "%s GC Happened MGC/GC %s/%s" (current-time-string) mgc-times
                                   gc-times)))

;; Emacs在加载文件时会通过‘file-name-handler-alist’ 加载对应的处理程序。启动时设置为空。启动完成后重新设置。
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; 启动后重新设置一些在启动时修改的选项
(add-hook 'emacs-startup-hook (lambda ()
                                ;; 重新设置GC阈值
                                (setq gc-cons-threshold (* 1024 1024 256))
                                ;; 重新设置‘file-name-handler-alist’
                                (setq file-name-handler-alist file-name-handler-alist-original)
                                (makunbound 'file-name-handler-alist-original)))
(defun set-font()
  ;; 12-1.7/14-1.75
  (setq face-font-rescale-alist '(("Inconsolata" . 1.0)
                                  ("Hiragino Sans GB" . 1.7)))
  (add-to-list 'default-frame-alist '(font . "Inconsolata-12"))
  (add-hook 'emacs-startup-hook (lambda()
                                  (dolist (charset '(kana han symbol cjk-misc bopomofo))
                                    (set-fontset-font (frame-parameter nil 'font) charset (font-spec
                                                                                           :family
                                                                                           "Hiragino Sans GB"
                                                                                           :size
                                                                                           12))))))

(set-font)
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 35))
(provide 'early-init)
