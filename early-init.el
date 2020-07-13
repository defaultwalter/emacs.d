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
(setq gc-cons-threshold (* 1048576 125))
(setq garbage-collection-messages t)    ;显示GC信息
;; (setq gc-cons-percentage 0.1)           ;GC
;;空闲15秒后进行GC
(run-with-idle-timer 15 t (lambda()
                            (garbage-collect)
                            ;; (message "Garbage Collector has run for %.06f sec" (collect-time
                            ;;                                                     (garbage-collect)))
                            ))

;; Emacs在加载文件时会通过‘file-name-handler-alist’ 加载对应的处理程序。启动时设置为空。启动完成后重新设置。
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; 启动后重新设置一些在启动时修改的选项
(add-hook 'emacs-startup-hook (lambda ()
                                ;; 重新设置GC阈值
                                (setq gc-cons-threshold (* 1048576 125))
                                ;; 重新设置‘file-name-handler-alist’
                                (setq file-name-handler-alist file-name-handler-alist-original)
                                (makunbound 'file-name-handler-alist-original)))
(provide 'early-init)
