;; hide scroll bar
(scroll-bar-mode -1)
;; hide tool bar
(tool-bar-mode -1)
;; hide menu bar
(menu-bar-mode -1)
;; not show tooltip
(tooltip-mode -1)

;; set frame title
(setq-default frame-title-format "[%m] %f")

(setq-default
 ;; 不显示启动屏幕
 inhibit-splash-screen 1
 ;; 将 Scratch 的内容设为空
 initial-scratch-message nil
 ;; 不现实对话框
 use-dialog-box nil)

(when window-system
  ;; frame 透明
  (set-frame-parameter nil 'alpha 99)

  ;; 自定义换行标识
  (define-fringe-bitmap 'right-curly-arrow [#b00000000 #b00000000 #b00000000 #b00000000 #b00000000
                                                       #b00000000 #b00000000 #b00000000 #b00000000
                                                       #b00000000
                                                       ;;
                                                       #b00000000 #b00000000 #b00000000 #b00101010
                                                       #b00000000 #b00000010 #b00000000 #b00000010
                                                       #b00000000 #b00000010 #b00000000 #b00010010
                                                       #b00100000 #b01111110 #b00000000 #b00000000])
  (define-fringe-bitmap 'left-curly-arrow [#b00000000 #b00000000 #b00000000 #b00000000 #b00000000
                                                      #b00000000 #b00000000 #b00000000])
  ;; if system is macOS
  (when (eq system-type 'darwin)
    ;; Emacs-plus transparent of title bar.
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))))


(use-package
  doom-themes
  ;; :when (display-graphic-p)
  :ensure t
  :init (load-theme 'doom-one t)
  :config
  (set-face-attribute 'fringe nil
                      :foreground "#fc5c59"
                      :background (face-background 'default))
  (defun on-frame-open (frame)
    (if (not (display-graphic-p frame))
        (set-face-background 'default "unspecified-bg" frame)))
  (on-frame-open (selected-frame))
  (add-hook 'after-make-frame-functions 'on-frame-open))
;; Modeline theme
(use-package
  doom-modeline
  :ensure t
  :init (doom-modeline-init)
  (setq doom-modeline-height 25 doom-modeline-bar-width 3 doom-modeline-icon nil
        doom-modeline-enable-word-count 10 doom-modeline-icon (display-graphic-p)
        doom-modeline-buffer-file-name-style 'file-name doom-modeline-modal-icon nil)
  :hook (after-init . doom-modeline-mode)
  :config
  ;; (set-face-attribute 'mode-line nil
  ;;                             :background nil)
  ;; (set-face-attribute 'mode-line-inactive nil
  ;;                     :background nil)
  )
(use-package
  cnfonts
  :ensure t
  :init (setq cnfonts-profiles '("default" "min" "max"))
  (setq cnfonts-use-system-type t)
  :config (cnfonts-enable))


(use-package
  windresize
  :ensure t
  :defer t)

(provide 'core/ui)
