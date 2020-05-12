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

(setq linum-format "%4d|")
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


(defun my/set-font ()
  ;;
  (set-face-attribute 'default nil
                      :font (format   "%s:pixelsize=%d" "Sarasa Mono SC" 18))
  ;; chinese font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family "Sarasa Mono SC"))))
(if (display-graphic-p)
    (my/set-font))
;; (if (daemonp)
;;     (my/set-font))
(defun my/set-frame (frame)
  (select-frame frame)
  (my/set-font))
(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/set-frame))
(use-package
  doom-themes
  ;; :when (display-graphic-p)
  :ensure t
  :init (load-theme 'doom-one t)
  :config (set-face-attribute 'fringe nil
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
  (setq doom-modeline-height 20 doom-modeline-bar-width 3 doom-modeline-icon nil
        doom-modeline-enable-word-count 10
        ;; doom-modeline-icon (display-graphic-p)
        doom-modeline-buffer-file-name-style 'relative-to-project doom-modeline-modal-icon nil)
  ;; (set-face-attribute 'mode-line nil
  ;;                     :background "#2f3239")
  ;; (set-face-attribute 'mode-line-inactive nil
  ;;                     :background nil)
  :hook (after-init . doom-modeline-mode)
  :config)

;; 跳转后 显示光标位置
(use-package
  beacon
  :ensure t
  :config (beacon-mode t))

;; Rainbow parentheses
(use-package
  rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight parentheses
(use-package
  highlight-parentheses
  :ensure t
  :defer t
  :hook (prog-mode . highlight-parentheses-mode))
;; Highlight indent
(use-package
  highlight-indent-guides
  :ensure t
  :defer t
  :init (setq highlight-indent-guides-method 'fill)
  (setq highlight-indent-guides-auto-odd-face-perc 15)
  (setq highlight-indent-guides-auto-even-face-perc 15)
  (setq highlight-indent-guides-auto-character-face-perc 20)
  (setq highlight-indent-guides-auto-enabled nil)
  :hook (prog-mode . highlight-indent-guides-mode))
(provide 'core/ui)
