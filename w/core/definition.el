(defun w/set-font ()
  ;;
  (set-face-attribute 'default nil
                      :font (format   "%s:pixelsize=%d" "Sarasa Mono SC" 20))
  ;; chinese font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family "Sarasa Mono SC"))))

(defun w/set-wrap-fringe ()
  ;; 自定义换行标识
  (define-fringe-bitmap 'right-curly-arrow [#b00000000 ;
                                            #b00000000 ;
                                            #b00000000 ;
                                            #b00000000 ;
                                            #b00000000 ;
                                            #b00000000 ;
                                            #b00000000 ;
                                            #b00000000 ;
                                            #b00000000 ;
                                            #b00000000 ;
                                            #b00000000 ;
                                            #b00000000 ;
                                            #b00000000 ;
                                            #b00101010 ;
                                            #b00000000 ;
                                            #b00000010 ;
                                            #b00000000 ;
                                            #b00000010 ;
                                            #b00000000 ;
                                            #b00000010 ;
                                            #b00000000 ;
                                            #b00010010 ;
                                            #b00100000 ;
                                            #b01111110 ;
                                            #b00000000 ;
                                            #b00000000])
  (define-fringe-bitmap 'left-curly-arrow [#b00000000 ;
                                           #b00000000 ;
                                           #b00000000 ;
                                           #b00000000 ;
                                           #b00000000 ;
                                           #b00000000 ;
                                           #b00000000 ;
                                           #b00000000]))



(defmacro w/bind-to-map (map key command &optional name)
  `(if ,name (progn (define-key ,map ,key '(,name . ,command)))
     (define-key ,map ,key ,command)))

(defun w/get-mode-map (mode)
  (let ((mode-map (cdr (assoc mode evil-leader--mode-maps))))
    (unless mode-map
      (setq mode-map (copy-keymap evil-leader--default-map))
      ;; (setq mode-map (make-sparse-keymap))
      ;; (set-keymap-parent mode-map evil-leader--default-map)
      (push (cons mode mode-map) evil-leader--mode-maps)) mode-map))


;; (defmacro w/create-keymap (prefix map-name name)
;;   `(progn
;;      (setq ,map-name (make-sparse-keymap))
;;      (define-prefix-command ',map-name ',map-name ,name)
;;      ))

(defmacro w/create-keymap (map-name name)
  `(progn
     (setq ,map-name (make-sparse-keymap))
     (define-prefix-command ',map-name ',map-name ,name)))

(provide 'core/definition)
