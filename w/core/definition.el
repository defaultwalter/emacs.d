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

(defmacro w/create-keymap (map-name name &optional parent)
  `(progn
     (setq ,map-name (make-sparse-keymap))
     (define-prefix-command ',map-name ',map-name ,name)
     (when ,parent (set-keymap-parent ,map-name ,parent))
     (lambda ()
       (which-key-show-keymap ',map-name))))

(defmacro w/create-leader-keymap (key map-name name &optional parent leader-key)
  `(let ((current-key (if ,parent (concat ,parent " " ,key) ,key) )
         (key-map (make-sparse-keymap))
         (leader-key  (when (eq ,leader-key nil) "SPC")))
     (define-prefix-command ',map-name 'key-map ,name)
     (evil-leader/set-key current-key key-map)
     (which-key-add-key-based-replacements (concat leader-key " " current-key) ,name)
     current-key))

(defmacro w/create-leader-key (key command name &optional parent leader-key)
  `(let ((current-key (if ,parent (concat ,parent " " ,key) ,key) )
         (leader-key  (when (eq ,leader-key nil) "SPC")))
     (evil-leader/set-key current-key ,command)
     (which-key-add-key-based-replacements (concat leader-key " " current-key) ,name)))

(defmacro w/create-leader-keymap-for-mode (mode key map-name name &optional parent leader-key)
  `(let ((current-key (if ,parent (concat ,parent " " ,key) ,key) )
         (key-map (make-sparse-keymap))
         (leader-key  (when (eq ,leader-key nil) "SPC")))
     (define-prefix-command ',map-name 'key-map ,name)
     (evil-leader/set-key-for-mode mode current-key key-map)
     (which-key-add-major-mode-key-based-replacements mode (concat leader-key " "
                                                                   current-key) ,name) current-key))

(defmacro w/create-leader-key-for-mode (mode key command name &optional parent leader-key)
  `(let ((current-key (if ,parent (concat ,parent " " ,key) ,key) )
         (leader-key  (when (eq ,leader-key nil) "SPC")))
     (evil-leader/set-key-for-mode mode current-key ,command)
     (which-key-add-major-mode-key-based-replacements mode (concat leader-key " "
                                                                   current-key) ,name)))

(provide 'core/definition)
