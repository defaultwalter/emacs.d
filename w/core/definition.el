;; 定义一些常量
(defconst environment/gui
  (display-graphic-p)
  "Are we running on a GUI Emacs?")

(defconst environment/windowns
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst environment/linux
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst environment/mac
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst environment/root
  (string-equal "root" (getenv "USER"))
  "Are you a ROOT user?")

(defconst misc-file-directory
  (expand-file-name "misc" user-emacs-directory)
  "Save automatically generated files.")

(defun w/set-font ()
  "Set font"
  (set-face-attribute 'default nil
                      :font (format   "%s:pixelsize=%d" "Sarasa Mono SC" 20))
  ;; chinese font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family "Sarasa Mono SC"))))

(defun w/set-wrap-fringe ()
  "Set wrap fringe"
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
