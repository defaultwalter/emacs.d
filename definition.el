;; 定义一些常量
(defconst environment/gui (display-graphic-p)
  "Are we running on a GUI Emacs?")

(defconst environment/windows (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst environment/linux (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst environment/mac (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst environment/root (string-equal "root" (getenv "USER"))
  "Are you a ROOT user?")

(defconst misc-file-directory (expand-file-name "misc" user-emacs-directory)
  "Save automatically generated files.")

(defconst bin-file-directory (expand-file-name "bin" user-emacs-directory)
  "Binary file.")

(defconst right-arrow-bitmap
  [#b00000000                           ;
   #b00000000                           ;
   #b00000000                           ;
   #b00000000                           ;
   #b00000000                           ;
   #b00000000                           ;
   #b00000000                           ;
   #b00000000                           ;
   #b00000000                           ;
   #b00000000                           ;
   #b00000000                           ;
   #b00000000                           ;
   #b00000000                           ;
   #b00101010                           ;
   #b00000000                           ;
   #b00000010                           ;
   #b00000000                           ;
   #b00000010                           ;
   #b00000000                           ;
   #b00000010                           ;
   #b00000000                           ;
   #b00010010                           ;
   #b00100000                           ;
   #b01111110                           ;
   #b00000000                           ;
   #b00000000])

(defconst left-arrow-bitmap
  [#b00000000                           ;
   #b00000000                           ;
   #b00000000                           ;
   #b00000000                           ;
   #b00000000                           ;
   #b00000000                           ;
   #b00000000                           ;
   #b00000000])

(defun set-font (font size)
  "Set font"
  (set-face-attribute 'default nil
                      :font (format   "%s:pixelsize=%d" font size))
  ;; chinese font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family font))))
(defun switch-im (im)
  "Switch Input Method"
  (start-process "issw" nil (expand-file-name "issw" bin-file-directory) im))

(defun switch-to-default-im ()
  "Switch to default Input Method"
  (switch-im "com.apple.keylayout.ABC"))

(defmacro collect-time
    (&rest
     body)
  "计算表达式执行所花费的时间"
  `(let ((time (current-time))) ,@body (float-time (time-since time))))

(provide 'core/definition)
