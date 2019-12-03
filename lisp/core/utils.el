;;;package--util
;;;Commentary:
;;;Code:

(defun install-package 
    (name &optional refresh)
"Install package by NAME and REFRESH."
    (when refresh 
        (progn 
            (message "Refresh contents")
            (package-refresh-contents)))
    (if 
        (package-installed-p name) t 
        (if 
            (assoc name package-archive-contents)
            (progn 
                (message "Auto install Package:%s" name)
                (package-install name)
                (require name))
            (if 
                (not refresh)
                (install-package name t)
                (error
"Not found package:%s"
name)))))



(defun set-gui-font
    ()
"Set gui font."

    (set-fontset-font t nil 
        (font-spec :family "Apple Color Emoji") nil 'append)
    (set-face-attribute 'default nil
:font
        (font-spec :name
"-*-Source Code Pro-ultrabold-italic-normal-*-*-*-*-*-m-0-iso10646-1"
:weight'normal
:slant'normal
:size 18))
    (dolist 
        (charset '
            (kana han symbol cjk-misc bopomofo))
        (set-fontset-font 
            (frame-parameter nil 'font) charset 
            (font-spec :name
"-*-PingFang SC-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1"
:weight'normal
:slant'normal
:size 18))))
(provide 'core/utils)
;;;utils.elendshere
