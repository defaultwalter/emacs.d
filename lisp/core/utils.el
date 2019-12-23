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




(provide 'core/utils)
;;;utils.elendshere
