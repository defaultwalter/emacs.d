;;; package -- option
;;; Commentary:
;;; Code:

(defgroup maf nil
  "Option for my config"
  :prefix "maf/")

(defcustom maf/fontsize 12
  "Font size"
  :type 'integer
  :group 'maf)


;;;; Custom文件配置
;; Use `user.el` to save custom config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;Load custom config
(when (file-exists-p custom-file)
  (load-file custom-file))
(provide 'core/option)
