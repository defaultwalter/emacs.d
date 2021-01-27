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


(defcustom maf/note-directory "~/.notes"
  "Note root directory"
  :type 'string
  :group 'maf)

(defcustom maf/note-server-host "127.0.0.1"
  "Note server host"
  :type 'string
  :group 'maf)

(defcustom maf/note-server-port 10101
  "Note server port"
  :type 'integer
  :group 'maf)


(defcustom maf/agenda-directory "~/.agenda"
  "Agenda root directory"
  :type 'string
  :group 'maf)


;;;; Custom文件配置
;; Use `user.el` to save custom config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;Load custom config
(when (file-exists-p custom-file)
  (load-file custom-file))
(provide 'core/option)
