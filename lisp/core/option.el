;;; package -- option
;;; Commentary:
;;; Code:

(defgroup user nil
  "Option for my config"
  :prefix "user/")

(defcustom user/fontsize 12
  "Font size"
  :type 'integer
  :group 'user)


(defcustom user/note-directory "~/.cache/notes"
  "Note root directory"
  :type 'string
  :group 'user)
(unless (file-directory-p user/note-directory)
  (mkdir user/note-directory))

(defcustom user/note-server-host "127.0.0.1"
  "Note server host"
  :type 'string
  :group 'user)

(defcustom user/note-server-port 10101
  "Note server port"
  :type 'integer
  :group 'user)


(defcustom user/agenda-directory "~/.cache/agenda"
  "Agenda root directory"
  :type 'string
  :group 'user)
(unless (file-directory-p user/agenda-directory)
  (mkdir user/agenda-directory))

;;;; Custom文件配置
;; Use `user.el` to save custom config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;Load custom config
(when (file-exists-p custom-file)
  (load-file custom-file))

(provide 'core/option)
