;;; package -- util
;;; Commentary:
;;; Code:
;; Not show hidden files.
(defun by/find-file()
  "Find file."
  (interactive)
  (let ((counsel-find-file-ignore-regexp "^\\..*"))
    (counsel-find-file)))

(defun install-package (name &optional refresh)
  "Install package by NAME and REFRESH."
  (when refresh (progn (message "Refresh contents")
                       (package-refresh-contents)))
  (if (package-installed-p name) t (if (assoc name package-archive-contents)
                                       (progn (message "Auto install Package:%s" name)
                                              (package-install name)
                                              (require name))
                                     (if (not refresh)
                                         (install-package name t)
                                       (error
                                        "Not found package:%s"
                                        name)))))

(defun by/wrap-symbol()
  "Wrap symbol."
  (interactive)
  (when (region-active-p)
    (let ((left-symbol (format "%c" (read-char "Enter left symbol:")))
          (right-symbol (format "%c" (read-char "Enter right symbol:")))
          (start (region-beginning))
          (end (region-end)))
      (goto-char end)
      (insert right-symbol)
      (goto-char start)
      (insert left-symbol)
      (goto-char (+ end 2)))))

(defun by/kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
;; Keybonds
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)

(defun by/insert-random-uuid ()
  "Insert a random UUID.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d
WARNING: this is a simple implementation. The chance of generating the same UUID is much higher than a robust algorithm.."
  (interactive)
  (insert
   (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 6))
           (random (expt 16 6)))))
(provide 'utils)
;;; utils.el ends here
