(setq window-sides-slots '(1 1 1 1))
(setq window-sides-vertical t)

(setq +bottom-side-windows '("\\*org-roam\\*"
                             help-mode
                             debugger-mode
                             cargo-process-mode
                             message-mode
                             vterm-mode
                             deft-mode))

(defun +display-buffer-in-bottom-side-window-condition (buffer action)
  (seq-filter (lambda (item)
                (cond  ((stringp item)
                        (s-matches-p item buffer))
                       ((symbolp item)
                        (with-current-buffer buffer (equal major-mode item)))))
              +bottom-side-windows))

(defun +display-buffer-in-bottom-side-window (buffer alist)

  (let  ((bottom-side-window (window-with-parameter 'window-side 'bottom)))
    (when bottom-side-window
      (delete-window bottom-side-window))
    (display-buffer-in-side-window buffer alist)))

(setq display-buffer-alist '((+display-buffer-in-bottom-side-window-condition
                              +display-buffer-in-bottom-side-window
                              (dedicated .  t)
                              (window-parameters . (;; (delete-window . nil)
                                                    ;; (delete-other-windows . nil)
                                                    (no-delete-other-windows . t)
                                                    ;; (quit-restore . (window window nil (get-buffer buffer-name)))
                                                    ))
                              (window-min-height . 10)
                              (window-height . 0.3)
                              (side . bottom)
                              (slot . 0))))
;;
(provide 'core/layout)
