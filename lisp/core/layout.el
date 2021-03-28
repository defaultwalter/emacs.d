(setq window-sides-slots '(1 1 1 1))
(setq window-sides-vertical t)
(defvar bottom-windows)

(defvar +bottom-side-windows
  '("\\*Buffer List\\*" help-mode debugger-mode cargo-process-mode message-mode vterm-mode
    deft-mode))

(setq +bottom-side-windows'("\\*Buffer List\\*" help-mode debugger-mode cargo-process-mode
                            message-mode vterm-mode deft-mode))
(defun +display-buffer-in-bottom-side-window-condition (buffer action)
  (seq-filter (lambda (item)
                (message "--%s" item)
                (cond  ((stringp item)
                        (s-matches-p item buffer))
                       ((symbolp item)
                        (with-current-buffer buffer (equal major-mode item)))))
              +bottom-side-windows))

(defun +display-buffer-in-bottom-side-window (buffer alist)
  (let  ( (window (display-buffer-in-side-window buffer alist)) ) window))
(setq display-buffer-alist '((+display-buffer-in-bottom-side-window-condition
                              +display-buffer-in-bottom-side-window
                              ;; (dedicated .  t)
                              (window-parameters . ((delete-window . nil)
                                                    (delete-other-windows . nil)
                                                    (no-delete-other-windows . t)
                                                    (quit-restore . (window window nil (get-buffer
                                                                                        buffer-name)))))
                              (window-min-height . 10)
                              (window-height . 0.3)
                              (preserve-size . (t . t))
                              (side . bottom)
                              (slot . 0))))
;;
(s-matches-p "\\*Buffer List\\*" "\\*Buffer List\\*")
(popwin:display-buffer-condition popwin:display-buffer-action)

(message "\*Buffer List\*")
(setq display-buffer-mark-dedicated t)
(quit-restore other (#<buffer *Cargo Build*> 1 #<marker (moves after insertion) at 1 in *Cargo
                              Build*> 14) #<window 9 on layout.el> #<buffer *Help*>)
