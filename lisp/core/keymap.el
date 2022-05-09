;;; package -- keymap
;;; Commentary:
;;; Code:

(defvar mini-global-map (make-sparse-keymap)
  "Global keymap")
(define-key mini-global-map [mouse-1] (lambda(event)
                                        (interactive "e")
                                        (mouse-set-point event)))

(define-key mini-global-map [mouse-4] #'mwheel-scroll)
(define-key mini-global-map [mouse-5] #'mwheel-scroll)
(define-key mini-global-map [mouse-6] #'mwheel-scroll)
(define-key mini-global-map [mouse-7] #'mwheel-scroll)

(substitute-key-definition 'self-insert-command 'self-insert-command mini-global-map global-map)

(substitute-key-definition 'newline 'newline mini-global-map global-map)
(substitute-key-definition 'indent-for-tab-command 'indent-for-tab-command mini-global-map global-map)
(substitute-key-definition 'delete-char 'delete-char mini-global-map global-map)
(substitute-key-definition 'delete-backward-char 'delete-backward-char mini-global-map global-map)

(substitute-key-definition 'move-beginning-of-line 'move-beginning-of-line mini-global-map global-map)
(substitute-key-definition 'move-end-of-line 'move-end-of-line mini-global-map global-map)

(substitute-key-definition 'right-char 'right-char mini-global-map global-map)
(substitute-key-definition 'left-char 'left-char mini-global-map global-map)

(substitute-key-definition 'backward-char 'backward-char mini-global-map global-map)
(substitute-key-definition 'forward-char 'forward-char mini-global-map global-map)

(substitute-key-definition 'next-line 'next-line mini-global-map global-map)
(substitute-key-definition 'previous-line 'previous-line mini-global-map global-map)

(substitute-key-definition 'swiper 'swiper mini-global-map global-map)

(substitute-key-definition 'keyboard-quit 'keyboard-quit mini-global-map global-map)

;; (add-hook 'after-init-hook (lambda()
;;                              (use-global-map mini-global-map)))

(provide 'core/keymap)
;;; keymap.el ends here
