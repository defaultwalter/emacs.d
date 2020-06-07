(use-package
  which-key
  :ensure t
  :defer nil
  :custom (which-key-enable-extended-define-key t)
  :init (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0)
  (setq which-key-sort-order 'which-key-key-order)
  (setq which-key-enable-extended-define-key t)
  :config (which-key-mode t)
  (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil))))



(setq file-map-prefix (w/create-leader-keymap "f" file-map "file"))
(w/create-leader-key "o" 'find-file "open-file" file-map-prefix)
(w/create-leader-key "f" 'find-file "open-file" file-map-prefix)
(w/create-leader-key "r" 'recentf-open-files "recent-file" file-map-prefix)


(setq buffer-map-prefix (w/create-leader-keymap "b" buffer-map "buffer"))
(w/create-leader-key "c" 'kill-current-buffer "close-buffer" buffer-map-prefix)
(w/create-leader-key "b" 'switch-to-buffer "switch-buffer" buffer-map-prefix)
(w/create-leader-key "k" 'kill-buffer "close-selected-buffer" buffer-map-prefix)
(w/create-leader-key "w" 'kill-buffer-and-window "close-buffer-and-window" buffer-map-prefix)
(w/create-leader-key "s" 'save-buffer "save-buffer" buffer-map-prefix)
(w/create-leader-key "S" 'save-some-buffers "save-all-buffer-interactive" buffer-map-prefix)
(w/create-leader-key "p" 'switch-to-prev-buffer "previous-buffer" buffer-map-prefix)
(w/create-leader-key "n" 'switch-to-next-buffer "next-buffer" buffer-map-prefix)
(w/create-leader-key "m" 'view-echo-area-messages "message-buffer" buffer-map-prefix)

(setq window-map-prefix (w/create-leader-keymap "w" window-map "window"))
(w/create-leader-key "c" 'delete-window "close-window" window-map-prefix)
(w/create-leader-key "o" 'delete-other-windows "close-other-window" window-map-prefix)
(w/create-leader-key "m" 'maximize-window "maximize-window" window-map-prefix)
(w/create-leader-key "n" 'minimize-window "minimize-window" window-map-prefix)
(w/create-leader-key "b" 'balance-windows "balance-window" window-map-prefix)
(w/create-leader-key "s" 'split-window-horizontally "split-window-horizontally" window-map-prefix)
(w/create-leader-key "v" 'split-window-vertically "split-window-vertically" window-map-prefix)
(w/create-leader-key "w" 'kill-buffer-and-window "close-window-and-buffer" window-map-prefix)

(setq content-map-prefix (w/create-leader-keymap "c" content-map "content"))
(w/create-leader-key "c" 'comment-line "comment" content-map-prefix)
(w/create-leader-key "r" 'comment-or-uncomment-region "(un)comment-region" content-map-prefix)


(setq goto-map-prefix (w/create-leader-keymap "g" goto-map "goto"))

(setq project-map-prefix (w/create-leader-keymap "p" project-map "project"))

(setq major-map-prefix (w/create-leader-keymap "m" major-mode-map "major"))

(setq help-map-prefix (w/create-leader-keymap "h" help-map "help"))
(w/create-leader-key "RET" 'view-order-manuals "manuals" help-map-prefix)
(w/create-leader-key "f" 'describe-function "function-help" help-map-prefix)
(w/create-leader-key "v" 'describe-variable "variable-help" help-map-prefix)
(w/create-leader-key "k" 'describe-key "key-help" help-map-prefix)
(w/create-leader-key "c" 'describe-coding-system "coding-system" help-map-prefix)
(w/create-leader-key "m" 'describe-mode "mode-help" help-map-prefix)
(w/create-leader-key "p" 'describe-package "package-help" help-map-prefix)
(w/create-leader-key "w" 'where-is "where-is" help-map-prefix)
(w/create-leader-key "s" 'describe-symbol "symbol-help" help-map-prefix)
(w/create-leader-key "?" 'about-emacs "about" help-map-prefix)



(provide 'core/keymap)
