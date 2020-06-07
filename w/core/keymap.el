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

(w/create-keymap file-map "File Map")
(w/bind-to-map file-map "o" find-file "open-file")
(w/bind-to-map file-map "f" find-file "open-file")
(w/bind-to-map file-map "r" recentf-open-files "recent-file")

(w/create-keymap buffer-map "Buffer Map")
(w/bind-to-map buffer-map "c" kill-current-buffer "close-buffer")
(w/bind-to-map buffer-map "b" switch-to-buffer "switch-buffer")
(w/bind-to-map buffer-map "k" kill-buffer "close-selected-buffer")
(w/bind-to-map buffer-map "w" kill-buffer-and-window "close-buffer-and-window")
(w/bind-to-map buffer-map "s" save-buffer "save-buffer")
(w/bind-to-map buffer-map "S" save-some-buffers "save-all-buffer-interactive")
(w/bind-to-map buffer-map "p" switch-to-prev-buffer "previous-buffer")
(w/bind-to-map buffer-map "n" switch-to-next-buffer "next-buffer")
(w/bind-to-map buffer-map "m" view-echo-area-messages "message-buffer")

(w/create-keymap window-map "Window Map")


(w/bind-to-map window-map "c" delete-window "close-window")
(w/bind-to-map window-map "o" delete-other-windows "close-other-window")
(w/bind-to-map window-map "m" maximize-window "maximize-window")
(w/bind-to-map window-map "n" minimize-window "minimize-window")
(w/bind-to-map window-map "b" balance-windows "balance-window")
(w/bind-to-map window-map "s" split-window-horizontally "split-window-horizontally")
(w/bind-to-map window-map "v" split-window-vertically "split-window-vertically")
(w/bind-to-map window-map "w" kill-buffer-and-window "close-window-and-buffer")

(w/create-keymap content-map "Content Map")
(w/bind-to-map content-map "c" comment-line "comment")
(w/bind-to-map content-map "r" comment-or-uncomment-region "(un)comment-region")

(w/create-keymap goto-map "Goto Map")

(w/create-keymap project-map "Project Map")

(w/create-keymap major-map "Major Map")

(w/create-keymap help-map "Help Map")
(w/bind-to-map help-map (kbd "<return>") 'view-order-manuals "manuals")
(w/bind-to-map help-map "f" describe-function "function-help")
(w/bind-to-map help-map "v" describe-variable "variable-help")
(w/bind-to-map help-map "k" describe-key "key-help")
(w/bind-to-map help-map "c" describe-coding-system "coding-system")
(w/bind-to-map help-map "m" describe-mode "mode-help")
(w/bind-to-map help-map "p" describe-package "package-help")
(w/bind-to-map help-map "w" where-is "where-is")
(w/bind-to-map help-map "s" describe-symbol "symbol-help")
(w/bind-to-map help-map "?" about-emacs "about")



(provide 'core/keymap)
