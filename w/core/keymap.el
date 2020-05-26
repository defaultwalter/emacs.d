(defun w/leader-map (key name &optional parent)
  (when parent
    (setq key (concat parent " " key)))
  (evil-leader/set-key key nil)
  (when name (which-key-add-key-based-replacements (concat evil-leader/leader " " key) name))
  (set (intern (concat "@" name "-map")) key))

(defun w/leader-set-key (key name def &optional parent)
  (when parent
    (setq key (concat parent " " key)))
  (evil-leader/set-key key def)
  (when name (which-key-add-key-based-replacements (concat evil-leader/leader " " key) name)))

(defun w/leader-set-key-for-mode (mode key name def &optional parent)
  (when parent
    (setq key (concat parent " " key)))
  (evil-leader/set-key-for-mode mode key def)
  (when (which-key-add-major-mode-key-based-replacements mode (concat evil-leader/leader " " key)
          name)))
(use-package
  evil
  :ensure t
  :custom (evil-want-minibuffer nil)
  :init
  ;;
  (setq evil-emacs-state-cursor '("#ffb1ef" bar))
  (setq evil-normal-state-cursor '("#55b1ef" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("#c46bbc" bar))
  (setq evil-replace-state-cursor '("#c46bbc" hollow-rectangle))
  (setq evil-operator-state-cursor '("#c46bbc" hollow))
  :config (evil-mode t))
(use-package
  evil-surround
  :ensure t
  :config (global-evil-surround-mode 1))
(use-package
  evil-leader
  :ensure t
  :after evil
  :init (setq evil-leader/in-all-states t)
  :config
  ;;
  (evil-leader/set-leader "SPC")
  (global-evil-leader-mode t)
  (w/leader-map "f" "file")
  (w/leader-map "b" "buffer")
  (w/leader-map "e" "edit")
  (w/leader-map "h" "help")
  (w/leader-map "w" "window")
  (w/leader-map "j" "jump")
  (w/leader-map "c" "content")
  (w/leader-map "v" "view")
  (w/leader-map "s" "search")
  (w/leader-map "p" "project")
  (w/leader-set-key "h" "switch-to-left-window" 'evil-window-left @window-map)
  (w/leader-set-key "j" "switch-to-down-window" 'evil-window-down @window-map)
  (w/leader-set-key "k" "switch-to-up-window" 'evil-window-up @window-map)
  (w/leader-set-key "l" "switch-to-right-window" 'evil-window-right @window-map)
  ;; 这里有个BUG，需要重新开启 evil-mode 否则在 *Messages* buffer 中，evil-leader 无效。
  ;; https://github.com/cofi/evil-leader/issues/10
  (evil-mode t))

(use-package
  evil-terminal-cursor-changer
  :ensure t
  :unless (display-graphic-p)
  :after evil
  :config (evil-terminal-cursor-changer-activate))

(use-package
  which-key
  :ensure t
  ;; :commands(which-key-add-key-based-replacements)
  :init (setq which-key-idle-delay 0)
  (setq which-key-idle-secondary-delay 0)
  (setq which-key-sort-order 'which-key-key-order)
  :config (which-key-mode t))



(w/leader-set-key "f" "open-file" 'find-file @file-map)
(w/leader-set-key "r" "recent-file" 'recentf-open-files @file-map)

(w/leader-set-key "c" "close-buffer" 'kill-current-buffer @buffer-map)
(w/leader-set-key "b" "switch-buffer" 'switch-to-buffer @buffer-map)
(w/leader-set-key "C" "close-selected-buffer" 'kill-buffer @buffer-map)
(w/leader-set-key "w" "close-buffer-and-window" 'kill-buffer-and-window @buffer-map)
(w/leader-set-key "s" "save-buffer" 'save-buffer @buffer-map)
(w/leader-set-key "S" "save-all-buffer" 'save-some-buffers @buffer-map)
(w/leader-set-key "p" "previous-buffer" 'switch-to-prev-buffer @buffer-map)
(w/leader-set-key "n" "next-buffer" 'switch-to-next-buffer @buffer-map)
(w/leader-set-key "n" "switch-to-message-buffer" 'view-echo-area-messages @buffer-map)

(w/leader-set-key "f" "search-forward" 'search-forward @search-map)
(w/leader-set-key "b" "search-backward" 'search-backward @search-map)

(w/leader-set-key "f" "describe-function" 'describe-function @help-map)
(w/leader-set-key "v" "describe-variable" 'describe-variable @help-map)
(w/leader-set-key "k" "describe-key" 'describe-key @help-map)
(w/leader-set-key "c" "describe-coding" 'describe-coding-system @help-map)
(w/leader-set-key "m" "describe-mode" 'describe-mode @help-map)
(w/leader-set-key "p" "describe-package" 'describe-package @help-map)
(w/leader-set-key "w" "where-is" 'where-is @help-map)
(w/leader-set-key "s" "describe-symbol" 'describe-symbol @help-map)
(w/leader-set-key "?" "about-emacs" 'about-emacs @help-map)

(w/leader-set-key "c" "close-window" 'delete-window @window-map)
(w/leader-set-key "o" "close-other-window" 'delete-other-windows @window-map)
(w/leader-set-key "m" "maximize-window" 'maximize-window @window-map)
(w/leader-set-key "n" "minimize-window" 'minimize-window @window-map)
(w/leader-set-key "b" "balance-window" 'balance-windows @window-map)
(w/leader-set-key "s" "split-window-horizontally" 'split-window-horizontally @window-map)
(w/leader-set-key "v" "split-window-vertically" 'split-window-vertically @window-map)
(provide 'core/keymap)
