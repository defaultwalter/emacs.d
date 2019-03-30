;;; package -- vim
;;; Commentary:
;;; Code:


(use-package
  evil
  :ensure t
  :config
  (setq evil-emacs-state-cursor '("#ffb1ef" bar))
  (setq evil-normal-state-cursor '("#55b1ef" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("#c46bbc" bar))
  (setq evil-replace-state-cursor '("#c46bbc" hollow-rectangle))
  (setq evil-operator-state-cursor '("#c46bbc" hollow))
  (evil-mode 1)
  :bind (:map evil-insert-state-map
              ("C-g" . evil-normal-state)))
(use-package
  evil-leader
  :ensure t
  :after hydra
  ;; :commands (evil-leader/set-leader)
  :config (evil-leader/set-leader "SPC")
  (defhydra hydra-frame-window (:color red :hint nil)
    "
^Frame^                        ^^Frame Size^^
_s_plit
_vs_plit                        ^ ^  _k_  ^ ^
_d_elete                        _h_ _TAB_ _l_
delete _o_ther                  ^ ^  _j_  ^ ^
"
    ("s" evil-window-split)
    ("vs" evil-window-vsplit)
    ("d" evil-window-delete  :exit t)
    ("o" delete-other-windows :exit t)
    ("TAB" other-window)
    ("h" shrink-window-horizontally)
    ("k" shrink-window)
    ("j" enlarge-window)
    ("l" enlarge-window-horizontally)
    ("q" nil "quit"))
  (evil-leader/set-key "SPC" 'counsel-M-x)
  (evil-leader/set-key "f f" 'by/find-file)
  (evil-leader/set-key "f a" 'find-file)
  (evil-leader/set-key "b b" 'switch-to-buffer)
  (evil-leader/set-key "b k" 'kill-buffer)
  (evil-leader/set-key "w" #'hydra-frame-window/body)
  (evil-leader/set-key "q" #'evil-window-map)
  (evil-leader/set-key "h" 'evil-first-non-blank)
  (evil-leader/set-key "l" 'evil-end-of-line)
  (evil-leader/set-key "? f" 'describe-function)
  (evil-leader/set-key "? v" 'describe-variable)
  (evil-leader/set-key "? k" 'describe-key)
  (global-evil-leader-mode))


(provide 'vim)
;;; vim.el ends here
