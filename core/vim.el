(defvar-local 
  by/default-input-method
  "com.apple.keylayout.ABC")
;; Switch to default keyboard.
(defun by/switch-to-input-method(input-method) 
  "Switch to INPUT-METHOD."
  (call-process-shell-command (concat "~/.emacs.d/issw " input-method)))
(defun by/switch-to-default-input-method() 
  "Switch to default input method."
  (by/switch-to-input-method by/default-input-method))

;; Only show user buffers.
(defun by/switch-user-buffer () 
  (interactive) 
  (let ((ivy-ignore-buffers '("^\\*.*"))) 
    (ivy-switch-buffer)))
;; Not show hidden files.
(defun by/find-file() 
  (interactive) 
  (let ((counsel-find-file-ignore-regexp "^\\..*")) 
    (counsel-find-file)))

(use-package 
  evil 
  :ensure t 
  :config (evil-mode)
  ;; (setq evil-move-cursor-back nil)
  ;; (defalias 'evil-insert-state 'evil-emacs-state) 
  (setq evil-emacs-state-cursor '("#ffb1ef" bar)) 
  (setq evil-normal-state-cursor '("#55b1ef" box)) 
  (setq evil-visual-state-cursor '("orange" box)) 
  (setq evil-insert-state-cursor '("#c46bbc" bar)) 
  (setq evil-replace-state-cursor '("#c46bbc" hollow-rectangle)) 
  (setq evil-operator-state-cursor '("#c46bbc" hollow)) 
  (add-hook 'evil-insert-state-exit-hook (lambda () 
                                          (by/switch-to-default-input-method)))
  ;; :hook (evil-normal-state-entry . by/switch-to-default-input-method)
  :bind (:map evil-emacs-state-map
              ("<escape>" . evil-normal-state) 
              ("C-g" . evil-normal-state)))


(use-package 
  evil-leader 
  :ensure t 
  :config (global-evil-leader-mode) 
  (evil-leader/set-leader "SPC") 
  (evil-leader/set-key "SPC" 'counsel-M-x) 
  (evil-leader/set-key "f f" 'by/find-file) 
  (evil-leader/set-key "f a" 'find-file) 
  (evil-leader/set-key "b a" 'switch-to-buffer) 
  (evil-leader/set-key "b b" 'by/switch-user-buffer) 
  (evil-leader/set-key "b k" 'kill-buffer) 
  (evil-leader/set-key "w" 'evil-window-map) 
  (evil-leader/set-key "h" 'evil-first-non-blank) 
  (evil-leader/set-key "l" 'evil-end-of-line)
  (evil-leader/set-key "? f" 'describe-function)
  (evil-leader/set-key "? v" 'describe-variable)
  (evil-leader/set-key "? k" 'describe-key))


(provide 'vim)
