;;; modal.el --- My modal edit -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; which-key
(unless (package-installed-p 'which-key)
  (package-install 'which-key))
(require 'which-key)

(custom-set-variables '(which-key-idle-delay 0)
                      '(which-key-idle-secondary-delay 0.05)
                      '(which-key-sort-order 'which-key-prefix-then-key-order)
                      '(which-key-allow-multiple-replacements t)
                      '(which-key-allow-evil-operators t)
                      '(which-key-popup-type 'side-window))

(add-to-list 'which-key-replacement-alist '(("ESC" . nil) . ("esc" . nil)))
(add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("tab" . nil)))
(add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("return" . nil)))
(add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("delete" . nil)))
(add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))
(add-to-list 'which-key-replacement-alist '(("left" . nil) . ("left" . nil)))
(add-to-list 'which-key-replacement-alist '(("right" . nil) . ("right" . nil)))
(add-to-list 'which-key-replacement-alist '(("<left>" . nil) . ("left" . nil)))
(add-to-list 'which-key-replacement-alist '(("<right>" . nil) . ("right" . nil)))
(add-to-list 'which-key-replacement-alist '(("up" . nil) . ("up" . nil)))
(add-to-list 'which-key-replacement-alist '(("down" . nil) . ("down" . nil)))
(which-key-mode t)

;;;; mode
(defgroup modal nil
  "Introduce modal editing"
  :tag "Modal"
  :prefix "modal-")

(defvar modal-mode-map (make-sparse-keymap)
  "Global keymap for Modal")

(defvar modal-insert-state-map
  (let ((keymap (make-keymap)))
    ;; (define-key keymap (kbd "<escape>") 'meow-insert-exit)
    keymap)
  "Keymap for Modal insert state.")

(defvar modal-normal-state-map (let ((keymap (make-keymap)))
                                 (suppress-keymap keymap t) keymap)
  "Keymap for Modal normal state.")

(defvar modal-motion-state-map
  (let ((keymap (make-keymap)))
    (suppress-keymap keymap t)
    (set-keymap-parent keymap modal-normal-state-map) keymap)
  "Keymap for Modal motion state.")


(define-minor-mode modal-normal-mode "Modal normal state."
  nil
  " ModalNormal"
  modal-normal-state-map
  (when modal-normal-mode               ;
    (modal-insert-mode -1)
    (modal-motion-mode -1)
    (setq-local cursor-type 'box)))

(define-minor-mode modal-insert-mode "Modal insert state."
  nil
  " ModalInsert"
  modal-insert-state-map
  (when modal-insert-mode               ;
    (modal-normal-mode -1)
    (modal-motion-mode -1)
    (setq-local cursor-type 'bar)))

(define-minor-mode modal-motion-mode "Modal motion state."
  nil
  " ModalMotion"
  modal-motion-state-map
  (when modal-motion-mode               ;
    (modal-insert-mode -1)
    (modal-normal-mode -1)
    (setq-local cursor-type 'box)))
;;; define mode
(defun modal--enable ()
  "Enable Modal mode"
  (if (derived-mode-p 'special-mode)
      (progn (message "special mode")
             (modal-motion-mode 1))
    (modal-normal-mode 1)))

(defun modal--disable ()
  "Disable Modal mode"
  (modal-normal-mode -1)
  (modal-motion-mode -1)
  (modal-insert-mode -1))

(define-minor-mode modal-mode "Toggle `modal-mode` minor mode."
  nil
  " Modal"
  modal-mode-map
  (if modal-mode (modal--enable)
    (modal--disable)))

(defun modal-mode--minibuffer-setup()
  "Set modal-mode state when minibuffer avtive."
  (setq-local modal-normal-mode nil))

(defun modal--global-enable()
  "Enable Modal global mode"
  (setq-default modal-normal-mode t)
  (setq-default modal-mode t)
  (add-hook 'minibuffer-setup-hook #'modal-mode--minibuffer-setup))

(defun modal--global-disable()
  "Disable Modal global mode"
  (setq-default meow-normal-mode nil)
  (setq-default modal-mode nil)
  (remove-hook 'minibuffer-setup-hook #'modal-mode--minibuffer-setup))

(define-globalized-minor-mode modal-global-mode modal-mode
  (lambda ()
    (unless (minibufferp)
      (modal-mode 1)))
  (if modal-mode (modal--global-enable)
    (modal--global-disable )))

;;;; leader
(defgroup modal-leader nil
  "<leader> support for modal."
  :group 'modal
  :prefix 'modal-leader-)

(defcustom modal-leader-key "SPC"
  "Leader key"
  :type 'string
  :group 'modal-leader)

(defvar modal--normal-mode-maps nil
  "Modal map for mode")

(defvar modal--motion-mode-maps nil
  "Modal map for mode")

(defvar modal-leader--mode-maps nil
  "Leader map for mode")

(defvar modal-leader--default-map (make-sparse-keymap)
  "Keymap used for mode-independent leader bindings.")

(define-key modal-normal-state-map (read-kbd-macro modal-leader-key) modal-leader--default-map)
(define-key modal-motion-state-map (read-kbd-macro modal-leader-key) modal-leader--default-map)

(defun modal-leader--apply-mode-map()
  "Apply mode map"
  (let ((normal-mode-map (or (cdr (assoc major-mode modal--normal-mode-maps))
                             modal-normal-state-map))
        (motion-mode-map (or (cdr (assoc major-mode modal--motion-mode-maps))
                             modal-motion-state-map)))
    (push `(modal-normal-mode . ,normal-mode-map) minor-mode-overriding-map-alist)
    (push `(modal-motion-mode . ,motion-mode-map) minor-mode-overriding-map-alist)))

(add-hook 'after-change-major-mode-hook #'modal-leader--apply-mode-map)

(defun modal-leader--define-key(map key def)
  "Leader define key."
  (let* ((def (if (commandp def)
                  (list :command def)
                (if (commandp (car def))
                    (cons :command def) def)))
         (ignore (plist-get def
                            :ignore))
         (command (plist-get def
                             :command))
         (which-key (plist-get def
                               :which-key)))
    (unless ignore (define-key map (read-kbd-macro key) command))
    (when which-key (which-key-add-keymap-based-replacements map key which-key))))

(defun modal-leader-set-key(key def)
  "Set key for leader."
  (modal-leader--define-key modal-leader--default-map key def))

(defun modal-leader-set-key-for-mode (mode key def)
  "Set key for leader."
  (let* ((normal-mode-map (cdr (assoc mode modal--normal-mode-maps)))
         (motion-mode-map (cdr (assoc mode modal--motion-mode-maps)))
         (leader-map (cdr (assoc mode modal-leader--mode-maps))))
    (unless normal-mode-map
      (setq normal-mode-map (make-sparse-keymap))
      (set-keymap-parent normal-mode-map modal-normal-state-map)
      (push (cons mode normal-mode-map) modal--normal-mode-maps))
    (unless motion-mode-map
      (setq motion-mode-map (make-sparse-keymap))
      (set-keymap-parent motion-mode-map modal-motion-state-map)
      (push (cons mode motion-mode-map) modal--motion-mode-maps))
    (unless leader-map
      (setq leader-map (make-sparse-keymap))
      (set-keymap-parent leader-map modal-leader--default-map)
      (push (cons mode leader-map) modal-leader--mode-maps)
      (define-key normal-mode-map (read-kbd-macro modal-leader-key) leader-map)
      (define-key motion-mode-map (read-kbd-macro modal-leader-key) leader-map))
    (modal-leader--define-key leader-map key def)))

;;;; function
(defun modal--set-mark()
  (when (not (region-active-p))
    (set-mark-command nil)))

;;;; command

(defun modal-insert()
  (interactive)
  (when (region-active-p)
    (goto-char (region-beginning))
    (deactivate-mark t))
  (modal-insert-mode 1))

(defun modal-append()
  (interactive)
  (when (region-active-p)
    (goto-char (region-end))
    (deactivate-mark t))
  (modal-forward-char 1)
  (modal-insert-mode 1))

(defun modal-line-insert()
  (interactive)
  (when (region-active-p)
    (deactivate-mark t))
  (goto-char (line-beginning-position))
  (modal-insert-mode 1))

(defun modal-line-append()
  (interactive)
  (when (region-active-p)
    (deactivate-mark t))
  (goto-char (line-end-position))
  (modal-insert-mode 1))

(defun modal-quit-insert-mode()
  (interactive)
  (if (derived-mode-p 'special-mode)
      (modal-motion-mode 1)
    (modal-normal-mode 1)))

(defun modal--temporary-insert-callback()
  (unless (eq this-command #'modal-temporary-insert)
    (remove-hook 'post-command-hook #'modal--temporary-insert-callback t)
    (modal-quit-insert-mode)))

(defun modal-temporary-insert()
  (interactive)
  (modal-insert-mode 1)
  (add-hook 'post-command-hook #'modal--temporary-insert-callback 0 t))

(defun modal-previous-line (arg)
  (interactive "p")
  (previous-line arg))

(defun modal-next-line (arg)
  (interactive "p")
  (next-line arg))

(defun modal-forward-char (arg)
  (interactive "p")
  (let ((boundary-position (line-end-position)))
    (forward-char arg)
    (when (> (point) boundary-position)
      (goto-char boundary-position))))

(defun modal-backward-char (arg)
  (interactive "p")
  (let ((boundary-position (line-beginning-position)))
    (backward-char arg)
    (when (< (point) boundary-position)
      (goto-char boundary-position))))


(defun modal-previous-line-expand (arg)
  (interactive "p")
  (modal--set-mark)
  (if (= (point)
         (region-beginning))
      (previous-line arg)
    (exchange-point-and-mark)))

(defun modal-next-line-expand (arg)
  (interactive "p")
  (modal--set-mark)
  (if (= (point)
         (region-end))
      (next-line arg)
    (exchange-point-and-mark)))

(defun modal-forward-char-expand (arg)
  (interactive "p")
  (modal--set-mark)
  (if (= (point)
         (region-end))
      (modal-forward-char arg)
    (exchange-point-and-mark)))

(defun modal-backward-char-expand (arg)
  (interactive "p")
  (modal--set-mark)
  (if (= (point)
         (region-beginning))
      (modal-backward-char arg)
    (exchange-point-and-mark)))

(defun modal-mark-word(arg)
  (interactive "p")
  (let ((point (bounds-of-thing-at-point 'word)))
    (goto-char (cdr point))
    (push-mark (car point) t t)))

(defun modal-mark-symbol(arg)
  (interactive "p")
  (let ((point (bounds-of-thing-at-point 'symbol)))
    (goto-char (cdr point))
    (push-mark (car point) t t)))

(defun modal-forward-word(arg)
  (interactive "p")
  (forward-thing 'word arg))

(defun modal-backward-word(arg)
  (interactive "p")
  (forward-thing 'word (- arg)))

(defun modal-forward-word-expand(arg)
  (interactive "p")
  (modal--set-mark)
  (if (= (point)
         (region-end))
      (forward-thing 'word arg)
    (exchange-point-and-mark)))

(defun modal-backward-word-expand(arg)
  (interactive "p")
  (modal--set-mark)
  (if (= (point)
         (region-beginning))
      (forward-thing 'word (- arg))
    (exchange-point-and-mark)))


(defun modal-open-line-above(arg)
  (interactive "p")
  (goto-char (line-beginning-position))
  (newline arg)
  (backward-char arg)
  (indent-for-tab-command)
  (modal-insert-mode 1))

(defun modal-open-line-below(arg)
  (interactive "p")
  (goto-char (line-end-position))
  (newline arg)
  (indent-for-tab-command)
  (modal-insert-mode 1))

(defun modal-delete ()
  (interactive)
  (if (region-active-p)
      (delete-region (region-beginning)
                     (region-end))
    (delete-char 1)))

(defun modal-save-and-delete ()
  (interactive)
  (if (region-active-p)
      (clipboard-kill-region (region-beginning)
                             (region-end))
    (clipboard-kill-region (point)
                           (1+ (point)))))

(defun modal-change()
  (interactive)
  (if (region-active-p)
      (delete-region (region-beginning)
                     (region-end))
    (delete-region (point)
                   (1+ (point))))
  (modal-insert-mode 1))


(defun modal-save-and-change()
  (interactive)
  (if (region-active-p)
      (clipboard-kill-region (region-beginning)
                             (region-end))
    (clipboard-kill-region (point)
                           (1+ (point))))
  (modal-insert-mode 1))


(defun modal-diagnose()
  (interactive)
  (message
   "modal global state: %s\nmodal state: %s\nnormal state: %s\ninsert state: %s\nmotion state: %s"
   modal-global-mode modal-mode modal-normal-mode modal-insert-mode modal-motion-mode))

(provide 'core/modal)
;;; modal.el ends here
