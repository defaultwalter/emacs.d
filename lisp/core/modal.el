;;; modal.el --- My modal edit -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; which-key
(unless (package-installed-p 'which-key)
  (package-install 'which-key))
(require 'which-key)

(custom-set-variables '(which-key-show-early-on-C-h t)
                      '(which-key-idle-delay 10)
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

(defcustom modal-normal-cursor nil
  "Modal normal state cursor style."
  :group 'modal)
(defcustom modal-motion-cursor nil
  "Modal normal state cursor style."
  :group 'modal)
(defcustom modal-visual-cursor nil
  "Modal normal state cursor style."
  :group 'modal)
(defcustom modal-insert-cursor nil
  "Modal normal state cursor style."
  :group 'modal)


(defvar modal-mode-map (make-sparse-keymap)
  "Global keymap for Modal")

(defvar modal-insert-state-map
  (let ((keymap (make-keymap)))
    ;; (define-key keymap (kbd "<escape>") 'meow-insert-exit)
    keymap)
  "Keymap for Modal insert state.")
(defvar modal-motion-state-map (let ((keymap (make-keymap)))
                                 (suppress-keymap keymap t) keymap)
  "Keymap for Modal motion state.")

(defvar modal-normal-state-map
  (let ((keymap (make-sparse-keymap)))
    (suppress-keymap keymap t) keymap)
  "Keymap for Modal normal state.")

(defvar modal-visual-state-map (let ((keymap (make-keymap)))
                                 (suppress-keymap keymap t) keymap)
  "Keymap for Modal visual state.")


(defvar modal--normal-mode-maps nil
  "Modal map for mode")

(defvar modal--motion-mode-maps nil
  "Modal map for mode")

(defvar modal--visual-mode-maps nil
  "Modal map for mode")



(define-minor-mode modal-normal-mode "Modal normal state."
  nil
  " ModalNormal"
  modal-normal-state-map
  (when modal-normal-mode               ;
    (modal-insert-mode -1)
    (modal-motion-mode -1)
    (modal-visual-mode -1)
    (set-cursor-color (face-foreground 'default))))

(define-minor-mode modal-motion-mode "Modal motion state."
  nil
  " ModalMotion"
  modal-motion-state-map
  (when modal-motion-mode               ;
    (modal-insert-mode -1)
    (modal-normal-mode -1)
    (modal-visual-mode -1)))

(define-minor-mode modal-visual-mode "Modal motion state."
  nil
  " ModalMotion"
  modal-visual-state-map
  (when modal-visual-mode               ;
    (modal-insert-mode -1)
    (modal-normal-mode -1)
    (modal-motion-mode -1)
    (add-hook 'post-command-hook #'modal--visual-mode-post-command-handler nil t)
    (add-hook 'deactivate-mark-hook #'modal--exit-visual nil t))
  (unless modal-visual-mode (remove-hook 'post-command-hook
                                         #'modal--visual-mode-post-command-handler t)
          (remove-hook 'deactivate-mark-hook #'modal--exit-visual t)))


(defun modal--exit-visual()
  (deactivate-mark)
  (modal--quit-edit))

(defun modal--visual-mode-post-command-handler
    (&optional
     command)
  (when modal-visual-mode (let ((command (or command
                                             this-command)))
                            (when (or (eq command #'keyboard-quit)
                                      deactivate-mark
                                      (not (region-active-p)))
                              (deactivate-mark)
                              (modal--quit-edit) ))))

(define-minor-mode modal-insert-mode "Modal insert state."
  nil
  " ModalInsert"
  modal-insert-state-map
  (when modal-insert-mode               ;
    (modal-normal-mode -1)
    (modal-motion-mode -1)
    (modal-visual-mode -1)
    (add-hook 'post-command-hook #'modal--insert-mode-post-command-handler nil t))
  (unless modal-insert-mode             ;
    (remove-hook 'post-command-hook #'modal--insert-mode-post-command-handler t)))

(defun modal--insert-mode-post-command-handler
    (&optional
     command)
  (when modal-insert-mode (let ((command (or command
                                             this-command)))
                            (when  (eq command #'keyboard-quit)
                              (modal--quit-edit) ))))


(defun modal--switch-visual-state()
  (modal--switch-state 'visual))

(defun modal--refresh-cursor()
  "Change cursor color."
  (let ((cursor-style (cond ((bound-and-true-p modal-normal-mode)
                             (or modal-normal-cursor
                                 `(bar . ,(face-foreground 'link))))
                            ((bound-and-true-p modal-motion-mode)
                             (or modal-motion-cursor
                                 `(bar . ,(face-foreground 'success))))
                            ((bound-and-true-p modal-visual-mode)
                             (or modal-visual-cursor
                                 `(bar . ,(face-foreground 'warning))))
                            ((bound-and-true-p modal-insert-mode)
                             (or modal-insert-cursor
                                 `(bar . ,(face-foreground 'error))))
                            (t `(bar . ,(face-foreground 'default))))))
    (setq-local cursor-type (car cursor-style))
    (set-cursor-color (cdr cursor-style))))

;;;; define mode
(defun modal--enable ()
  "Enable Modal mode"
  (if (derived-mode-p 'special-mode)
      (modal-motion-mode 1)
    (modal-normal-mode 1))
  (add-hook 'modal-normal-mode-hook #'modal--refresh-cursor nil t)
  (add-hook 'modal-motion-mode-hook #'modal--refresh-cursor nil t)
  (add-hook 'modal-visual-mode-hook #'modal--refresh-cursor nil t)
  (add-hook 'modal-insert-mode-hook #'modal--refresh-cursor nil t)
  (add-hook 'activate-mark-hook #'modal--switch-visual-state nil t))

(defun modal--disable ()
  "Disable Modal mode"
  (modal-normal-mode -1)
  (modal-motion-mode -1)
  (modal-insert-mode -1)
  (modal-visual-mode -1)
  (remove-hook 'activate-mark-hook #'modal--switch-visual-state t))


(define-minor-mode modal-mode "Toggle `modal-mode` minor mode."
  nil
  " Modal"
  modal-mode-map
  (if modal-mode                        ;
      (modal--enable)
    (modal--disable)))

(defun modal--current-state ()
  "Current state."
  (cond ((bound-and-true-p modal-insert-mode) 'insert)
        ((bound-and-true-p modal-normal-mode) 'normal)
        ((bound-and-true-p modal-motion-mode) 'motion)
        ((bound-and-true-p modal-visual-mode) 'visual)))

(defun modal--switch-state (state)
  "Switch state."
  (cond ((equal state 'normal)
         (modal-normal-mode 1))
        ((equal state 'motion)
         (modal-motion-mode 1))
        ((equal state 'insert)
         (modal-insert-mode 1))
        ((equal state 'visual)
         (modal-visual-mode 1))))


(defun modal--quit-edit ()
  "Quit edit mode,"
  (if (derived-mode-p 'special-mode)
      (modal-motion-mode 1)
    (modal-normal-mode 1)))

(defun modal--window-state-change-handler
    (&rest
     args)
  "Update modal state when Window state change."
  (modal--refresh-cursor))

(defun modal-mode--minibuffer-setup()
  "Set modal-mode state when minibuffer avtive."
  (setq-local modal-normal-mode nil))

(defun modal--global-enable()
  "Enable Modal global mode"
  (add-hook 'window-state-change-functions #'modal--window-state-change-handler)
  (add-hook 'minibuffer-setup-hook #'modal-mode--minibuffer-setup))

(defun modal--global-disable()
  "Disable Modal global mode"
  (remove-hook 'window-state-change-functions #'modal--window-state-change-handler)
  (remove-hook 'minibuffer-setup-hook #'modal-mode--minibuffer-setup))

(defun modal--turn-on()
  (unless (minibufferp)
    (modal-mode 1)))
(define-globalized-minor-mode modal-global-mode modal-mode
  modal--turn-on
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

(defvar modal-leader--mode-maps nil
  "Leader map for mode")

(defvar modal-leader--default-map (make-sparse-keymap)
  "Keymap used for mode-independent leader bindings.")

(define-key modal-normal-state-map (read-kbd-macro modal-leader-key) modal-leader--default-map)
(define-key modal-motion-state-map (read-kbd-macro modal-leader-key) modal-leader--default-map)
(define-key modal-visual-state-map (read-kbd-macro modal-leader-key) modal-leader--default-map)

(defun modal-leader--apply-mode-map()
  "Apply mode map"
  (let ((normal-mode-map (or (cdr (assoc major-mode modal--normal-mode-maps))
                             modal-normal-state-map))
        (motion-mode-map (or (cdr (assoc major-mode modal--motion-mode-maps))
                             modal-motion-state-map))
        (visual-mode-map (or (cdr (assoc major-mode modal--visual-mode-maps))
                             modal-visual-state-map)))
    (push `(modal-normal-mode . ,normal-mode-map) minor-mode-overriding-map-alist)
    (push `(modal-motion-mode . ,motion-mode-map) minor-mode-overriding-map-alist)
    (push `(modal-visual-mode . ,visual-mode-map) minor-mode-overriding-map-alist)))

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
         (visual-mode-map (cdr (assoc mode modal--visual-mode-maps)))
         (leader-map (cdr (assoc mode modal-leader--mode-maps))))
    (unless normal-mode-map
      (setq normal-mode-map (make-sparse-keymap))
      (set-keymap-parent normal-mode-map modal-normal-state-map)
      (push (cons mode normal-mode-map) modal--normal-mode-maps))
    (unless motion-mode-map
      (setq motion-mode-map (make-sparse-keymap))
      (set-keymap-parent motion-mode-map modal-motion-state-map)
      (push (cons mode motion-mode-map) modal--motion-mode-maps))
    (unless visual-mode-map
      (setq visual-mode-map (make-sparse-keymap))
      (set-keymap-parent visual-mode-map modal-visual-state-map)
      (push (cons mode motion-mode-map) modal--motion-mode-maps))
    (unless leader-map
      (setq leader-map (make-sparse-keymap))
      (set-keymap-parent leader-map modal-leader--default-map)
      (push (cons mode leader-map) modal-leader--mode-maps)
      (define-key normal-mode-map (read-kbd-macro modal-leader-key) leader-map)
      (define-key motion-mode-map (read-kbd-macro modal-leader-key) leader-map)
      (define-key visual-mode-map (read-kbd-macro modal-leader-key) leader-map))
    (modal-leader--define-key leader-map key def)))

;;;; indicator
(defvar modal-indicator-alist
  '((normal . "<N>")
    (motion . "<M>")
    (visual . "<V>")
    (insert . "<I>")))
(defface modal-indicator-normal '((t
                                   (:inherit link
                                             :weight bold
                                             :underline nil)))
  "Normal state indicator."
  :group 'modal)

(defface modal-indicator-motion '((t
                                   (:inherit success
                                             :weight bold)))
  "Motion state indicator."
  :group 'modal)

(defface modal-indicator-visual '((t
                                   (:inherit warning
                                             :weight bold)))
  "Motion state indicator."
  :group 'modal)

(defface modal-indicator-insert '((t
                                   (:inherit error
                                             :weight bold)))
  "Insert state indicator."
  :group 'modal)

(defun modal-indicator()
  (let* ((current-state (modal--current-state))
         (state-text (cdr (assoc current-state modal-indicator-alist)))
         (state-face (intern (format "modal-indicator-%s" (symbol-name current-state)))))
    (propertize (format "%s " state-text) 'face state-face)))

;;;; function

(defun modal--set-mark()
  (when (not (region-active-p))
    (push-mark (point) t t)))

;;;; command
(defun modal-quit-insert-mode()
  (interactive)
  (if (derived-mode-p 'special-mode)
      (modal-motion-mode 1)
    (modal-normal-mode 1)))
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
(defun modal--temporary-insert-callback()
  (unless (eq this-command #'modal-temporary-insert)
    (remove-hook 'post-command-hook #'modal--temporary-insert-callback t)
    (modal-quit-insert-mode)))
(defun modal-temporary-insert()
  (interactive)
  (modal-insert-mode 1)
  (add-hook 'post-command-hook #'modal--temporary-insert-callback 0 t))

(defun modal-diagnose()
  (interactive)
  (message
   "modal global state: %s\nmodal state: %s\nnormal state: %s\nvisual state: %s\ninsert state: %s\nmotion state: %s"
   modal-global-mode modal-mode modal-normal-mode modal-visual-mode modal-insert-mode
   modal-motion-mode))

;;;;; motion
(defun modal-previous-line (arg)
  (interactive "p")
  (setq this-command #'previous-line)
  (previous-line arg))

(defun modal-next-line (arg)
  (interactive "p")
  (setq this-command #'next-line)
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

(defun modal-forward-word(arg)
  (interactive "p")
  (forward-thing 'word arg))

(defun modal-backward-word(arg)
  (interactive "p")
  (forward-thing 'word (- arg)))

;;;;; select
(defun modal-select-word()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'word)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))

(defun modal-select-symbol()
  (interactive )
  (let ((position (bounds-of-thing-at-point 'symbol)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))

(defun modal-select-inner-line()
  (interactive )
  (goto-char (line-end-position))
  (push-mark (line-beginning-position) t t))
(defun modal-select-whole-line()
  (interactive )
  (let ((position (bounds-of-thing-at-point 'line)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))

(defun modal-select-forward-word(arg)
  (interactive "p")
  (forward-thing 'word arg)
  (let ((position (bounds-of-thing-at-point 'word)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))

(defun modal-select-backward-word(arg)
  (interactive "p")
  (forward-thing 'word (- arg))
  (let ((position (bounds-of-thing-at-point 'word)))
    (goto-char (car position))
    (push-mark (cdr position) t t)))

(defun modal-select-forward-symbol(arg)
  (interactive "p")
  (forward-thing 'word arg)
  (let ((position (bounds-of-thing-at-point 'symbol)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))
(defun modal-select-backward-symbol(arg)
  (interactive "p")
  (forward-thing 'word (- arg))
  (let ((position (bounds-of-thing-at-point 'symbol)))
    (goto-char (car position))
    (push-mark (cdr position) t t)))

(defun modal-select-inner-parentheses()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'parentheses)))
    (goto-char (1- (cdr position)))
    (push-mark (1+ (car position)) t t)))
(defun modal-select-whole-parentheses()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'parentheses)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))

(defun modal-select-inner-square-brackets()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'square-brackets)))
    (goto-char (1- (cdr position)))
    (push-mark (1+ (car position)) t t)))
(defun modal-select-whole-square-brackets()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'square-brackets)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))

(defun modal-select-inner-curly-brackets()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'curly-brackets)))
    (goto-char (1- (cdr position)))
    (push-mark (1+ (car position)) t t)))

(defun modal-select-whole-curly-brackets()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'curly-brackets)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))

(defun modal-select-inner-string()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'string)))
    (goto-char (1- (cdr position)))
    (push-mark (1+ (car position)) t t)))

(defun modal-select-whole-string()
  (interactive)
  (when-let ((position (bounds-of-thing-at-point 'string)))
    (goto-char (cdr position))
    (push-mark (car position) t t)))

;;;;; select to
(defun modal-select-to-forward-char(arg)
  (interactive "p")
  (unless (region-active-p)
    (push-mark (point) t t))
  (modal-forward-char arg))

(defun modal-select-to-backward-char(arg)
  (interactive "p")
  (unless (region-active-p)
    (push-mark (point) t t))
  (modal-backward-char arg))

(defun modal-select-to-forward-word(arg)
  (interactive "p")
  (unless (region-active-p)
    (push-mark (point) t t)  )
  (forward-thing 'word arg))

(defun modal-select-to-backward-word(arg)
  (interactive "p")
  (unless (region-active-p)
    (push-mark (point) t t)  )
  (forward-thing 'word (- arg)))

(defun modal-select-to-next-line(arg)
  (interactive "p")
  (unless (region-active-p)
    (push-mark (point) t t))
  (modal-next-line arg))

(defun modal-select-to-previous-line(arg)
  (interactive "p")
  (unless (region-active-p)
    (push-mark (point) t t))
  (modal-previous-line arg))

(defun modal-select-lines()
  (interactive)
  (if (not (region-active-p))
      (modal-select-whole-line)
    (cond ((= (point)
              (region-end))
           (let ((position (bounds-of-thing-at-point 'line)))
             (goto-char (cdr position)))
           (save-excursion (goto-char (region-beginning))
                           (push-mark (line-beginning-position) t t)))
          ((= (point)
              (region-beginning))
           (goto-char (line-beginning-position))
           (save-excursion (goto-char (region-end))
                           (let ((position (bounds-of-thing-at-point 'line)))
                             (push-mark (cdr position) t t)))))))

;;;;; modify
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

(defun modal-delete-char (arg)
  (interactive "p")
  (delete-char 1))
(defun modal-save-and-delete-char (arg)
  (interactive "p")
  (kill-region (point)
               (1+ (point))))

(defun modal-delete ()
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning)
                   (region-end))))

(defun modal-save-and-delete ()
  (interactive)
  (when (region-active-p)
    (kill-region (region-beginning)
                 (region-end))))

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
      (kill-region (region-beginning)
                   (region-end))
    (kill-region (point)
                 (1+ (point))))
  (modal-insert-mode 1))

(defun modal-delete-boundary()
  (interactive)
  (when (region-active-p)
    (delete-char 1)))

(defun modal--suround-insert (begin end)
  (when (region-active-p)
    (save-excursion (goto-char (region-end))
                    (insert end)
                    (goto-char (region-beginning))
                    (insert begin))))

(defun modal-suround-insert-par()
  (interactive))

(provide 'core/modal)
;;; modal.el ends here
