;;; modal.el --- My modal edit -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
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


(defvar modal-motion-state-map
  (let ((keymap (make-keymap)))
    ;; (define-key keymap (kbd "<escape>") 'meow-insert-exit)
    keymap)
  "Keymap for Modal motion state.")

(defvar modal-normal-state-map (let ((keymap (make-keymap)))
                                 (suppress-keymap keymap t) keymap)
  "Keymap for Modal normal state.")

(define-minor-mode modal-normal-mode "Modal normal state."
  nil
  " ModalNormal"
  modal-normal-state-map
  (when modal-normal-mode               ;
    (modal-insert-mode -1)
    (modal-motion-mode -1)
    (setq-local cursor-type 'hollow)))

(define-minor-mode modal-insert-mode "Modal insert state."
  nil
  " ModalInsert"
  modal-insert-state-map
  (when modal-insert-mode               ;
    (modal-normal-mode -1)
    (modal-motion-mode -1)
    (setq-local cursor-type '(bar . 1))))

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


;;; command
(defun modal-insert-quit()
  (interactive)
  (if (derived-mode-p 'special-mode)
      (modal-motion-mode 1)
    (modal-normal-mode 1)))

(defun modal--set-mark()
  (when (not (region-active-p))
    (set-mark-command nil)))

(defun modal-mark-and-forward-char(arg)
  (interactive "p")
  (call-interactively #'forward-char))

(defun modal-mark-and-backward-char(arg)
  (interactive "p")
  (modal--set-mark)
  (call-interactively #'backward-char))

(defun modal-mark-and-forward-char(arg)
  (interactive "p")
  (modal--set-mark)
  (call-interactively #'forward-char))

(defun modal-mark-and-previous-line(arg)
  (interactive "p")
  (modal--set-mark)
  (call-interactively #'previous-line))

(defun modal-mark-and-next-line(arg)
  (interactive "p")
  (modal--set-mark)
  (call-interactively #'next-line))

(defun modal-mark-and-next-line(arg)
  (interactive "p")
  (modal--set-mark)
  (call-interactively #'next-line))

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
  (let ((arg (- 0 arg)))
    (forward-thing 'word arg)))


(defun modal-diagnose()
  (interactive)
  (message
   "modal global state: %s\nmodal state: %s\nnormal state: %s\ninsert state: %s\nmotion state: %s"
   modal-global-mode modal-mode modal-normal-mode modal-insert-mode modal-motion-mode))

;;; bind map
(define-key modal-normal-state-map (kbd "j") #'next-line)
(define-key modal-normal-state-map (kbd "k") #'previous-line)
(define-key modal-normal-state-map (kbd "h") #'backward-char)
(define-key modal-normal-state-map (kbd "l") #'forward-char)
(define-key modal-normal-state-map (kbd "L") #'modal-mark-and-forward-char)
(define-key modal-normal-state-map (kbd "K") #'modal-mark-and-previous-line)
(define-key modal-normal-state-map (kbd "J") #'modal-mark-and-next-line)
(define-key modal-normal-state-map (kbd "H") #'modal-mark-and-backward-char)


(define-key modal-normal-state-map (kbd "w") #'modal-mark-word)
(define-key modal-normal-state-map (kbd "W") #'backward-word)
(define-key modal-normal-state-map (kbd "s") #'modal-mark-symbol)
(define-key modal-normal-state-map (kbd "S") #'backward-symbol)

(define-key modal-normal-state-map (kbd "i") #'modal-insert-mode)
(define-key modal-insert-state-map (kbd "<escape>") #'modal-normal-mode)

;;; leader key


;;; misc
(modal-global-mode 1)
(cua-mode 1)
(provide 'core/modal)
;;; modal.el ends here
