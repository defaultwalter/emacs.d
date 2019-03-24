(setq comint-prompt-read-only t)
(define-key comint-mode-map (kbd "C-p") 'comint-previous-input)
(define-key comint-mode-map (kbd "C-n") 'comint-next-input)
(provide 'shell-config)
