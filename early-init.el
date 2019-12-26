(make-frame-invisible (selected-frame) t)
(add-hook 'after-init-hook #'make-frame-visible)
(add-hook 'after-init-hook (lambda()
                             (message "init ==============================")))
