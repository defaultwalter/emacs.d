(defvar w/wrap-region-wrap-map '(("(" . ("(" . ")"))
                                 (")" . ("(" . ")"))
                                 ("()" . ("(" . ")"))
                                 ("[" . ("[" . "]"))
                                 ("]" . ("[" . "]"))
                                 ("[]" . ("[" . "]"))
                                 ("{" . ("{" . "}"))
                                 ("}" . ("{" . "}"))
                                 ("{}" . ("{" . "}"))
                                 ("'" . ("'" . "'"))
                                 ("''" . ("'" . "'"))
                                 ("\"" . ("\"" . "\""))
                                 ("\"\"" . ("\"" . "\""))
                                 ) "Used by ‘w/wrap-region-match-wrap’")

(defun w/wrap-region--find-match-wrap(text)
  "Find match wrap"
  (cdr (car (delq nil (mapcar (lambda (x)
                      (when (string-equal text (car x))
                        x)) w/wrap-region-wrap-map)))))

(defun w/wrap-region(left-text right-text)
  "Wrap by left-text/righ-text"
  (let ((start (region-beginning))
        (end (region-end)))
    (if  (region-active-p)
        (save-excursion
          (goto-char end)
          (insert right-text)
          (goto-char start)
          (insert left-text)
          "")
      "")))

(defun w/wrap-region-wrap (text)
  "Wrap current selection by text"
  (interactive "sWrap: ")
  (w/wrap-region text text))

(defun w/wrap-region-match-wrap (text)
  "Wrap current selection by match text"
  (interactive "sWrap: ")
  (let* ((wrap-text (w/wrap-region--find-match-wrap text))
         (left-text (car wrap-text))
         (right-text (cdr wrap-text)))
    (if wrap-text (w/wrap-region left-text right-text)
      (w/wrap-region text text))))

;; 如果在mac 终端下使用emacs ,则使用pbpaste从clipboard 获取内容
(defadvice gui-backend-get-selection (around get-clip-from-terminal-on-osx activate)
  ad-do-it
  (when (and (equal system-type 'darwin)
             (not (display-graphic-p))
             (not (window-system))
             (equal (ad-get-arg 0) 'CLIPBOARD))
    (let ((default-directory "~/"))
      (setq ad-return-value (shell-command-to-string "pbpaste")))))

;; "+yy 设置内容到系统clipboard
;; 如果在mac 终端下使用emacs ,则使用pbpaste从clipboard 获取内容
(defadvice gui-backend-set-selection (around set-clip-from-terminal-on-osx activate)
  ad-do-it
  ;; (message "%s %s"  (ad-get-arg 0)  (ad-get-arg 1))
  (when (and (equal system-type 'darwin)
             (not (display-graphic-p))
             (not (window-system))
             (equal (ad-get-arg 0) 'CLIPBOARD))
    (let ((process-connection-type nil)   ; ; use pipe
          (default-directory "~/"))
      (let ((proc (start-process "pbcopy" nil "pbcopy")))
        (process-send-string proc (ad-get-arg 1))
        (process-send-eof proc)))))

(provide 'core/feature)
