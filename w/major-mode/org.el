;;; package -- org-config
;;; Commentary:
;;; Code:

(setq-default org-agenda-files '("~/Nutstore Files/GTD"))
(use-package
  org
  :ensure org-plus-contrib
  :defer t
  :config (require 'ox-freemind)
  (require 'org-tempo))


;; (use-package
;;   org-bullets
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'org-mode-hook
;;                     (lambda ()
;;                       (org-bullets-mode 1))))
(use-package
  evil-org
  :ensure t
  :after org
  :config (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
(setq-default org-confirm-babel-evaluate nil)
(add-hook 'org-mode-hook
          (lambda ()
            (if (fboundp 'org-display-inline-images)
                (org-display-inline-images t t))
            (if (fboundp 'org-indent-mode)
                (org-indent-mode))
            (setq truncate-lines nil)))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)


(require 'ob-python)
(require 'ob-shell)
(require 'ob-plantuml)
(require 'ob-java)
(require 'ob-js)
(require 'ob-python)
(require 'ob-latex)
(require 'ob-dot)

(setq org-latex-compiler "xelatex")
(if (eq system-type 'darwin)
    (progn (setenv "PATH" (concat "/Library/TeX/texbin" (getenv "PATH")))
           (setq exec-path (append '("/Library/TeX/texbin") exec-path))))
(provide 'major-mode/org)
;;; org-config.el ends here
