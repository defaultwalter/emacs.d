;;; plantuml.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  meetcw

;; Author: meetcw <meetcw@outlook.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'module/completion)

(use-package
  plantuml-mode
  :ensure t
  :defer t
  :config (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  ;; (setq-default plantuml-default-exec-mode 'jar)
  ;; (setq plantuml-jar-path "/Users/baiyan/.emacs.d/plantuml.jar")
  ;; (add-hook 'org-mode-hook (lambda ()
  ;;                            (plantuml-set-exec-mode "jar")))
  :custom                               ;
  (plantuml-default-exec-mode 'executable)
  (plantuml-jar-path "")
  :config;
  (add-hook 'org-mode-hook
            (lambda ()
              (if (boundp 'org-src-lang-modes)
                  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))))))


(provide 'purpose/plantuml)
;;; plantuml.el ends here
