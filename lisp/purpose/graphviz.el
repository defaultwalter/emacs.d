;;; graphviz.el ---                                  -*- lexical-binding: t; -*-

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
  graphviz-dot-mode
  :ensure t
  :defer t
  :config (add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
  (setq-default graphviz-dot-indent-width 4)
  (add-hook 'org-mode-hook
            (lambda ()
              (if (boundp 'org-src-lang-modes)
                  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t))))


(provide 'purpose/graphviz)
;;; graphviz.el ends here
