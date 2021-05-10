;;; completion.el ---                                -*- lexical-binding: t; -*-

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

;; 自动完成
(use-package
  company
  :ensure t
  :defer t
  :hook                                 ;
  (prog-mode . company-mode)
  (conf-mode . company-mode)
  :init ;; Don't convert to downcase.
  (defun +company-set-complete()
    (interactive)
    (or (yas/expand)
        (company-indent-or-complete-common nil)))
  (setq-default company-dabbrev-downcase nil)
  :bind (:map company-mode-map
              ;; ("<tab>" . +company-set-complete)
              ;; ("TAB" . +company-set-complete)
              ;;
              :map company-active-map   ;
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-s" . company-filter-candidates)
              ("<tab>" . company-complete-selection)
              ("TAB" . company-complete-selection)
              ("<return>" . company-complete-selection)
              ("RET" . company-complete-selection)
              :map company-search-map   ;
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("<tab>" . company-complete-selection)
              ("TAB" . company-complete-selection)
              ("<return>" . company-complete-selection)
              ("RET" . company-complete-selection))
  :custom                               ;
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.01)
  (company-echo-delay 0.2)
  (company-show-numbers t)
  :config                               ;
  (setq company-selection-default 0)
  (setq company-backends '(;; (:separate company-yasnippet
                           ;;            company-capf)
                           (company-capf company-dabbrev-code company-keywords company-files)
                           (company-dabbrev)))
  (setq company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)))


(use-package
  company-box
  :ensure t
  :requires company
  :hook (company-mode . company-box-mode)
  :init                                 ;
  (setq company-box-show-single-candidate t)
  :config)

(use-package
  company-prescient
  :ensure t
  :after company
  :hook (company-mode . company-prescient-mode))


(provide 'module/completion)
;;; completion.el ends here
