;;; completion.el --- completion                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Maf

;; Author: Maf <wmafire@gmail.com>
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

(use-package
  company
  :ensure t
  :defer t
  :hook ;; (prog-mode . company-mode)
  (after-init . global-company-mode)
  :init ;; Don't convert to downcase.
  (defun user/complete()
    (interactive)
    (or (yas/expand)
        (company-indent-or-complete-common nil)))
  (setq-default company-dabbrev-downcase nil)
  :bind (:map company-mode-map
              ("<tab>" . user/complete)
              ("TAB" . user/complete)
              ;;
              :map company-active-map   ;
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-s" . company-filter-candidates)
              ("<tab>" . company-complete-selection)
              ("TAB" . company-complete-selection)
              ("<return>" . company-complete-selection) ; 终端下无效
              ("RET" . company-complete-selection)      ; 终端下生效
              :map company-search-map                   ;
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("<tab>" . company-complete-selection)
              ("TAB" . company-complete-selection)
              ("<return>" . company-complete-selection) ; 终端下无效
              ("RET" . company-complete-selection))     ; 终端下生效
  :custom                                               ;
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.01)
  (company-echo-delay 0.2)
  (company-show-numbers nil)
  :config                               ;
  (setq company-selection-default 0)
  (setq company-backends '((company-capf company-dabbrev-code company-keywords company-files)
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



(provide 'infrastructure/completion)
;;; completion.el ends here
