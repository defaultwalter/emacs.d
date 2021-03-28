;;; theme.el --- theme                               -*- lexical-binding: t; -*-

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
  all-the-icons
  :ensure t
  :init (setq all-the-icons-scale-factor 0.9))

(use-package
  solaire-mode
  :ensure t
  :if (display-graphic-p)
  :hook                                 ;
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config                               ;
  (set-face-background 'solaire-mode-line-face nil)
  (set-face-background 'solaire-mode-line-inactive-face nil)
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(use-package
  doom-themes
  :ensure t
  :init
  :custom                               ;
  (doom-themes-neotree-file-icons t)
  (doom-themes-treemacs-theme "doom-colors")
  :custom-face                          ;
  (font-lock-comment-face ((t
                            (:slant italic))))
  (neo-root-dir-face ((t
                       (:extend t))))
  :hook (server-after-make-frame . (lambda()
                                     (load-theme 'doom-one t)))
  :config                               ;
  (load-theme 'doom-one t)
  ;; (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))


(provide 'infrastructure/theme)
;;; theme.el ends here
