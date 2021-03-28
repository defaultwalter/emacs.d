;;; filetree.el ---                                  -*- lexical-binding: t; -*-

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
  neotree
  :ensure t
  :defer t
  :custom                               ;
  (neo-smart-open t)
  (neo-window-width 35)
  (neo-mode-line-type 'none)
  (neo-vc-integration 'face)            ; 和doom主题冲突
  (neo-hide-cursor t)
  :bind                                 ;
  ("C-<tab>" . neotree-toggle)
  ("C-TAB" . neotree-toggle)
  :init                                 ;
  (user/leader-key "fv" '(neotree-toggle :name "file view")))


(provide 'filetree)
;;; filetree.el ends here
