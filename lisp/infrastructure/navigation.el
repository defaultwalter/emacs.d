;;; navigation.el --- navigation                     -*- lexical-binding: t; -*-

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
  avy
  :ensure t
  :defer t
  :init                                 ;
  (with-eval-after-load "evil" (evil-define-key 'normal 'global "gc" 'avy-goto-char)))

(use-package
  ace-window                            ; 窗口跳转
  :ensure t
  :defer t
  :init                                 ;
  (user/leader-key "ww" '(ace-window :name "select window"))
  :config (setq aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f ?g)))


(provide 'navigation)
;;; navigation.el ends here
