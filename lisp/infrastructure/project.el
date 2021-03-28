;;; project.el --- project                           -*- lexical-binding: t; -*-

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
  projectile                            ;project 插件
  :ensure t
  :custom       ;
  ;; (projectile-track-known-projects-automatically nil)
  ;; (projectile-indexing-method 'native)
  (projectile-sort-order 'access-time)
  (projectile-find-dir-includes-top-level t)
  :init                                 ;
  (user/leader-key "pk" '(project-kill-buffers :name "close all project buffers"))
  (user/leader-key "pi" '(projectile-project-info :name "project info"))
  (user/leader-key "pd" '(projectile-remove-known-project :name "remove project"))
  :config (projectile-mode +1))

(use-package
  counsel-projectile                    ;projectile 使用 counsel前端
  :ensure t
  :custom                               ;
  (counsel-projectile-sort-files t)
  (counsel-projectile-sort-directories t)
  (counsel-projectile-sort-buffers t)
  (counsel-projectile-sort-projects t)
  :init                                 ;
  (user/leader-key "pp" '(counsel-projectile-switch-project :name "switch project"))
  (user/leader-key "pf" '(counsel-projectile-find-file :name "find file in project"))
  (user/leader-key "ps" '(counsel-projectile-git-grep :name "search in project by git"))
  (user/leader-key "pS" '(counsel-projectile-grep :name "search in project"))
  :config (counsel-projectile-mode t))


(provide 'project)
;;; project.el ends here
