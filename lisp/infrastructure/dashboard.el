;;; dashboard.el --- dashborard                      -*- lexical-binding: t; -*-

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
  dashboard
  :ensure t
  :config                               ;
  (user/leader-key "b <home>" '(dashboard-refresh-buffer :name "dashboard"))
  (setq dashboard-startup-banner (expand-file-name "dashboard-banner.txt" user-config-directory))
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '())
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons `(   ;
                                      () ;
                                      () ;
                                      () ;
                                      ((,(all-the-icons-octicon "calendar"
                                                                :height 0.9
                                                                :v-adjust 0.0)
                                        " Open agenda                         " "" (lambda
                                                                                     (&rest
                                                                                      _)
                                                                                     (call-interactively
                                                                                      'org-agenda))
                                        nil "" "")
                                       ("" "SPC n a   " "" (lambda
                                                             (&rest
                                                              _)
                                                             (call-interactively 'org-agenda))
                                        default "" ""))
                                      () ;
                                      ((,(all-the-icons-octicon "file-text"
                                                                :height 0.9
                                                                :v-adjust 0.0)
                                        " Open recently file                  " "" (lambda
                                                                                     (&rest
                                                                                      _)
                                                                                     (call-interactively
                                                                                      'counsel-recentf))
                                        nil "" "")
                                       ("" "SPC f r   " "" (lambda
                                                             (&rest
                                                              _)
                                                             (call-interactively 'counsel-recentf))
                                        default "" ""))
                                      () ;
                                      ((,(all-the-icons-octicon "briefcase"
                                                                :height 0.9
                                                                :v-adjust 0.0)
                                        " Open project                        " "" (lambda
                                                                                     (&rest
                                                                                      _)
                                                                                     (call-interactively
                                                                                      'counsel-projectile))
                                        nil "" "")
                                       ("" "SPC p p   " "" (lambda
                                                             (&rest
                                                              _)
                                                             (call-interactively
                                                              'counsel-projectile)) default "" ""))
                                      () ;
                                      ((,(all-the-icons-octicon "star"
                                                                :height 0.9
                                                                :v-adjust 0.0)
                                        " Jump to bookmark                    " "" (lambda
                                                                                     (&rest
                                                                                      _)
                                                                                     (call-interactively
                                                                                      'counsel-bookmark))
                                        nil "" "")
                                       ("" "SPC return" "" (lambda
                                                             (&rest
                                                              _)
                                                             (call-interactively 'counsel-bookmark))
                                        default "" ""))
                                      ()   ;
                                      ()   ;
                                      ())) ;
  ;;
  (setq dashboard-page-separator "")
  (setq dashboard-set-footer nil)
  (setq dashboard-items-default-length 20)
  ;; C/S mode use dashboard as default buffer
  (dashboard-setup-startup-hook))


(provide 'infrastructure/dashboard)
;;; dashboard.el ends here
