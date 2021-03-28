;;; leader.el --- leader key                         -*- lexical-binding: t; -*-

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
(require 'infrastructure/whichkey)

;;; #autoload
(use-package
  evil-leader
  :ensure t
  :after (evil which-key)
  :config                               ;
  (evil-leader/set-leader "<SPC>")
  ;; (setq evil-leader/in-all-states t)
  (global-evil-leader-mode t)
  (evil-mode t) ; 为了让 scratch/message 等buffer 能够使用 evil-leader，必须在 evil-leader 后启用 evil
  (user/leader-key "f"
                   '(:ignore t
                             :name "file"))
  (user/leader-key "ff" '(find-file :name "find file"))
  (user/leader-key "fs" '(save-buffer :name "save file"))
  (user/leader-key "fS" '(save-some-buffers :name "save all files"))
  (user/leader-key "fr" '(recentf-open-files :name "recent file"))
  (user/leader-key "f." '((lambda()
                            (interactive)
                            (dired user-config-directory)) :name "open configuration"))
  (user/leader-key "b"
                   '(:ignore t
                             :name "buffer"))
  (user/leader-key "bb" '(switch-to-buffer :name "switch buffer"))
  (user/leader-key "bs" '(save-buffer :name "save buffer"))
  (user/leader-key "bS" '(save-some-buffers :name "save all buffers"))
  (user/leader-key "bk" '(kill-this-buffer :name "kill buffer"))
  (user/leader-key "bK" '(kill-buffer-and-window :name "kill buffer&window"))
  (user/leader-key "bc" '(kill-this-buffer :name "kill buffer"))
  (user/leader-key "bC" '(kill-buffer-and-window :name "kill buffer&window"))
  (user/leader-key "b <end>" '((lambda()
                                 (interactive)
                                 (switch-to-buffer "*scratch*")) :name "scratch buffer"))
  (user/leader-key "c"
                   '(:ignore t
                             :name "content"))
  (user/leader-key "cc" '(comment-line :name "comment"))
  (user/leader-key "cr" '(comment-or-uncomment-region :name "comment region"))
  (user/leader-key "w"
                   '(:ignore t
                             :name "window"))
  (user/leader-key "ws" '(split-window-horizontally :name "split window horizontally"))
  (user/leader-key "wv" '(split-window-vertically :name "split window vertically"))
  (user/leader-key "wm" '(maximize-window :name "maximize window"))
  (user/leader-key "wn" '(minimize-window :name "minimize window"))
  (user/leader-key "wb" '(balance-windows :name "balance window"))
  (user/leader-key "wd" '(delete-window :name "delete window"))
  (user/leader-key "wD" '(delete-other-windows :name "delete other window"))
  (user/leader-key "wc" '(delete-window :name "delete window"))
  (user/leader-key "wC" '(delete-other-windows :name "delete other window"))
  (user/leader-key "h"
                   '(:ignore t
                             :name "help"))
  ;; (user/leader-key "h <return>" '(view-order-manuals :name "manuals"))
  (user/leader-key "h RET" '(view-order-manuals :name ("return" . "manuals")))
  (user/leader-key "hf" '(describe-function :name "describe function"))
  (user/leader-key "hv" '(describe-variable :name "describe variable"))
  (user/leader-key "hk" '(describe-key :name "describe key"))
  (user/leader-key "hc" '(describe-char :name "describe char"))
  (user/leader-key "hm" '(describe-mode :name "describe mode"))
  (user/leader-key "hp" '(describe-package :name "describe package"))
  (user/leader-key "hs" '(describe-symbol :name "describe symbol"))
  (user/leader-key "hw" '(where-is :name "where is"))
  (user/leader-key "h?" '(about-emacs :name "about"))
  (user/leader-key "g"
                   '(:ignore t
                             :name "goto"))
  (user/leader-key "p"
                   '(:ignore t
                             :name "project"))
  (user/leader-key "n"
                   '(:ignore t
                             :name "note"))
  (user/leader-key "na" '(org-agenda :name "agenda"))
  (user/leader-key "m"
                   '(:ignore t
                             :name "mode")))


(provide 'infrastructure/leader)
;;; leader.el ends here
