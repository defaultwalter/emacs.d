;;; keybinding.el --- keybinding                             -*- lexical-binding: t; -*-

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

(unless (package-installed-p 'which-key)
  (package-install 'which-key))
(require 'which-key)

(custom-set-variables '(which-key-idle-delay 0)
                      '(which-key-idle-secondary-delay 0.05)
                      '(which-key-sort-order 'which-key-prefix-then-key-order)
                      '(which-key-allow-multiple-replacements t)
                      '(which-key-allow-evil-operators t)
                      '(which-key-popup-type 'side-window))

(add-to-list 'which-key-replacement-alist '(("ESC" . nil) . ("esc" . nil)))
(add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("tab" . nil)))
(add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("return" . nil)))
(add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("delete" . nil)))
(add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))
(add-to-list 'which-key-replacement-alist '(("left" . nil) . ("left" . nil)))
(add-to-list 'which-key-replacement-alist '(("right" . nil) . ("right" . nil)))
(add-to-list 'which-key-replacement-alist '(("<left>" . nil) . ("left" . nil)))
(add-to-list 'which-key-replacement-alist '(("<right>" . nil) . ("right" . nil)))
(add-to-list 'which-key-replacement-alist '(("up" . nil) . ("up" . nil)))
(add-to-list 'which-key-replacement-alist '(("down" . nil) . ("down" . nil)))
(which-key-mode t)

(unless (package-installed-p 'use-package)
  (package-install 'general))
(require 'general)

(general-create-definer modal-set-leader-key
  :keymaps '(modal-normal-state-map modal-motion-state-map)
  :prefix "SPC")

(modal-set-leader-key "SPC" '(counsel-M-x :which-key "command"))
(modal-set-leader-key "f"
                      '(:ignore t
                                :which-key "file"))
(modal-set-leader-key "ff" '(find-file :which-key "find file"))
(modal-set-leader-key "fs" '(save-buffer :which-key "save file"))
(modal-set-leader-key "fS" '(save-some-buffers :which-key "save all files"))
(modal-set-leader-key "fr" '(recentf-open-files :which-key "recent file"))
(modal-set-leader-key "f." '((lambda()
                               (interactive)
                               (dired user-config-directory)) :which-key "open configuration"))
(modal-set-leader-key "b"
                      '(:ignore t
                                :which-key "buffer"))
(modal-set-leader-key "bb" '(switch-to-buffer :which-key "switch buffer"))
(modal-set-leader-key "bs" '(save-buffer :which-key "save buffer"))
(modal-set-leader-key "bS" '(save-some-buffers :which-key "save all buffers"))
(modal-set-leader-key "bk" '(kill-this-buffer :which-key "kill buffer"))
(modal-set-leader-key "bK" '(kill-buffer-and-window :which-key "kill buffer&window"))
(modal-set-leader-key "bc" '(kill-this-buffer :which-key "kill buffer"))
(modal-set-leader-key "bC" '(kill-buffer-and-window :which-key "kill buffer&window"))
(modal-set-leader-key "b <end>" '((lambda()
                                    (interactive)
                                    (switch-to-buffer "*scratch*")) :which-key "scratch buffer"))
(modal-set-leader-key "c"
                      '(:ignore t
                                :which-key "content"))
(modal-set-leader-key "cc" '(comment-line :which-key "comment"))
(modal-set-leader-key "cr" '(comment-or-uncomment-region :which-key "comment region"))
(modal-set-leader-key "w"
                      '(:ignore t
                                :which-key "window"))
(modal-set-leader-key "ws" '(split-window-horizontally :which-key "split window horizontally"))
(modal-set-leader-key "wv" '(split-window-vertically :which-key "split window vertically"))
(modal-set-leader-key "wm" '(maximize-window :which-key "maximize window"))
(modal-set-leader-key "wn" '(minimize-window :which-key "minimize window"))
(modal-set-leader-key "wb" '(balance-windows :which-key "balance window"))
(modal-set-leader-key "wd" '(delete-window :which-key "delete window"))
(modal-set-leader-key "wD" '(delete-other-windows :which-key "delete other window"))
(modal-set-leader-key "wc" '(delete-window :which-key "delete window"))
(modal-set-leader-key "wC" '(delete-other-windows :which-key "delete other window"))
(modal-set-leader-key "h"
                      '(:ignore t
                                :which-key "help"))
;; (modal-set-leader-key "h <return>" '(view-order-manuals :which-key "manuals"))
(modal-set-leader-key "h RET" '(view-order-manuals :which-key ("return" . "manuals")))
(modal-set-leader-key "hf" '(describe-function :which-key "describe function"))
(modal-set-leader-key "hv" '(describe-variable :which-key "describe variable"))
(modal-set-leader-key "hk" '(describe-key :which-key "describe key"))
(modal-set-leader-key "hc" '(describe-char :which-key "describe char"))
(modal-set-leader-key "hm" '(describe-mode :which-key "describe mode"))
(modal-set-leader-key "hp" '(describe-package :which-key "describe package"))
(modal-set-leader-key "hs" '(describe-symbol :which-key "describe symbol"))
(modal-set-leader-key "hw" '(where-is :which-key "where is"))
(modal-set-leader-key "h?" '(about-emacs :which-key "about"))
(modal-set-leader-key "g"
                      '(:ignore t
                                :which-key "goto"))
(modal-set-leader-key "p"
                      '(:ignore t
                                :which-key "project"))
(modal-set-leader-key "n"
                      '(:ignore t
                                :which-key "note"))
(modal-set-leader-key "na" '(org-agenda :which-key "agenda"))
(modal-set-leader-key "m"
                      '(:ignore t
                                :which-key "mode"))

(provide 'core/keybinding)
;;; keybinding.el ends here
