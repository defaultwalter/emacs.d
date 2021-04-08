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

;;;; bind map

(define-key modal-normal-state-map (kbd "j") #'modal-next-line)
(define-key modal-normal-state-map (kbd "k") #'modal-previous-line)
(define-key modal-normal-state-map (kbd "h") #'modal-backward-char)
(define-key modal-normal-state-map (kbd "l") #'modal-forward-char)
(define-key modal-normal-state-map (kbd "J") #'forward-paragraph)
(define-key modal-normal-state-map (kbd "K") #'backward-paragraph)
(define-key modal-normal-state-map (kbd "H") #'move-beginning-of-line)
(define-key modal-normal-state-map (kbd "L") #'move-end-of-line)


(define-key modal-normal-state-map (kbd "w") #'modal-select-forward-word)
(define-key modal-normal-state-map (kbd "W") #'modal-select-backward-word)
(define-key modal-normal-state-map (kbd "s") #'modal-select-forward-symbol)
(define-key modal-normal-state-map (kbd "S") #'modal-select-backward-symbol)

(define-key modal-normal-state-map (kbd "v") #'modal-mark)
(define-key modal-normal-state-map (kbd "V") #'modal-mark-line)

(define-key modal-normal-state-map (kbd "e") #'modal-forward-word)
(define-key modal-normal-state-map (kbd "b") #'modal-backward-word)
(define-key modal-normal-state-map (kbd "E") #'modal-select-to-forward-word)
(define-key modal-normal-state-map (kbd "B") #'modal-select-to-backward-word)

(define-key modal-normal-state-map (kbd "i") #'modal-insert)
(define-key modal-normal-state-map (kbd "a") #'modal-append)
(define-key modal-normal-state-map (kbd "I") #'modal-line-insert)
(define-key modal-normal-state-map (kbd "A") #'modal-line-append)

(define-key modal-normal-state-map (kbd "c") #'modal-change)
(define-key modal-normal-state-map (kbd "C") #'modal-save-and-change)
(define-key modal-normal-state-map (kbd "d") #'modal-delete)
(define-key modal-normal-state-map (kbd "D") #'modal-save-and-delete)


(define-key modal-normal-state-map (kbd "o") #'modal-open-line-below)
(define-key modal-normal-state-map (kbd "O") #'modal-open-line-above)
(define-key modal-normal-state-map (read-kbd-macro "g l") #'move-end-of-line)
(define-key modal-normal-state-map (read-kbd-macro "g h") #'move-beginning-of-line)

(define-key modal-insert-state-map (kbd "<escape>") #'modal-quit-insert-mode)
(define-key modal-motion-state-map (kbd "<escape>") #'modal-temporary-insert)

(define-key modal-normal-state-map (kbd "M-h") #'windmove-left)
(define-key modal-normal-state-map (kbd "M-l") #'windmove-right)
(define-key modal-normal-state-map (kbd "M-j") #'windmove-down)
(define-key modal-normal-state-map (kbd "M-k") #'windmove-up)

(define-key modal-normal-state-map (kbd "`") #'exchange-point-and-mark)
(define-key modal-normal-state-map (kbd "~") #'exchange-point-and-mark)

(define-key modal-normal-state-map (kbd "z") #'outline-toggle-children)
;;;; leader key

(modal-leader-set-key "SPC" '(counsel-M-x :which-key "command"))
(modal-leader-set-key "f"
                      '(:ignore t
                                :which-key "file"))
(modal-leader-set-key "ff" '(find-file :which-key "find file"))
(modal-leader-set-key "fs" '(save-buffer :which-key "save file"))
(modal-leader-set-key "fS" '(save-some-buffers :which-key "save all files"))
(modal-leader-set-key "fr" '(recentf-open-files :which-key "recent file"))
(modal-leader-set-key "f." '((lambda()
                               (interactive)
                               (dired user-config-directory)) :which-key "open configuration"))
(modal-leader-set-key "b"
                      '(:ignore t
                                :which-key "buffer"))
(modal-leader-set-key "bb" '(switch-to-buffer :which-key "switch buffer"))
(modal-leader-set-key "bs" '(save-buffer :which-key "save buffer"))
(modal-leader-set-key "bS" '(save-some-buffers :which-key "save all buffers"))
(modal-leader-set-key "bk" '(kill-this-buffer :which-key "kill buffer"))
(modal-leader-set-key "bK" '(kill-buffer-and-window :which-key "kill buffer&window"))
(modal-leader-set-key "bc" '(kill-this-buffer :which-key "kill buffer"))
(modal-leader-set-key "bC" '(kill-buffer-and-window :which-key "kill buffer&window"))
(modal-leader-set-key "b <end>" '((lambda()
                                    (interactive)
                                    (switch-to-buffer "*scratch*")) :which-key "scratch buffer"))
(modal-leader-set-key "c"
                      '(:ignore t
                                :which-key "content"))
(modal-leader-set-key "cc" '(comment-line :which-key "comment"))
(modal-leader-set-key "cr" '(comment-or-uncomment-region :which-key "comment region"))
(modal-leader-set-key "w"
                      '(:ignore t
                                :which-key "window"))
(modal-leader-set-key "ws" '(split-window-horizontally :which-key "split window horizontally"))
(modal-leader-set-key "wv" '(split-window-vertically :which-key "split window vertically"))
(modal-leader-set-key "wm" '(maximize-window :which-key "maximize window"))
(modal-leader-set-key "wn" '(minimize-window :which-key "minimize window"))
(modal-leader-set-key "wb" '(balance-windows :which-key "balance window"))
(modal-leader-set-key "wd" '(delete-window :which-key "delete window"))
(modal-leader-set-key "wD" '(delete-other-windows :which-key "delete other window"))
(modal-leader-set-key "wc" '(delete-window :which-key "delete window"))
(modal-leader-set-key "wC" '(delete-other-windows :which-key "delete other window"))
(modal-leader-set-key "h"
                      '(:ignore t
                                :which-key "help"))
;; (modal-leader-set-key "h <return>" '(view-order-manuals :which-key "manuals"))
(modal-leader-set-key "h RET" '(view-order-manuals :which-key ("return" . "manuals")))
(modal-leader-set-key "hf" '(describe-function :which-key "describe function"))
(modal-leader-set-key "hv" '(describe-variable :which-key "describe variable"))
(modal-leader-set-key "hk" '(describe-key :which-key "describe key"))
(modal-leader-set-key "hc" '(describe-char :which-key "describe char"))
(modal-leader-set-key "hm" '(describe-mode :which-key "describe mode"))
(modal-leader-set-key "hp" '(describe-package :which-key "describe package"))
(modal-leader-set-key "hs" '(describe-symbol :which-key "describe symbol"))
(modal-leader-set-key "hw" '(where-is :which-key "where is"))
(modal-leader-set-key "h?" '(about-emacs :which-key "about"))
(modal-leader-set-key "g"
                      '(:ignore t
                                :which-key "goto"))
(modal-leader-set-key "p"
                      '(:ignore t
                                :which-key "project"))
(modal-leader-set-key "n"
                      '(:ignore t
                                :which-key "note"))
(modal-leader-set-key "na" '(org-agenda :which-key "agenda"))
(modal-leader-set-key "m"
                      '(:ignore t
                                :which-key "mode"))

(provide 'core/keybinding)
;;; keybinding.el ends here
