;;; modal-default.el --- modal default               -*- lexical-binding: t; -*-

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
(require 'dash)
(require 'modal-core)
(require 'modal-command)

(defun modal-setup-indicator ()
  "Setup indicator appending the return of function `meow-indicator' to the modeline.

This function should be called after you setup other parts of the mode-line and will work well for most cases.
If this function is not enough for your requirements, use `meow-indicator' to get the raw text for indicator and put it anywhere you want."
  (unless (-contains? mode-line-format
                      '(:eval (modal-indicator)))
    (setq-default mode-line-format (append
                                    '((:eval (modal-indicator)) " ")
                                    mode-line-format))))

(defun modal-setup()
  "Modal default keybinding"
  (set-keymap-parent modal-visual-state-map modal-motion-state-map)
  (set-keymap-parent modal-normal-state-map modal-motion-state-map)

;;;; Insert state
  (modal-insert-set-key "<escape>" #'modal-switch-to-default-state)

;;;; Motion state
  (modal-motion-set-key "j" #'modal-next-line)
  (modal-motion-set-key "k" #'modal-previous-line)
  (modal-motion-set-key "h" #'modal-backward-char)
  (modal-motion-set-key "l" #'modal-forward-char)
  (modal-motion-set-key ";" #'modal-move-between-line-head-and-tail)
  (modal-motion-set-key ":" #'modal-move-between-line-head-and-tail)
  (modal-motion-set-key "J" #'modal-select-to-next-line)
  (modal-motion-set-key "K" #'modal-select-to-previous-line)
  (modal-motion-set-key "H" #'modal-select-to-backward-char)
  (modal-motion-set-key "L" #'modal-select-to-forward-char)
  (modal-motion-set-key "w" #'modal-select-word)
  (modal-motion-set-key "W" #'modal-select-symbol)
  (modal-motion-set-key "s (" #'modal-select-inner-parentheses)
  (modal-motion-set-key "s )" #'modal-select-whole-parentheses)
  (modal-motion-set-key "s {" #'modal-select-inner-curly-brackets)
  (modal-motion-set-key "s }" #'modal-select-whole-curly-brackets)
  (modal-motion-set-key "s [" #'modal-select-inner-square-brackets)
  (modal-motion-set-key "s ]" #'modal-select-whole-square-brackets)
  (modal-motion-set-key "s '" #'modal-select-inner-string)
  (modal-motion-set-key "s \"" #'modal-select-whole-string)
  (modal-motion-set-key "s s" #'modal-select-inner-line)
  (modal-motion-set-key "s S" #'modal-select-whole-line)
  (modal-motion-set-key "S" #'modal-select-lines)
  (modal-motion-set-key "f" #'modal-forward-word)
  (modal-motion-set-key "b" #'modal-backward-word)
  (modal-motion-set-key "F" #'modal-select-to-forward-word)
  (modal-motion-set-key "B" #'modal-select-to-backward-word)
  (modal-motion-set-key "i" #'modal-insert)
  (modal-motion-set-key "a" #'modal-append)
  (modal-motion-set-key "I" #'modal-line-insert)
  (modal-motion-set-key "A" #'modal-line-append)
  (modal-motion-set-key "c" #'modal-save-and-change)
  (modal-motion-set-key "C" #'modal-change)
  (modal-motion-set-key "d" #'modal-save-and-delete-char)
  (modal-motion-set-key "D" #'modal-delete-char)
  (modal-motion-set-key "y" #'kill-ring-save)
  (modal-motion-set-key "p" #'yank)
  (modal-motion-set-key "u" #'undo)
  (modal-motion-set-key "o" #'modal-open-line-below)
  (modal-motion-set-key "O" #'modal-open-line-above)
  (modal-motion-set-key "g g" #'beginning-of-buffer)  (modal-motion-set-key "G" #'end-of-buffer)
  (modal-motion-set-key "M-h" #'windmove-left)
  (modal-motion-set-key "M-l" #'windmove-right)
  (modal-motion-set-key "M-j" #'windmove-down)
  (modal-motion-set-key "M-k" #'windmove-up)
  (modal-motion-set-key "<escape>" #'modal-temporary-insert)
;;;; Normal state
  (modal-normal-set-key "<escape>" #'keyboard-escape-quit)
;;;; Visual state
  (modal-visual-set-key "<escape>" #'modal-switch-to-default-state)
  (modal-visual-set-key "d" #'modal-save-and-delete)
  (modal-visual-set-key "D" #'modal-delete)
  (modal-visual-set-key ";" #'exchange-point-and-mark)
  (modal-visual-set-key ")" #'modal-insert-parentheses)
  (modal-visual-set-key "(" #'modal-insert-parentheses-with-space)
  (modal-visual-set-key "]" #'modal-insert-square-brackets)
  (modal-visual-set-key "[" #'modal-insert-square-brackets-with-space)
  (modal-visual-set-key "}" #'modal-insert-curly-brackets)
  (modal-visual-set-key "{" #'modal-insert-curly-brackets-with-space)
  (modal-visual-set-key "'" #'modal-insert-single-quotes)
  (modal-visual-set-key "\"" #'modal-insert-double-quotes)
  (modal-visual-set-key "`" #'modal-insert-back-quotes)
  (modal-visual-set-key "~" #'modal-delete-pair)


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
  (modal-leader-set-key "h RET" '(view-order-manuals :which-key "manuals"))
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
                                  :which-key "mode")))

(provide 'modal-default)
;;; modal-default.el ends here
