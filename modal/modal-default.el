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


(defun modal-setup()
  "Modal default keybinding"
  (set-keymap-parent modal-visual-state-map modal-motion-state-map)
  (set-keymap-parent modal-normal-state-map modal-motion-state-map)

;;;; Insert state
  (modal-insert-set-key "<escape>" #'modal-switch-to-default-state)
  ;;   (modal-insert-set-key "C-z" #'undo)
  ;;   (modal-insert-set-key "C-S-z" #'redo)
  ;;   (modal-insert-set-key "C-x" #'modal-save-and-delete)
  ;;   (modal-insert-set-key "C-c" #'kill-ring-save)
  ;;   (modal-insert-set-key "C-v" #'yank)
  ;;   (modal-insert-set-key "C-s" #'save-buffer)
  ;;   (modal-insert-set-key "C-S-s" #'save-some-buffers)

  ;; ;;;; Motion state
  ;;   (modal-motion-set-key "C-z" #'undo)
  ;;   (modal-motion-set-key "C-S-z" #'redo)
  ;;   (modal-motion-set-key "C-x" #'modal-save-and-delete)
  ;;   (modal-motion-set-key "C-c" #'kill-ring-save)
  ;;   (modal-motion-set-key "C-v" #'yank)
  ;;   (modal-motion-set-key "C-s" #'save-buffer)
  ;;   (modal-motion-set-key "C-S-s" #'save-some-buffers)

  (modal-motion-set-key "j" #'modal-next-line)
  (modal-motion-set-key "k" #'modal-previous-line)
  (modal-motion-set-key "h" #'modal-left-char)
  (modal-motion-set-key "l" #'modal-right-char)
  (modal-motion-set-key "J" #'modal-select-to-next-line)
  (modal-motion-set-key "K" #'modal-select-to-previous-line)
  (modal-motion-set-key "H" #'modal-select-to-left-char)
  (modal-motion-set-key "L" #'modal-select-to-right-char)
  
  ;; (modal-motion-set-key "J" #'modal-goto-next-char)
  ;; (modal-motion-set-key "K" #'modal-goto-preview-char)
  ;; (modal-motion-set-key "H" #'beginning-of-line)
  ;; (modal-motion-set-key "L" #'end-of-line)

  (modal-motion-set-key "\\" #'modal-move-between-line-head-and-tail)

  (modal-motion-set-key "f" #'modal-forward-word)
  (modal-motion-set-key "b" #'modal-backward-word)
  (modal-motion-set-key "F" #'modal-forward-symbol)
  (modal-motion-set-key "B" #'modal-backward-symbol)

  (modal-motion-set-key "w" #'modal-select-word)
  (modal-motion-set-key "W" #'modal-select-symbol)
  (modal-motion-set-key "r" #'modal-replace)
  (modal-motion-set-key "R" #'modal-save-and-replace)
  (modal-motion-set-key "s(" #'modal-select-inner-parentheses)
  (modal-motion-set-key "s)" #'modal-select-whole-parentheses)
  (modal-motion-set-key "s{" #'modal-select-inner-curly-brackets)
  (modal-motion-set-key "s}" #'modal-select-whole-curly-brackets)
  (modal-motion-set-key "s[" #'modal-select-inner-square-brackets)
  (modal-motion-set-key "s]" #'modal-select-whole-square-brackets)
  (modal-motion-set-key "s'" #'modal-select-inner-string)
  (modal-motion-set-key "s\"" #'modal-select-whole-string)
  (modal-motion-set-key "ss" #'modal-secondary-selection)

  (modal-motion-set-key "S" #'modal-select-lines)

  (modal-motion-set-key "i" #'modal-insert)
  (modal-motion-set-key "a" #'modal-append)
  (modal-motion-set-key "I" #'modal-line-insert)
  (modal-motion-set-key "A" #'modal-line-append)
  (modal-motion-set-key "c" #'modal-change)
  (modal-motion-set-key "C" #'modal-save-and-change)
  (modal-motion-set-key "d" #'modal-delete-char)
  (modal-motion-set-key "D" #'modal-save-and-delete-char)
  (modal-motion-set-key "y" #'modal-save)
  (modal-motion-set-key "p" #'modal-paste-before)
  (modal-motion-set-key "P" #'modal-paste-after)
  (modal-motion-set-key "u" #'modal-undo)
  (modal-motion-set-key "U" #'modal-redo)
  (modal-motion-set-key "o" #'modal-open-line-below)
  (modal-motion-set-key "O" #'modal-open-line-above)

  (modal-motion-set-key "g h" #'beginning-of-line)
  (modal-motion-set-key "g k" #'beginning-of-buffer)
  (modal-motion-set-key "g j" #'end-of-buffer)
  (modal-motion-set-key "g l" #'end-of-line)
  (modal-motion-set-key "g ." #'last-buffer)

  (modal-motion-set-key "g d" #'xref-find-definitions)
  (modal-motion-set-key "g r" #'xref-find-references)

  (modal-motion-set-key "[w" #'modal-backward-word)
  (modal-motion-set-key "[s" #'modal-backward-symbol)
  (modal-motion-set-key "]w" #'modal-forward-word)
  (modal-motion-set-key "]s" #'modal-forward-symbol)


  (modal-motion-set-key "M-h" #'windmove-left)
  (modal-motion-set-key "M-l" #'windmove-right)
  (modal-motion-set-key "M-j" #'windmove-down)
  (modal-motion-set-key "M-k" #'windmove-up)

  (modal-motion-set-key "/" #'isearch-forward)
  (modal-motion-set-key "?" #'isearch-backward)
  (modal-motion-set-key "n" #'isearch-repeat-forward)
  (modal-motion-set-key "N" #'isearch-repeat-backward)

  (modal-motion-set-key "<escape>" #'modal-temporary-insert)
;;;; Normal state
  ;; (modal-normal-set-key-for-mode 'text-mode "DEL" #'backward-char)
  ;; (modal-normal-set-key "RET" #'forward-char)
  (modal-normal-set-key "<escape>" #'keyboard-escape-quit)
;;;; Visual state
  (modal-visual-set-key "<escape>" #'modal-switch-to-default-state)
  (modal-visual-set-key "d" #'modal-delete)
  (modal-visual-set-key "D" #'modal-save-and-delete)
  (modal-visual-set-key "e" #'modal-exchange-selection)
  (modal-visual-set-key "o" #'exchange-point-and-mark)

  (modal-visual-set-key ";(" #'modal-insert-parentheses-with-space)
  (modal-visual-set-key ";)" #'modal-insert-parentheses)
  (modal-visual-set-key ";[" #'modal-insert-square-brackets-with-space)
  (modal-visual-set-key ";]" #'modal-insert-square-brackets)
  (modal-visual-set-key ";{" #'modal-insert-curly-brackets-with-space)
  (modal-visual-set-key ";}" #'modal-insert-curly-brackets)
  (modal-visual-set-key ";'" #'modal-insert-single-quotes)
  (modal-visual-set-key ";\"" #'modal-insert-double-quotes)
  (modal-visual-set-key ";`" #'modal-insert-back-quotes)

  (modal-visual-set-key "C-<backspace>" #'modal-squeeze-selection)


;;;; leader key
  (modal-leader-set-key "." '((lambda()
                                (interactive)
                                (which-key-show-keymap 'global-map)
                                (set-transient-map global-map)) :which-key "global map"))
  (modal-leader-set-key "," '((lambda()
                                (interactive)
                                (let ((map (current-local-map)))
                                  (set-transient-map map)
                                  (which-key-show-keymap 'map)
                                  )) :which-key "local map"))

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
  (modal-leader-set-key "b" '(:ignore t :which-key "buffer"))
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
  (modal-leader-set-key "ww" '(window-toggle-side-windows :which-key "toggle side window"))
  (modal-leader-set-key "ws" '(split-window-horizontally :which-key "split window horizontally"))
  (modal-leader-set-key "wv" '(split-window-vertically :which-key "split window vertically"))
  (modal-leader-set-key "wm" '(maximize-window :which-key "maximize window"))
  (modal-leader-set-key "wn" '(minimize-window :which-key "minimize window"))
  (modal-leader-set-key "wb" '(balance-windows :which-key "balance window"))
  (modal-leader-set-key "wd" '(delete-window :which-key "delete window"))
  (modal-leader-set-key "wD" '(delete-other-windows :which-key "delete other window"))
  (modal-leader-set-key "wc" '(delete-window :which-key "delete window"))
  (modal-leader-set-key "wC" '(delete-other-windows :which-key "delete other window"))
  (modal-leader-set-key "wh" '(windmove-left :which-key "switch to left window"))
  (modal-leader-set-key "wl" '(windmove-right :which-key "switch to right window"))
  (modal-leader-set-key "wj" '(windmove-down :which-key "switch to down window"))
  (modal-leader-set-key "wk" '(windmove-up :which-key "switch to up window"))

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
