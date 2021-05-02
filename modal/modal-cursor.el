;;; modal-cursor.el --- modal cursor                 -*- lexical-binding: t; -*-

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
(require 'cl-lib)

(defun tcursor--terminal-type()
  (when (not (display-graphic-p))
    (cond ((getenv "TMUX") 'tmux)
          ((getenv "XTERM_VERSION") 'xterm)
          ((string= (getenv "TERM_PROGRAM") "Apple_Terminal") 'apple-terminal)
          ((getenv "KONSOLE_PROFILE_NAME") 'konsole)
          ((string= (getenv "COLORTERM") "gnome-terminal") 'gnome-terminal)
          ( (string= (getenv "TERM_PROGRAM") "iTerm.app") 'iterm)
          ((string= (getenv "TERM") "dumb") 'dumb))))

(defun tcursor--in-tmux()
  (when (not (display-graphic-p))
    (getenv "TMUX")))

(defun tcursor--make-tmux-shape (seq)
  "Make escape sequence for tmux."
  ;; (let ((prefix "\ePtmux;\e")
  ;;       (suffix "\e\\"))
  ;;   (concat prefix seq suffix))
  seq)
(defun tcursor--make-konsole-cursor-shape (shape)
  "Make escape sequence for konsole."
  (let ((prefix  "\e]50;CursorShape=")
        (suffix  "\x7")
        (box     "0")
        (bar     "1")
        (hbar    "2")
        (seq     nil))
    (unless (member shape '(box bar hbar))
      (setq shape 'box))
    (cond ((eq shape 'box)
           (setq seq (concat prefix box suffix)))
          ((eq shape 'bar)
           (setq seq (concat prefix bar suffix)))
          ((eq shape 'hbar)
           (setq seq (concat prefix hbar suffix))))
    (if (tcursor--in-tmux)
        (tcursor--make-tmux-shape seq) seq)))

(defun tcursor--make-xterm-cursor-shape (shape)
  "Make escape sequence for XTerm."
  (let ((prefix      "\e[")
        (suffix      " q")
        (box-blink   "1")
        (box         "2")
        (hbar-blink  "3")
        (hbar        "4")
        (bar-blink   "5")
        (bar         "6")
        (seq        nil))
    (unless (member shape '(box bar hbar))
      (setq shape 'box))
    (cond ((eq shape 'box)
           (setq seq (concat prefix (if blink-cursor-mode box-blink box) suffix)))
          ((eq shape 'bar)
           (setq seq (concat prefix (if blink-cursor-mode bar-blink bar) suffix)))
          ((eq shape 'hbar)
           (setq seq (concat prefix (if blink-cursor-mode hbar-blink hbar) suffix))))
    (if (tcursor--in-tmux)
        (tcursor--make-tmux-shape seq) seq)))

(defun tcursor--make-cursor-shape (shape)
  "Make escape sequence for cursor shape."
  (let ((terminal-type (tcursor--terminal-type)))
    (cond ((eq terminal-type 'konsole)
           (tcursor--make-konsole-cursor-shape shape))
          (terminal-type (tcursor--make-xterm-cursor-shape shape))
          (t ""))))

(defun tcursor--make-cursor-color (color)
  "Make escape sequence for cursor color."
  (or (when-let ((hex-color (apply 'color-rgb-to-hex (color-name-to-rgb color)))
                 (terminal-type (tcursor--terminal-type))
                 (prefix (if (eq terminal-type 'iterm) "\e]Pl" "\e]12;"))
                 (suffix (if (eq terminal-type 'iterm) "\e\\" "\a")))
        ;; https://www.iterm2.com/documentation-escape-codes.html
        (concat prefix
                ;; https://www.iterm2.com/documentation-escape-codes.html
                ;; Remove #, rr, gg, bb are 2-digit hex value for iTerm.
                (if (and (tcursor--in-tmux)
                         (string-prefix-p "#" hex-color))
                    (substring hex-color 1) hex-color) suffix))
      ""))


(provide 'modal-cursor)
;;; modal-cursor.el ends here
