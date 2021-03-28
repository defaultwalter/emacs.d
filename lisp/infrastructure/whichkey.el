;;; whichkey.el --- which key                        -*- lexical-binding: t; -*-

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
  which-key
  :ensure t
  :custom                               ;
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 5)
  (which-key-idle-secondary-delay 0.05)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-allow-multiple-replacements t)
  (which-key-allow-evil-operators t)
  (which-key-popup-type 'side-window)
  :config                               ;
  (which-key-mode t)
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
  (add-to-list 'which-key-replacement-alist '(("down" . nil) . ("down" . nil))))

(provide 'infrastructure/whichkey)
;;; whichkey.el ends here
