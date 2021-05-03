;;; modal-option.el --- modal option                 -*- lexical-binding: t; -*-

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

(defgroup modal nil
  "Introduce modal editing"
  :tag "Modal"
  :prefix "modal-")

(defcustom modal-normal-cursor nil
  "Modal normal state cursor style."
  :group 'modal)
(defcustom modal-motion-cursor nil
  "Modal normal state cursor style."
  :group 'modal)
(defcustom modal-visual-cursor nil
  "Modal normal state cursor style."
  :group 'modal)
(defcustom modal-insert-cursor nil
  "Modal normal state cursor style."
  :group 'modal)

(defcustom modal-motion-mode-list '(special-mode)
  "Modal normal state cursor style."
  :type 'list
  :group 'modal)


(defcustom modal-leader-key "SPC"
  "Leader key"
  :type 'string
  :group 'modal)


(provide 'modal-option)
;;; modal-option.el ends here
