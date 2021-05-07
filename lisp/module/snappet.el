;;; snappet.el ---                                   -*- lexical-binding: t; -*-

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

(use-package
  yasnippet
  :ensure t
  :defer t
  :hook (prog-mode . yas-minor-mode)
  :config                               ;
  (use-package
    yasnippet-snippets
    :ensure t
    :config (yas-reload-all)))

(use-package
  ivy-yasnippet
  :ensure t
  :after (yasnippet ivy)
  :defer t)


(provide 'module/snappet)
;;; snappet.el ends here
