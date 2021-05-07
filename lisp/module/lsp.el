;;; lsp.el ---                                       -*- lexical-binding: t; -*-

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
  lsp-mode
  :ensure t
  :defer t
  :custom                               ;
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-auto-configure t)
  (lsp-log-io nil)
  (lsp-print-performance t)
  (lsp-file-watch-threshold 2000)
  (lsp-completion-provider :capf)
  (lsp-enable-snippet t)
  (lsp-idle-delay 0.5)
  ;; (lsp-keymap-prefix "SPC m l")
  ;; (lsp-eldoc-enable-hover nil)
  (lsp-enable-completion-at-point t)
  (lsp-keep-workspace-alive nil)
  (lsp-enable-file-watchers t)
  (lsp-enable-semantic-highlighting nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color t)
  (lsp-enable-folding nil)
  (lsp-enable-indentation t)
  (lsp-enable-on-type-formatting t)
  :hook ((lsp-mode . lsp-enable-which-key-integration)))
(use-package
  lsp-ui
  :ensure t
  :after lsp-mode
  :custom                               ;
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-border "#777777")
  :hook (lsp-mode . lsp-ui-mode))

(use-package
  flycheck
  :ensure t
  :defer t
  :hook (lsp-mode . flycheck-mode))

(provide 'module/lsp)
;;; lsp.el ends here
