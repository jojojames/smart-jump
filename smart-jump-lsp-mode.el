;;; smart-jump-lsp-mode.el --- Register `lsp-mode' for `smart-jump'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen, Terje Larsen

;; Author: James Nguyen <james@jojojames.com>, Terje Larsen <terlar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>, Terje Larsen <terlar@gmail.com>
;; URL: https://github.com/jojojames/smart-jump
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: emacs, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Register `lsp-mode' for `smart-jump'.

;;; Code:
(require 'smart-jump)
(require 'lsp-mode nil t)

(defcustom smart-jump-lsp-mode-order 0
  "The order `lsp-mode' runs during a `smart-jump'.

This needs to be set before `smart-jump-lsp-mode-register' is called."
  :type 'integer
  :group 'smart-jump)

;;;###autoload
(defun smart-jump-lsp-mode-register ()
  "Register `lsp-mode' for `smart-jump'."
  (smart-jump-register :modes 'lsp-mode :order smart-jump-lsp-mode-order))

(provide 'smart-jump-lsp-mode)
;;; smart-jump-lsp-mode.el ends here
