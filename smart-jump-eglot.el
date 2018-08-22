;;; smart-jump-eglot.el --- Register `eglot' for `smart-jump'. -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

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
;;; Register `eglot' for `smart-jump'.

;;; Code:
(require 'smart-jump)
(require 'eglot nil t)

(defvar eglot--managed-mode)

(defcustom smart-jump-eglot-order 0
  "The order `eglot' runs during a `smart-jump'.

This needs to be set before `smart-jump-eglot-register' is called."
  :type 'integer
  :group 'smart-jump)

(defun smart-jump-eglot-register ()
  "Register `eglot' for `smart-jump'."
  (smart-jump-register :modes '(eglot-mode . eglot--managed-mode-hook)
                       :order smart-jump-eglot-order
                       :should-jump (lambda () eglot--managed-mode)))

(provide 'smart-jump-eglot)
;;; smart-jump-eglot.el ends here
