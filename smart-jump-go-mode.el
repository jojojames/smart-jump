;;; smart-jump-go-mode.el --- Register `go-mode' for `smart-jump'. -*- lexical-binding: t -*-

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
;;; Register `go-mode' for `smart-jump'.
;;; This requires the `go-guru' package and the tool `guru' installed and
;;; available on your $PATH.

;;; Code:
(require 'smart-jump)
(require 'go-guru nil t)

(defun smart-jump-go-mode-go-guru-available-p ()
  "Return whether or not `go-guru' is available."
  (fboundp 'go-guru-definition))

;;;###autoload
(defun smart-jump-go-mode-register ()
  "Register `go-mode' for `smart-jump'."
  (smart-jump-register :modes 'go-mode
                       :jump-fn 'go-guru-definition
                       :refs-fn 'go-guru-referrers
                       :order 1
                       :should-jump #'smart-jump-go-mode-go-guru-available-p)
  (smart-jump-register :modes 'go-mode
                       :jump-fn 'godef-jump
                       :order 2))

(provide 'smart-jump-go-mode)
;;; smart-jump-go-mode.el ends here
