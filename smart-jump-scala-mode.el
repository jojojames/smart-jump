;;; smart-jump-scala-mode.el --- Register `smart-jump' for `scala-mode'. -*- lexical-binding: t -*-

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
;;; Register `smart-jump' for `scala-mode'.

;;; Code:
(require 'smart-jump)
(require 'ensime nil t)

(defun smart-jump-scala-mode-ensime-available-p ()
  "Return whether or not `ensime' is available."
  ;; FIXME: Should we check if `ensime' is somehow 'connected'?
  (bound-and-true-p ensime-mode))

(defun smart-jump-scala-mode-register ()
  "Register `smart-jump' for `scala-mode'."
  (smart-jump-register :modes 'ensime-mode
                       :jump-fn 'ensime-edit-definition
                       :pop-fn 'ensime-pop-find-definition-stack
                       :refs-fn 'ensime-show-uses-of-symbol-at-point
                       :should-jump #'smart-jump-scala-mode-ensime-available-p))

(provide 'smart-jump-scala-mode)
;;; smart-jump-scala-mode.el ends here
