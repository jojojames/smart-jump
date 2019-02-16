;;; smart-jump-ruby-mode.el --- Register `smart-jump' for `ruby-mode'. -*- lexical-binding: t -*-

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
;;; Register `smart-jump' for `ruby-mode'.

;;; Code:
(require 'robe nil t)
(require 'ruby-mode)
(require 'smart-jump)

(defun smart-jump-ruby-robe-available-p ()
  "Return whether or not `robe' is available."
  (bound-and-true-p robe-mode))

;;;###autoload
(defun smart-jump-ruby-mode-register ()
  "Register `smart-jump' for `ruby-mode'."
  (smart-jump-register :modes 'robe-mode
                       :jump-fn 'robe-jump
                       :refs-fn 'smart-jump-simple-find-references
                       :should-jump #'smart-jump-ruby-robe-available-p))

(provide 'smart-jump-ruby-mode)
;;; smart-jump-ruby-mode.el ends here
