;;; smart-jump-tide.el --- Register `tide' for `smart-jump'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
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
;;; Register `tide' for `smart-jump'.

;;; Code:
(require 'smart-jump)
(require 'tide nil t)

(defun smart-jump-tide-register ()
  "Register `smart-jump' for `tide'."
  (smart-jump-register :modes 'tide-mode
                       :jump-fn 'tide-jump-to-definition
                       :pop-fn 'tide-jump-back
                       :refs-fn 'tide-references
                       :should-jump t
                       :heuristic 'point
                       :async t))

(provide 'smart-jump-tide)
;;; smart-jump-tide.el ends here
