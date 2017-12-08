;;; smart-jump-slime.el --- Register `slime' for `smart-jump'. -*- lexical-binding: t -*-

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
;;; Register `slime' for `smart-jump'.

;;; Code:
(require 'slime nil t)
(require 'smart-jump)

(defun smart-jump-slime-register ()
  "Register `slime' for `smart-jump'."
  (smart-jump-register :modes '(slime-mode slime-popup-buffer-mode)
                       :jump-fn 'slime-edit-definition
                       :pop-fn 'slime-pop-find-definition-stack
                       :refs-fn 'slime-who-references
                       :should-jump 'slime-current-connection
                       :heuristic 'point
                       :async 700))

(provide 'smart-jump-slime)
;;; smart-jump-slime.el ends here
