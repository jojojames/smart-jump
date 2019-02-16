;;; smart-jump-scheme.el --- Register `smart-jump' for `scheme'. -*- lexical-binding: t -*-

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
;;; Register `smart-jump' for `scheme'.

;;; Code:
(require 'smart-jump)
(require 'scheme)
(require 'geiser nil t)

(defun smart-jump-scheme-geiser-available-p ()
  "Return whether or not `geiser' is available."
  (and (bound-and-true-p geiser-mode)))

;;;###autoload
(defun smart-jump-scheme-register ()
  "Register `smart-jump' for `scheme'."
  (smart-jump-register :modes '(scheme-mode
                                geiser-repl-mode
                                geiser-doc-mode)
                       :jump-fn 'geiser-edit-symbol-at-point
                       :pop-fn 'geiser-pop-symbol-stack
                       :refs-fn 'geiser-xref-callers
                       :should-jump #'smart-jump-scheme-geiser-available-p
                       :heuristic 'point))

(provide 'smart-jump-scheme)
;;; smart-jump-scheme.el ends here
