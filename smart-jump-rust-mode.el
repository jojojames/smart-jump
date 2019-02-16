;;; smart-jump-rust-mode.el --- Register `smart-jump' for `rust-mode'. -*- lexical-binding: t -*-

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
;;; Register `smart-jump' for `rust-mode'.

;;; Code:
(require 'racer nil t)
(require 'rust-mode nil t)
(require 'smart-jump)

(defun smart-jump-rust-mode-racer-available-p ()
  "Return whether or not `racer' is available."
  (and (bound-and-true-p racer-mode)
       (executable-find "racer")))

;;;###autoload
(defun smart-jump-rust-mode-register ()
  "Register `smart-jump' for `rust-mode'."
  (smart-jump-register :modes 'rust-mode
                       :jump-fn 'racer-find-definition
                       :pop-fn 'pop-tag-mark
                       :refs-fn 'smart-jump-simple-find-references
                       :should-jump #'smart-jump-rust-mode-racer-available-p
                       :heuristic 'point))

(provide 'smart-jump-rust-mode)
;;; smart-jump-rust-mode.el ends here
