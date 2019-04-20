;;; smart-jump-python.el --- Register `smart-jump' for `python'. -*- lexical-binding: t -*-

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
;;; Register `smart-jump' for `python'.

;;; Code:
(require 'smart-jump)
(require 'python)
(require 'anaconda-mode nil t)
(require 'elpy nil t)

(defcustom smart-jump-python-force-elpy nil
  "If true, use elpy even if elpy-mode is not turned on."
  :type 'boolean
  :group 'smart-jump)

(defun smart-jump-python-elpy-available-p ()
  "Return whether or not `elpy' is available."
  (or smart-jump-python-force-elpy
    (bound-and-true-p elpy-mode)))

(defun smart-jump-python-anaconda-available-p ()
  "Return whether or not `anaconda-mode' is available."
  (bound-and-true-p anaconda-mode))

;;;###autoload
(defun smart-jump-python-register ()
  "Register `smart-jump' for `python'."
  (smart-jump-register :modes 'python-mode
                       :jump-fn 'anaconda-mode-find-definitions
                       :pop-fn 'anaconda-mode-go-back
                       :refs-fn 'anaconda-mode-find-references
                       :should-jump #'smart-jump-python-anaconda-available-p
                       :heuristic 'point
                       :async 600)
  
  (smart-jump-register :modes 'python-mode
                       :jump-fn 'elpy-goto-definition
                       :pop-fn 'xref-pop-marker-stack
                       ;; pending https://github.com/jorgenschaefer/elpy/issues/1082
                       :refs-fn nil
                       :should-jump #'smart-jump-python-elpy-available-p
                       :heuristic 'point
                       :async 600))

(provide 'smart-jump-python)
;;; smart-jump-python.el ends here
