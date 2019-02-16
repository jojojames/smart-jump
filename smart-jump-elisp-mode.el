;;; smart-jump-elisp-mode.el --- Register `smart-jump' for `elisp-mode'. -*- lexical-binding: t -*-

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
;;; Register `smart-jump' for `elisp-mode'.

;;; Code:
(require 'smart-jump)

;; We don't require this so that it can be autoloaded by the user.
;; (require 'elisp-slime-nav nil t)

(defun smart-jump-elisp-slime-nav-available-p ()
  "Return whether or not `elisp-slime-nav' is available."
  (or
   (bound-and-true-p elisp-slime-nav-mode)
   (fboundp 'elisp-slime-nav-find-elisp-thing-at-point)
   (autoloadp (symbol-function 'elisp-slime-nav-find-elisp-thing-at-point))))

;;;###autoload
(defun smart-jump-elisp-mode-register ()
  "Register `smart-jump' for `elisp-mode'."
  (smart-jump-register :modes '(emacs-lisp-mode lisp-interaction-mode)
                       :jump-fn 'elisp-slime-nav-find-elisp-thing-at-point
                       :pop-fn 'pop-tag-mark
                       :should-jump #'smart-jump-elisp-slime-nav-available-p
                       :heuristic 'error
                       :async nil)

  (smart-jump-register :modes '(emacs-lisp-mode lisp-interaction-mode)))

(provide 'smart-jump-elisp-mode)
;;; smart-jump-elisp-mode.el ends here
