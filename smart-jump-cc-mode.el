;;; smart-jump-cc-mode.el --- Register `cc-mode' for `smart-jump'. -*- lexical-binding: t -*-

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
;;; Register `cc-mode' for `smart-jump'.
;;; This file is pretty hacky since cc-mode doesn't provide the language feature.
;;; Adding `smart-jump-register's here that are related to `cc-mode'.
;;; I would love if someone had an idea on how to fix this elegantly.

;;; Code:
(require 'smart-jump)

(defvar rtags-rc-binary-name)

;;;###autoload
(defun smart-jump-cc-mode-register ()
  "Register `cc-mode' for `smart-jump'."
  ;; Java
  (smart-jump-register :modes 'java-mode
                       :jump-fn 'ggtags-find-tag-dwim
                       :pop-fn 'ggtags-prev-mark
                       :refs-fn 'ggtags-find-reference
                       :should-jump t
                       :heuristic 'point
                       :async 700
                       :order 2)

  (smart-jump-register :modes 'java-mode
                       :jump-fn 'meghanada-jump-declaration
                       :pop-fn 'meghanada-back-jump
                       :refs-fn 'meghanada-reference
                       :should-jump t
                       :heuristic 'point
                       :async 700
                       :order 1)

  ;; C/C++
  (smart-jump-register :modes '(c-mode c++-mode)
                       :jump-fn 'ycmd-goto
                       :refs-fn 'ycmd-goto-references
                       :should-jump (lambda ()
                                      (bound-and-true-p ycmd-mode))
                       :heuristic 'point
                       :async 2000
                       :order 2)

  (smart-jump-register :modes '(c-mode c++-mode)
                       :jump-fn 'rtags-find-symbol-at-point
                       :pop-fn 'rtags-location-stack-back
                       :refs-fn 'rtags-find-all-references-at-point
                       :should-jump (lambda ()
                                      (and
                                       (fboundp 'rtags-executable-find)
                                       (fboundp 'rtags-is-indexed)
                                       (rtags-executable-find rtags-rc-binary-name)
                                       (rtags-is-indexed)))
                       :heuristic 'point
                       :async 2000
                       :order 1)

  (smart-jump-register :modes '(c-mode c++-mode)
                       :jump-fn 'ggtags-find-tag-dwim
                       :pop-fn 'ggtags-prev-mark
                       :refs-fn 'ggtags-find-reference
                       :should-jump  (lambda ()
                                       (bound-and-true-p ggtags-mode))
                       :heuristic 'point
                       :async 3000
                       :order 3)

  ;; Objective-C
  (smart-jump-register :modes 'objc-mode
                       :jump-fn 'ycmd-goto
                       :refs-fn 'ycmd-goto-references
                       :should-jump (lambda ()
                                      (bound-and-true-p ycmd-mode))
                       :heuristic 'point
                       :async 2000)

  (smart-jump-register :modes 'objc-mode
                       :jump-fn 'etags-select-find-tag-at-point
                       :heuristic 'point
                       :async t))

(provide 'smart-jump-cc-mode)
;;; smart-jump-cc-mode.el ends here
