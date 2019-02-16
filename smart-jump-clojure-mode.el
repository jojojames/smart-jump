;;; smart-jump-clojure-mode.el --- Register `clojure-mode' for `smart-jump'. -*- lexical-binding: t -*-

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
;;; Register `clojure-mode' for `smart-jump'.

;;; Code:
(require 'clojure-mode nil t)
(require 'cider nil t)
(require 'clj-refactor nil t)
(require 'smart-jump)

(defun smart-jump-clojure-cider-available-p ()
  "Return whether or not `cider' is available."
  (and
   (bound-and-true-p cider-mode)
   (fboundp 'cider-connected-p)
   (cider-connected-p)))

;;;###autoload
(defun smart-jump-clojure-mode-register ()
  "Register `clojure-mode' for `smart-jump'."
  (smart-jump-register :modes '(clojure-mode cider-mode cider-repl-mode)
                       :jump-fn 'cider-find-var
                       :pop-fn 'cider-pop-back
                       :refs-fn 'cljr-find-usages
                       :should-jump #'smart-jump-clojure-cider-available-p
                       :heuristic 'point
                       :async 500))

(provide 'smart-jump-clojure-mode)
;;; smart-jump-clojure-mode.el ends here
