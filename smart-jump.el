;;; smart-jump.el --- Smart go to definition. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/smart-jump
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools

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
;;; Smart go to definition.

;;; Code:
(require 'dumb-jump)

(defgroup smart-jump nil
  "Easily jump to project function and variable definitions using
multiple fallbacks."
  :group 'tools
  :group 'convenience)

(defcustom smart-jump-bind-keys t
  "If this is true, bind M-.  and M-, upon registering `smart-jump'.

Defaults to t."
  :type 'boolean
  :group 'smart-jump)

(defcustom smart-jump-bind-keys-for-evil t
  "If this is true, bind M-.
and M-, upon registering `smart-jump' for symbol `evil-mode'.

Defaults to t."
  :type 'boolean
  :group 'smart-jump)

(defcustom smart-jump-async-wait-time 500
  "The time to wait in ms when GoToDefinition function is async."
  :type 'number
  :group 'smart-jump)

(defvar-local smart-jump-list '()
  "
List of plists that contain metadata to trigger GoToDefinition.

  '(gtd-fn: pop-fn: should-gtd: heuristic: async:)

gtd-fn: The function to call interactively to trigger go to definition.

pop-fn: The reverse of gtd-function.

should-gtd: Either t, nil or a function that determines if gtd-fn
should be triggered.

heuristic Either a recognized symbol or a custom function that will be
ran after gtd-function is triggered.

async: Whether or not to run the heuristic function after a certain time.
If this is a number, run the heuristic function after that many ms.")

(defvar smart-jump-stack '() "Stack used to navigate tags.")

(defvar dumb-jump-fallback '(
                             :gtd-fn dumb-jump-go
                             :pop-fn dumb-jump-back
                             :should-gtd t
                             :heuristic point
                             :async nil
                             )
  "Fallback settings to use when no other GoToDefinition mechanism succeeded.")

;;;###autoload
(defun smart-jump-go (&optional smart-list)
  "Go to the function/variable declartion for thing at point.

SMART-LIST will be set if this is a continuation of a previous jump."
  (interactive)
  (let ((sj-list (or smart-list (append smart-jump-list
                                        (list dumb-jump-fallback)))))
    (while sj-list
      (let* ((entry (car sj-list))
             (gtd-function (plist-get entry :gtd-fn))
             (pop-function (plist-get entry :pop-fn))
             (should-run-gtd-function (plist-get entry :should-gtd))
             (heuristic-function (plist-get entry :heuristic))
             (async (plist-get entry :async)))
        (setq sj-list (cdr sj-list))
        (when (or
               (and (fboundp should-run-gtd-function)
                    (funcall should-run-gtd-function))
               should-run-gtd-function)
          (condition-case nil
              (cond
               ((eq heuristic-function 'error)
                ;; We already catch for errors so nothing special
                ;; needs to be done here.
                (call-interactively gtd-function)
                (push pop-function smart-jump-stack)
                (setq sj-list nil))
               ((eq heuristic-function 'point)
                (let ((current-point (point)))
                  (call-interactively gtd-function)
                  (if async
                      (let ((saved-list sj-list))
                        (setq sj-list nil) ;; Early exit current function.
                        (run-with-idle-timer
                         (/ (if (numberp async)
                                async
                              smart-jump-async-wait-time) 1000.0)
                         nil
                         (lambda ()
                           (if (eq (point) current-point)
                               (smart-jump-go saved-list)
                             (push pop-function smart-jump-stack)))))
                    (if (eq (point) current-point)
                        :continue
                      (push pop-function smart-jump-stack)
                      (setq sj-list nil)))))
               (:custom-heuristic
                (call-interactively gtd-function)
                (if async
                    (let ((saved-list sj-list))
                      (setq sj-list nil) ;; Early exit current function.
                      (run-with-idle-timer
                       (/ (if (numberp async)
                              async
                            smart-jump-async-wait-time) 1000.0)
                       nil
                       (lambda ()
                         (if (funcall heuristic-function)
                             (push pop-function smart-jump-stack)
                           (smart-jump-go saved-list)))))
                  (when (funcall heuristic-function)
                    (push pop-function smart-jump-stack)))))
            (error :continue)))))))

;;;###autoload
(defun smart-jump-back ()
  "Jump back to where the last jump was done."
  (interactive)
  (call-interactively (if (> (length smart-jump-stack) 0)
                          (pop smart-jump-stack)
                        'xref-pop-marker-stack)))

;; FIXME: I'd prefer if this was more idempotent.
;; Right now it feels messy to call this multiple times.
(cl-defun smart-jump-register (&key
                               modes
                               (gtd-fn 'xref-find-definitions)
                               (pop-fn 'xref-pop-marker-stack)
                               (should-gtd t)
                               (heuristic 'error)
                               (async nil))
  "FIXME: Document argument list."
  (unless (listp modes)
    (setq modes (list modes)))
  (dolist (mode modes)
    (let ((mode-hook (intern (concat (symbol-name mode) "-hook"))))
      (add-hook mode-hook
                (lambda ()
                  (setq smart-jump-list
                        (append smart-jump-list
                                (list `(
                                        :gtd-fn ,gtd-fn
                                        :pop-fn ,pop-fn
                                        :should-gtd ,should-gtd
                                        :heuristic ,heuristic
                                        :async ,async
                                        ))))

                  ;; FIXME: Make these keys customizable.
                  (when smart-jump-bind-keys
                    (let ((map (symbol-value
                                (intern (concat (symbol-name mode) "-map")))))
                      (when smart-jump-bind-keys-for-evil
                        (with-eval-after-load 'evil
                          (when (fboundp 'evil-define-key*)
                            (evil-define-key* 'normal map
                                              (kbd "M-.") 'smart-jump-go
                                              (kbd "M-,") 'smart-jump-back))))
                      (define-key map (kbd "M-.") #'smart-jump-go)
                      (define-key map (kbd "M-,") #'smart-jump-back))))
                :append-to-hook))))

(provide 'smart-jump)
;;; smart-jump.el ends here
