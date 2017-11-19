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

(defcustom smart-jump-jump-key "M-."
  "Key used for binding jump."
  :type 'string
  :group 'smart-jump)

(defcustom smart-jump-pop-key "M-,"
  "Key used for binding jump."
  :type 'string
  :group 'smart-jump)

(defcustom smart-jump-refs-key "M-?"
  "Key used for finding references."
  :type 'string
  :group 'smart-jump)

(defcustom smart-jump-simple-find-references-function
  'smart-jump-find-references-with-ag
  "Function used as fallback when find references fails."
  :type 'symbol
  :group 'smart-jump)

(defvar-local smart-jump-list '()
  "
List of plists that contain metadata to trigger GoToDefinition.

  '(jump-fn: pop-fn: should-jump: heuristic: async:)

jump-fn: The function to call interactively to trigger go to definition.

pop-fn: The reverse of jump-function.

should-jump: Either t, nil or a function that determines if jump-fn
should be triggered.

heuristic Either a recognized symbol or a custom function that will be
ran after jump-function is triggered.

async: Whether or not to run the heuristic function after a certain time.
If this is a number, run the heuristic function after that many ms.")

(defvar smart-jump-stack '() "Stack used to navigate tags.")

(defvar smart-jump-dumb-fallback '(
                                   :jump-fn dumb-jump-go
                                   :pop-fn dumb-jump-back
                                   :refs-fn smart-jump-simple-find-references
                                   :should-jump t
                                   :heuristic point
                                   :async nil
                                   )
  "Fallback settings to use when no other :jump-fn mechanism succeeded.")

;;;###autoload
(defun smart-jump-go (&optional smart-list)
  "Go to the function/variable declartion for thing at point.

SMART-LIST will be set if this is a continuation of a previous jump."
  (interactive)
  (let ((sj-list (or smart-list (append smart-jump-list
                                        (list smart-jump-dumb-fallback)))))
    (while sj-list
      (let* ((entry (car sj-list))
             (jump-function (plist-get entry :jump-fn))
             (pop-function (plist-get entry :pop-fn))
             (should-run-jump-function (plist-get entry :should-jump))
             (heuristic-function (plist-get entry :heuristic))
             (async (plist-get entry :async)))
        (setq sj-list (cdr sj-list))
        (when (or
               (and (functionp should-run-jump-function)
                    (funcall should-run-jump-function))
               should-run-jump-function)
          (condition-case nil
              (cond
               ((eq heuristic-function 'error)
                ;; We already catch for errors so nothing special
                ;; needs to be done here.
                (call-interactively jump-function)
                (push pop-function smart-jump-stack)
                (setq sj-list nil))
               ((eq heuristic-function 'point)
                (let ((current-point (point)))
                  (call-interactively jump-function)
                  (if async
                      (let ((saved-list sj-list))
                        (setq sj-list nil) ;; Early exit current function.
                        (run-with-idle-timer
                         (smart-jump-get-async-wait-time async)
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
                (call-interactively jump-function)
                (if async
                    (let ((saved-list sj-list))
                      (setq sj-list nil) ;; Early exit current function.
                      (run-with-idle-timer
                       (smart-jump-get-async-wait-time async)
                       nil
                       (lambda ()
                         (if (funcall heuristic-function)
                             (push pop-function smart-jump-stack)
                           (smart-jump-go saved-list)))))
                  (when (funcall heuristic-function)
                    (setq sj-list nil)
                    (push pop-function smart-jump-stack)))))
            (error :continue)))))))

;;;###autoload
(defun smart-jump-back ()
  "Jump back to where the last jump was done."
  (interactive)
  (call-interactively (if (> (length smart-jump-stack) 0)
                          (pop smart-jump-stack)
                        'xref-pop-marker-stack)))

;;;###autoload
(defun smart-jump-references (&optional smart-list)
  "Find references with fallback."
  (interactive)
  (let ((sj-list (or smart-list (append smart-jump-list
                                        (list smart-jump-dumb-fallback)))))
    (while sj-list
      (let* ((entry (car sj-list))
             (refs-function (plist-get entry :refs-fn))
             (should-run-jump-function (plist-get entry :should-jump))
             (heuristic-function (plist-get entry :heuristic))
             (async (plist-get entry :async)))
        (setq sj-list (cdr sj-list))
        (when (or
               (and (functionp should-run-jump-function)
                    (funcall should-run-jump-function))
               should-run-jump-function)
          (condition-case nil
              (cond
               ((eq heuristic-function 'error)
                ;; We already catch for errors so nothing special
                ;; needs to be done here.
                (call-interactively refs-function)
                (setq sj-list nil))
               ((eq heuristic-function 'point)
                (let ((current-point (point)))
                  (call-interactively refs-function)
                  (if async
                      (let ((saved-list sj-list))
                        (setq sj-list nil) ;; Early exit current function.
                        (run-with-idle-timer
                         (smart-jump-get-async-wait-time async)
                         nil
                         (lambda ()
                           (when (eq (point) current-point)
                             (smart-jump-references saved-list)))))
                    (if (eq (point) current-point)
                        :continue
                      (setq sj-list nil)))))
               (:custom-heuristic
                (call-interactively refs-function)
                (if async
                    (let ((saved-list sj-list))
                      (setq sj-list nil) ;; Early exit current function.
                      (run-with-idle-timer
                       (smart-jump-get-async-wait-time async)
                       nil
                       (lambda ()
                         (unless (funcall heuristic-function)
                           (smart-jump-references saved-list)))))
                  (when (funcall heuristic-function)
                    (setq sj-list nil)))))
            (error :continue)))))))

;; FIXME: I'd prefer if this was more idempotent.
;; Right now it feels messy to call this multiple times.
(cl-defun smart-jump-register (&key
                               modes
                               (jump-fn 'xref-find-definitions)
                               (pop-fn 'xref-pop-marker-stack)
                               (refs-fn 'xref-find-references)
                               (should-jump t)
                               (heuristic 'error)
                               (async nil))
  "FIXME: Document argument list."
  (unless (listp modes)
    (setq modes (list modes)))
  (dolist (mode modes)
    (let ((mode-hook (intern (format "%S-hook" mode)))
          (mode-map (intern (format "%S-map" mode))))
      (add-hook mode-hook
                (lambda ()
                  (smart-jump-update-jump-list
                   jump-fn pop-fn refs-fn should-jump heuristic async)
                  (smart-jump-bind-jump-keys mode-map))
                :append-to-hook))))

(defun smart-jump-update-jump-list (jump-fn
                                    pop-fn
                                    refs-fn
                                    should-jump
                                    heuristic
                                    async)
  "Update `smart-jump-list' with new settings."
  (setq smart-jump-list
        (append
         ;; It's better to figure out how to remove the original
         ;; hook from the mode but for now, at the very least,
         ;; it's better to remove the old settings upon
         ;; calling smart-jump-register again.
         ;; It would be better if this updated smart-jump
         ;; settings for active modes too.
         (seq-remove (lambda (plist)
                       (eq jump-fn (plist-get plist :jump-fn)))
                     smart-jump-list)
         (list `(
                 :jump-fn ,jump-fn
                 :pop-fn ,pop-fn
                 :refs-fn ,refs-fn
                 :should-jump ,should-jump
                 :heuristic ,heuristic
                 :async ,async
                 )))))

(defun smart-jump-bind-jump-keys (mode-map-symbol)
  "Bind keys for GoToDefinition."
  (when smart-jump-bind-keys
    (let ((map (symbol-value mode-map-symbol)))
      (when smart-jump-bind-keys-for-evil
        (with-eval-after-load 'evil
          (when (fboundp 'evil-define-key*)
            (evil-define-key* 'normal map
                              (kbd smart-jump-jump-key) #'smart-jump-go
                              (kbd smart-jump-pop-key) #'smart-jump-back
                              (kbd smart-jump-refs-key) #'smart-jump-references))))
      (define-key map (kbd smart-jump-jump-key) #'smart-jump-go)
      (define-key map (kbd smart-jump-pop-key) #'smart-jump-back)
      (define-key map (kbd smart-jump-refs-key) #'smart-jump-references))))

;; Helpers
(defun smart-jump-simple-find-references ()
  "Call `smart-jump-simple-find-references-function'."
  (interactive)
  (funcall smart-jump-simple-find-references-function))

(defun smart-jump-find-references-with-ag ()
  "Use `ag' to find references."
  (interactive)
  (if (not (fboundp 'ag-project))
      (message "Install ag to use `smart-jump-simple-find-references-with-ag'.")
    (ag-project (cond ((use-region-p)
                       (buffer-substring-no-properties (region-beginning)
                                                       (region-end)))
                      ((symbol-at-point)
                       (substring-no-properties
                        (symbol-name (symbol-at-point))))))))

(defun smart-jump-get-async-wait-time (async)
  "Return the time in seconds for use with waiting for an async jump.
If ASYNC is a number, use to determine the wait time."
  (/ (if (numberp async)
         async
       smart-jump-async-wait-time) 1000.0))

(provide 'smart-jump)
;;; smart-jump.el ends here
