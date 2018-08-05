;;; smart-jump.el --- Smart go to definition. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/smart-jump
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (dumb-jump "0.5.1"))
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
(eval-when-compile (require 'subr-x))
(require 'dumb-jump)
(require 'seq)

;; Compatibility

(eval-and-compile
  (with-no-warnings
    (if (version< emacs-version "26")
        (progn
          (defalias 'smart-jump-if-let* #'if-let)
          (defalias 'smart-jump-when-let* #'when-let)
          (function-put #'smart-jump-if-let* 'lisp-indent-function 2)
          (function-put #'smart-jump-when-let* 'lisp-indent-function 1))
      (defalias 'smart-jump-if-let* #'if-let*)
      (defalias 'smart-jump-when-let* #'when-let*))))

;; Customizations

(defgroup smart-jump nil
  "Easily jump to project function and variable definitions using
multiple fallbacks."
  :group 'tools
  :group 'convenience)

(defcustom smart-jump-default-mode-list
  '(cc-mode ;; `java-mode', `c-mode', `c++-mode', `objc-mode'
    csharp-mode
    clojure-mode
    eglot
    elisp-mode
    elixir-mode
    elm-mode
    erlang-mode
    go-mode
    haskell-mode
    (js2-mode rjsx-mode)
    lisp-mode
    lispy
    lua-mode
    lsp-mode
    python
    ruby-mode
    rust-mode
    scala-mode
    scheme
    swift-mode
    typescript-mode
    web-mode)
  "The list of modes `smart-jump-setup-default-registers' uses to
register `smart-jump's."
  :type '(repeat (choice symbol sexp))
  :group 'smart-jump)

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
  "The default time to wait in ms when when setting :async to true
in `smart-jump-register'. before checking if a jump has failed."
  :type 'number
  :group 'smart-jump)

(defcustom smart-jump-default-order-weight 1
  "The default weight applied to each `smart-jump' registration.

This ordering is used when determining which `smart-jump' strategy to use
first."
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

;; FIXME: Finalize a good default key.
;; This key may or may not be final.
(defcustom smart-jump-peek-key "M-P"
  "Key used for peeping at definitions."
  :type 'string
  :group 'smart-jump)

(defcustom smart-jump-find-references-fallback-function
  'smart-jump-find-references-with-ag
  "The fallback function used by `smart-jump-simple-find-references'."
  :type 'function
  :group 'smart-jump)

(defvar smart-jump-stack '() "Stack used to navigate tags.")

(defvar smart-jump-simple-fallback
  '(
    :jump-fn dumb-jump-go
    :pop-fn dumb-jump-back
    :refs-fn smart-jump-simple-find-references
    :should-jump t
    :heuristic point
    :async nil
    :order 1000
    )
  "Fallback settings to use when no other :jump-fn mechanism succeeded.")

(defvar smart-jump-xref-fallback
  '(
    :jump-fn xref-find-definitions
    :pop-fn xref-pop-marker-stack
    :refs-fn xref-find-references
    :should-jump t
    :heuristic error
    :async nil
    :order 1000
    )
  "Xref fallback to use when no other :jump-fn mechanism succeeded.")

(defvar-local smart-jump-list `(,smart-jump-simple-fallback)
  "List of plists that contain metadata to trigger jump to definition
or find references.

The list comprises of argument lists of this format.

  '(jump-fn: pop-fn: refs-fn: should-jump: heuristic: async:)

See `smart-jump-register' for more details.")

;;;###autoload
(defun smart-jump-setup-default-registers ()
  "Register a default set of modes for `smart-jump'."
  (interactive)
  (dolist (mode smart-jump-default-mode-list)
    (let ((m mode)
          (reqs (list mode)))
      (when (listp mode)
        (setq m (car mode)
              reqs (cdr mode)))
      (dolist (req reqs)
        (with-eval-after-load req
          (require
           (intern (concat "smart-jump-" (symbol-name m))))
          (funcall
           (intern (concat "smart-jump-" (symbol-name m) "-register"))))))))

;;;###autoload
(defun smart-jump-peek ()
  "Peek at definition."
  (interactive)
  (smart-jump-make-peek-frame 'smart-jump-go))

;;;###autoload
(defun smart-jump-go (&optional smart-list continue)
  "Go to the function/variable declartion for thing at point.

SMART-LIST will be set (or nil) if this is a continuation of a previous jump.

CONTINUE will be non nil if this is a continuation of a previous jump."
  (interactive)
  (smart-jump-when-let*
      ((sj-list (or smart-list (and (not continue) smart-jump-list))))
    (smart-jump-run
     #'smart-jump-go
     sj-list
     :jump-fn :heuristic :pop-fn)))

;;;###autoload
(defun smart-jump-back ()
  "Jump back to where the last jump was done."
  (interactive)
  (call-interactively (if (> (length smart-jump-stack) 0)
                          (pop smart-jump-stack)
                        'xref-pop-marker-stack)))

;;;###autoload
(defun smart-jump-references (&optional smart-list continue)
  "Find references with fallback.
Optional argument SMART-LIST This will be non-nil of continuation of previous
call to `smart-jump-references'.

CONTINUE will be set if this is a continuation of a previous call to
`smart-jump-references'."
  (interactive)
  (smart-jump-when-let*
      ((sj-list (or smart-list (and (not continue) smart-jump-list))))
    (push-mark nil t nil)
    (smart-jump-run
     #'smart-jump-references
     sj-list
     :refs-fn :refs-heuristic :default-pop-key)))

;; Helpers
(defun smart-jump-run (self-command sj-list function-key heuristic-key pop-key)
  "Workhorse method for outer smart-jump-* methods.

SELF-COMMAND: The command the user interactively called. (e.g. `smart-jump-go').

SJ-LIST: The list of jumps to try.

FUNCTION-KEY: Key used to access function to call from SJ-LIST.

HEURISTIC-KEY: Key used to access heuristic function to run after attempting
jump. The heuristic will be used to check if the jump succeeded or not.

POP-KEY: Key used to access pop function. If key is not found or not
provided, `pop-tag-mark' will be used as the default."
  (interactive)
  (let* ((entry (car sj-list))
         (jump-function (plist-get entry function-key))
         (pop-function (or (plist-get entry pop-key) #'pop-tag-mark))
         (before-jump-fn (plist-get entry :before-jump-fn))
         (should-run-jump-function (plist-get entry :should-jump))
         (heuristic-function (plist-get entry heuristic-key))
         (async (plist-get entry :async)))
    (setq sj-list (cdr sj-list))
    (if (smart-jump-should-try-jump-p should-run-jump-function)
        (progn
          (when before-jump-fn
            (funcall before-jump-fn))
          (condition-case nil
              (cond
               ((eq heuristic-function 'error)
                ;; We already catch for errors so nothing special
                ;; needs to be done here.
                (call-interactively jump-function)
                (push pop-function smart-jump-stack))
               ((eq heuristic-function 'point)
                (let* ((current-point (point))
                       (cb (lambda ()
                             (if (eq (point) current-point)
                                 (funcall self-command sj-list :continue)
                               (push pop-function smart-jump-stack)))))
                  (call-interactively jump-function)
                  (if (not async)
                      (funcall cb)
                    (run-with-idle-timer
                     (smart-jump-get-async-wait-time async) nil cb))))
               (:custom-heuristic
                (call-interactively jump-function)
                (let ((cb (lambda ()
                            (if (funcall heuristic-function)
                                (push pop-function smart-jump-stack)
                              (funcall self-command sj-list :continue)))))
                  (if (not async)
                      (funcall cb)
                    (run-with-idle-timer
                     (smart-jump-get-async-wait-time async) nil cb)))))
            (error
             (funcall self-command sj-list :continue))))
      (funcall self-command sj-list :continue))))

(defun smart-jump-make-peek-frame (find-definition-function &rest args)
  "Make a new frame for peeking definition.

Credits to @tuhdo.

http://tuhdo.github.io/emacs-frame-peek.html"
  (let (doc-frame
        x y
        ;; 1. Find the absolute position of the current beginning of the
        ;; symbol at point, in pixels.
        (abs-pixel-pos (save-excursion
                         ;; (beginning-of-thing 'symbol)
                         (beginning-of-line)
                         (window-absolute-pixel-position))))
    (setq x (car abs-pixel-pos))

    ;; FIXME: We might want to recenter the original view first before getting
    ;; y so that the new popup frame never goes beneath our screen.
    (setq y (+ (cdr abs-pixel-pos)
               (frame-char-height)))

    ;; 2. Create a new invisible frame, with the current buffer in it.
    (setq doc-frame (make-frame '((name . "*SmartJump Peek*")
                                  (width . 80)
                                  (visibility . nil)
                                  (height . 20)
                                  (min-width  . t)
                                  (min-height . t)
                                  (border-width . 0)
                                  (internal-border-width . 0)
                                  (vertical-scroll-bars . nil)
                                  (horizontal-scroll-bars . nil)
                                  (left-fringe . 0)
                                  (right-fringe . 0)
                                  (tool-bar-lines . 0)
                                  (line-spacing . 0)
                                  (unsplittable . t)
                                  (no-other-frame . t)
                                  (no-special-glyphs . t))))

    ;; 3. Position the new frame right under the beginning of the
    ;; symbol at point.
    (set-frame-position doc-frame x y)

    ;; 4. Jump to the symbol at point.
    (with-selected-frame doc-frame
      (apply find-definition-function args)
      ;; FIXME: If we make this read-only, we need to be able to revert its
      ;; readonly status after the frame is killed.
      ;; FIXME: If we make this readonly, it'd be nice to bind q to quit the
      ;; frame and buffer quickly.
      ;; (read-only-mode)
      (recenter-top-bottom 0))

    ;; 5. Make frame visible again.
    (make-frame-visible doc-frame)))

(cl-defun smart-jump-register (&key
                               modes
                               (jump-fn 'xref-find-definitions)
                               (pop-fn 'xref-pop-marker-stack)
                               (refs-fn 'xref-find-references)
                               (before-jump-fn nil)
                               (should-jump t)
                               (heuristic 'error)
                               (refs-heuristic heuristic)
                               (async nil)
                               (order smart-jump-default-order-weight))
  "Register mode for use with `smart-jump'.

JUMP-FN: The function to call interactively to trigger go to definition.

POP-FN: The reverse of jump-function.

REFS-FN: Function used for finding references.

BEFORE-JUMP-FN: Function called before JUMP-FN is called.

SHOULD-JUMP: Either t, nil or a function that determines if jump-fn
should be triggered.

HEURISTIC: Either a recognized symbol or a custom function that will be
ran after jump-function is triggered.

REFS-HEURISTIC: Like HEURISTIC, but for use with REFS-FN.

ASYNC: Whether or not to run the heuristic function after a certain time.
If this is a number, run the heuristic function after that many ms.

ORDER: The weight applied to each JUMP-FN. This is used to determine which
fallback strategy is used first. Lower numbers give more precedence."
  ;; Add `smart-jump-go' to list of exclusions so `xref' doesn't prompt the user.
  (when (memq 'not xref-prompt-for-identifier)
    (unless (memq 'smart-jump-go xref-prompt-for-identifier)
      (setq xref-prompt-for-identifier
            (append xref-prompt-for-identifier (list 'smart-jump-go
                                                     'smart-jump-references
                                                     'smart-jump-peek)))))
  (unless (listp modes)
    (setq modes (list modes)))
  (dolist (mode modes)
    (let ((derived-mode-hook-name (intern (format "%S-hook" mode))))
      (dolist (b (buffer-list))
        (with-current-buffer b
          (when (or (bound-and-true-p mode) ;; `minor-mode'
                    (eq major-mode mode)) ;; `major-mode'
            (smart-jump-bind-jump-keys mode)
            (smart-jump-update-jump-list
             jump-fn
             pop-fn
             refs-fn
             before-jump-fn
             should-jump
             heuristic
             refs-heuristic
             async
             order))))
      (add-hook derived-mode-hook-name
                ;; Give the hook function a name so we don't add multiple
                ;; anonymous function to a mode hook everytime
                ;; `smart-jump-register' is called.
                (defalias (intern (format "smart-jump-setup-%S-%S" mode jump-fn))
                  (function
                   (lambda ()
                     (smart-jump-bind-jump-keys mode)
                     (smart-jump-update-jump-list
                      jump-fn
                      pop-fn
                      refs-fn
                      before-jump-fn
                      should-jump
                      heuristic
                      refs-heuristic
                      async
                      order))))
                :append-to-hook))))

(defun smart-jump-update-jump-list (jump-fn
                                    pop-fn
                                    refs-fn
                                    before-jump-fn
                                    should-jump
                                    heuristic
                                    refs-heuristic
                                    async
                                    order)
  "Update `smart-jump-list' with new settings.
Argument JUMP-FN Jump
Argument POP-FN Pop
Argument REFS-FN Find References
Argument SHOULD-JUMP Should Jump
Argument HEURISTIC Heuristic
Argument REFS-HEURISTIC Heuristic for REFS-FN
Argument ASYNC Async"
  (setq smart-jump-list
        (sort
         (append
          (seq-remove (lambda (plist)
                        (eq jump-fn (plist-get plist :jump-fn)))
                      smart-jump-list)
          (list `(
                  :jump-fn ,jump-fn
                  :pop-fn ,pop-fn
                  :refs-fn ,refs-fn
                  :before-jump-fn ,before-jump-fn
                  :should-jump ,should-jump
                  :heuristic ,heuristic
                  :refs-heuristic ,refs-heuristic
                  :async ,async
                  :order ,order
                  )))
         (lambda (first second)
           ;; Extra defensive.. around upgrades...
           ;; Only (< first-order second-order) is truly needed.
           ;; If the list is '(2 5 3 1), it should become '(1 2 3 5).
           (let ((first-order (plist-get first :order))
                 (second-order (plist-get second :order)))
             (if (or (null first-order) (null second-order))
                 nil
               (< first-order second-order)))))))

(defun smart-jump-bind-jump-keys (mode)
  "Bind keys for `smart-jump-go', `smart-jump-back' and `smart-jump-references'.

MODE is mode to bind keys to."
  (when smart-jump-bind-keys
    (let* ((derived-mode-map-name (intern (format "%S-map" mode))))
      (smart-jump-when-let*
          ((map (when (boundp derived-mode-map-name)
                  (symbol-value derived-mode-map-name))))
        (when smart-jump-bind-keys-for-evil
          (with-eval-after-load 'evil
            (when (fboundp 'evil-define-key*)
              (evil-define-key* 'normal map
                                (kbd smart-jump-jump-key) #'smart-jump-go
                                (kbd smart-jump-pop-key) #'smart-jump-back
                                (kbd smart-jump-refs-key) #'smart-jump-references
                                (kbd smart-jump-peek-key) #'smart-jump-peek))))
        (define-key map (kbd smart-jump-jump-key) #'smart-jump-go)
        (define-key map (kbd smart-jump-pop-key) #'smart-jump-back)
        (define-key map (kbd smart-jump-refs-key) #'smart-jump-references)
        (define-key map (kbd smart-jump-peek-key) #'smart-jump-peek)))))

(defun smart-jump-simple-find-references ()
  "Fallback method for `smart-jump-references'.

Use this when setting `smart-jump-references' :refs-fn and don't want
to use xref as the fallback."
  (interactive)
  (call-interactively smart-jump-find-references-fallback-function))

(defun smart-jump-find-references-with-ag ()
  "Use `ag' to find references."
  (interactive)
  (if (fboundp 'ag-project)
      (ag-project (cond ((use-region-p)
                         (buffer-substring-no-properties (region-beginning)
                                                         (region-end)))
                        ((symbol-at-point)
                         (substring-no-properties
                          (symbol-name (symbol-at-point))))))
    (message "Install ag to use `smart-jump-simple-find-references-with-ag'.")))

(defun smart-jump-get-async-wait-time (async)
  "Return the time in seconds for use with waiting for an async jump.
If ASYNC is a number, use to determine the wait time."
  (/ (if (numberp async)
         async
       smart-jump-async-wait-time) 1000.0))

(defun smart-jump-should-try-jump-p (should-run-jump-function)
  "Return whether or not SHOULD-RUN-JUMP-FUNCTION indicates a jump
is possible."
  (if (functionp should-run-jump-function)
      (funcall should-run-jump-function)
    should-run-jump-function))

(provide 'smart-jump)
;;; smart-jump.el ends here
