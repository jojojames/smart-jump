;;; smart-jump-test.el --- Tests for smart-jump -*- lexical-binding: t -*-
(require 'smart-jump)

(ert-deftest smart-jump-no-registration-uses-fallbacks ()
  "When mode has not been registered, calling `smart-jump' triggers fallback
functions."
  (defvar smart-jump-increment-counter nil)
  (let* ((smart-jump-list nil) ;; nil --> no registration.
         (counter 0)
         (smart-jump-increment-counter (lambda ()
                                         (interactive)
                                         (setq counter (1+ counter))))
         (smart-jump-simple-jump-function smart-jump-increment-counter)
         (smart-jump-simple-find-references-function
          smart-jump-increment-counter))
    (call-interactively #'smart-jump-go)
    (call-interactively #'smart-jump-references)
    (should (equal counter 2))))

(ert-deftest smart-jump-with-errors-uses-fallbacks ()
  "When first to N-1 `smart-jump's throws an error, fallbacks are triggered."
  (defvar smart-jump-increment-counter nil)
  (let* ((smart-jump-list '((
                             :jump-fn (lambda () (interactive) (throw 'error))
                             :refs-fn (lambda () (interactive (throw 'error)))
                             :should-jump t
                             :heuristic 'error
                             )))
         (counter 0)
         (smart-jump-increment-counter (lambda ()
                                         (interactive)
                                         (setq counter (1+ counter))))
         (smart-jump-simple-jump-function smart-jump-increment-counter)
         (smart-jump-simple-find-references-function smart-jump-increment-counter))
    (call-interactively #'smart-jump-go)
    (call-interactively #'smart-jump-references)
    (should (equal counter 2))))

(ert-deftest smart-jump-no-errors-no-fallbacks ()
  "When there are no errors, jumps in smart-jump-list are called successfully.
No fallbacks are triggered."
  (defvar smart-jump-fallback-counter nil)
  (defvar smart-jump-increment-counter nil)
  (let* ((counter 0)
         (smart-jump-increment-counter (lambda ()
                                         (interactive)
                                         (setq counter (1+ counter))))
         (smart-jump-list `((
                             :jump-fn ,smart-jump-increment-counter
                             :refs-fn ,smart-jump-increment-counter
                             :should-jump t
                             :heuristic error
                             )))
         (smart-jump-fallback-counter (lambda ()
                                        (interactive)
                                        ;; If fallback is called at all, this
                                        ;; test has failed.
                                        (setq counter -1)))
         (smart-jump-simple-jump-function smart-jump-fallback-counter)
         (smart-jump-simple-find-references-function smart-jump-fallback-counter))
    (call-interactively #'smart-jump-go)
    (call-interactively #'smart-jump-references)
    (should (equal counter 2))))

(ert-deftest smart-jump-with-args-do-not-add-fallbacks ()
  "When `smart-jump-references' or `smart-jump-go' is called with an argument,
do not add fallback strategy to `smart-jump-list'.

For example ,when continuing `smart-jump-references' (say from an async
strategy), we don't want to add the callback to the list of `smart-jump'
strategies because it should have already been added in the first call."
  (defvar smart-jump-fallback-counter nil)
  (defvar smart-jump-increment-counter nil)
  (let* ((counter 0)
         (smart-jump-increment-counter (lambda ()
                                         (interactive)
                                         (setq counter (1+ counter))))
         (smart-jump-list `((
                             :jump-fn ,smart-jump-increment-counter
                             :refs-fn ,smart-jump-increment-counter
                             :should-jump t
                             :heuristic error
                             )))
         (smart-jump-fallback-counter (lambda ()
                                        (interactive)
                                        ;; If fallback is called at all, this
                                        ;; test has failed.
                                        (setq counter -1)))
         (smart-jump-simple-jump-function smart-jump-fallback-counter)
         (smart-jump-simple-find-references-function
          smart-jump-fallback-counter))
    (smart-jump-go smart-jump-list)
    (smart-jump-references smart-jump-list)
    (should (equal counter 2))))

;;; smart-jump-test.el ends here
