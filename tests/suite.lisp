;;;; tests/suite.lisp
;;;;
;;;; Author: Joseph Lin

(in-package #:magicl-tests)

(defun run-magicl-tests (&key (verbose nil) (headless nil))
  "Run all MAGICL tests. If VERBOSE is T, print out lots of test info. If HEADLESS is T, disable interactive debugging and quit on completion."
  (cond
    ((null headless)
     (run-package-tests :package ':magicl-tests
                        :verbose verbose
                        :describe-failures t
                        :interactive t))
    (t
     (let ((successp (run-package-tests :package ':magicl-tests
                                        :verbose t
                                        :describe-failures t
                                        :interactive nil)))
       (uiop:quit (if successp 0 1))))))
