;;;; benchmark-utilities.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl)

;;; Little utilities for benchmarking.

(defun time-backends (fn &optional (backends (reverse magicl.backends::*known-backends*)))
  "Time the thunk FN against the backends BACKENDS. By default. BACKENDS are all backends."
  (format t ";;; Timing ~A~%" fn)
  (dolist (b  (alexandria:ensure-list backends))
    (handler-case
        (let ((magicl.backends:*backend* (list b))
              (start (get-internal-real-time)))
          (funcall fn)
          (format t "Time for backend ~A: ~D seconds~%"
                  b
                  (float (/ (- (get-internal-real-time) start)
                            internal-time-units-per-second))))
      (magicl.backends::no-applicable-implementation (c)
        (declare (ignore c))
        nil)))
  (terpri))

