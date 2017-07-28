(in-package :cl-user)

;; Workaround for a bug in SBCL 1.0.7.18 to 1.0.17.? where many nested
;; UNWIND-PROTECTs as produced by the macro CFFI:DEFCFUN result in
;; heap exhaustion.
;;
;; The version check needs to be in another file because the symbol in
;; question doesn't exist in SBCL before 1.0.7.18.

#|

#+(and sbcl sbcl-block-gensym-bug)      ; feature added in features.lisp
(sb-ext:without-package-locks
  (defun sb-impl::block-gensym (&optional (name "G")
                                (env (when (boundp 'sb-c::*lexenv*)
                                       (symbol-value 'sb-c::*lexenv*))))
    (let ((block-name
           (when env
             (car (find-if #'car (sb-c::lexenv-blocks env))))))
      (if block-name
          (gensym (format nil "~A[~A]"  ; fix is here: was "~A[~S]"
                          name block-name))
          (gensym name)))))

|#
