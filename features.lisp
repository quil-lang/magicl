(in-package :cl-user)

;; Workaround for a bug in SBCL 1.0.7.18 to 1.0.17.? where many nested
;; UNWIND-PROTECTs as produced by the macro CFFI:DEFCFUN result in
;; heap exhaustion.
;;
;; The version check needs to be in another file because the symbol in
;; question doesn't exist in SBCL before 1.0.7.18.
;;
;; Bugfix is actually in bugfix.lisp

#+sbcl
(labels ((sbcl-version->list (v)
           "convert a SBCL version string like \"1.0.6.debian\" into a
           list of numbers (1 0 6 0)."
           (let ((v (concatenate 'string "("
                                 (substitute #\space #\. v)
                                 ")")))
             (let ((list (remove-if-not #'numberp
                                        (read-from-string v))))
               (if (= 3 (length list))
                   (append list (list 0))
                   list))))

         (sbcl-version-lessp (v1 v2)
           (or (equal (sbcl-version->list v1) (sbcl-version->list v2))
               (some #'< (sbcl-version->list v1)
                     (sbcl-version->list v2))))

         (sbcl-version-check (version-min version-max)
           (let ((v (lisp-implementation-version)))
             (and (sbcl-version-lessp version-min v)
                  (sbcl-version-lessp v version-max)))))
 ;; (when (sbcl-version-check "1.0.7.18" "1.0.17")
 ;;   (pushnew :sbcl-block-gensym-bug *features*))

 )
