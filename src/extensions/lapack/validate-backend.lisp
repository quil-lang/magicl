(in-package #:magicl.foreign-libraries)

;;;; Sanity checks on the LAPACK implementation that actually got loaded.
;;;;
;;;; magicl selects a backend by trying a list of paths and using whatever loads
;;;; first. Nothing about that guarantees the result can satisfy magicl's
;;;; bindings, or that it computes correct answers, and both have failed:
;;;;
;;;;   - Accelerate's legacy interface is LAPACK 3.2.1 and lacks 40 of the 142
;;;;     symbols magicl binds, among them zuncsd_. Loading succeeds; the failure
;;;;     arrives later as `The alien function "zuncsd_" is undefined'.
;;;;
;;;;   - Homebrew's reference lapack 3.12.1 on arm64, as bottled, returns vectors
;;;;     that are not eigenvectors for complex input at n >= 3. Nothing fails at
;;;;     all; the caller just gets wrong numbers. (A gfortran miscompile of
;;;;     zlarf1f.f rather than a LAPACK defect: GCC PR122408.)
;;;;
;;;; That an implementation may be incomplete is already understood here --
;;;; FOREIGN-SYMBOL-AVAILABLE-P and PRINT-AVAILABILITY-REPORT exist for exactly
;;;; that, and the generated bindings register every routine with TRACK-SYMBOLS.
;;;; What was missing is anything consulting them before use, and any notion of
;;;; the loaded library being *wrong* rather than merely incomplete.
;;;;
;;;; quilc has twice traced a puzzling downstream symptom back to the backend by
;;;; hand (quil-lang/quilc#842, and again in 2026 from an unrelated cause). Both
;;;; would have been a one-line message here.

(define-condition backend-check-failed (simple-error)
  ()
  (:documentation "Signalled when the loaded LAPACK fails a startup check."))

(defun backend-check-error (format &rest arguments)
  (error 'backend-check-failed
         :format-control format
         :format-arguments arguments))

(defun loaded-lapack-description ()
  (or *lapack-library-path* "an unknown path"))

;;; Completeness

(defparameter *required-lapack-routines*
  '("zuncsd" "zgesdd" "zgeev" "zheev" "zgesvd" "zgetrf" "zgeqrf")
  "Routines that must be available for magicl's high-level API to work, named
without the trailing underscore.

Deliberately a small subset of the symbols TRACK-SYMBOLS registers: most of
those are bound but never reached, and refusing to start over a routine nothing
calls would be worse than useless. PRINT-AVAILABILITY-REPORT remains the way to
see the whole picture. zuncsd_ is the one that matters in practice -- it backs
MAGICL:CSD, and is exactly what Accelerate's legacy interface lacks.")

(defun lapack-routine-available-p (base-name)
  "Whether BASE-NAME, given without a trailing underscore, is callable.
Accepts Accelerate's new-LAPACK spelling as well as the standard one.

The lookup is process-wide in practice -- on SBCL the :LIBRARY argument does not
restrict the search -- which is the question worth asking here: will a call to
this routine resolve?"
  (or (foreign-symbol-available-p (concatenate 'string base-name "_") 'liblapack)
      (foreign-symbol-available-p (concatenate 'string base-name "$NEWLAPACK")
                                  'liblapack)))

(defun missing-lapack-symbols ()
  (remove-if #'lapack-routine-available-p *required-lapack-routines*))

(defun check-lapack-completeness ()
  (let ((missing (missing-lapack-symbols)))
    (when missing
      (backend-check-error
       "The LAPACK loaded by magicl is missing ~R routine~:P that magicl needs:~
        ~%  ~{~A~^ ~}~
        ~%Loaded from: ~A~
        ~%~
        ~%On macOS this is what Accelerate's legacy interface looks like: it is~
        ~%LAPACK 3.2.1 and predates these routines. Install a complete LAPACK~
        ~%(`brew install openblas'), or point MAGICL_LAPACK_PATH at one.~
        ~%MAGICL:PRINT-AVAILABILITY-REPORT lists everything else that is missing."
       (length missing) missing (loaded-lapack-description)))))

;;; Correctness
;;;
;;; One 3x3 complex eigenproblem, checking that the returned eigenvectors satisfy
;;; A v = lambda v. Three things matter about the choice:
;;;
;;;   - complex, because the miscompile that motivates this leaves real
;;;     arithmetic correct;
;;;   - n >= 3, because it is exact at n = 2 (the Householder path that gets
;;;     miscompiled is not reached for smaller matrices);
;;;   - a residual with a loose tolerance rather than a comparison against
;;;     expected values, so that legitimate differences between implementations
;;;     -- ordering, phase, scaling of eigenvectors -- do not trip it.

(defparameter *backend-check-tolerance* 1d-8
  "Residual above which the loaded LAPACK is considered broken. Chosen far above
round-off (~1d-15) and far below the failure being detected (~1d0), so it needs no
tuning per implementation.")

(defun %eigen-residual-3x3 ()
  "Largest |A v - lambda v| over the eigenpairs of a fixed 3x3 complex matrix."
  (let* ((n 3)
         ;; Hermitian, so the eigenvalues are real and the problem is well
         ;; conditioned; still exercises the complex code path.
         (entries (list #C(1d0 0d0)    #C(3d0 -1.5d0)  #C(5d0 -2d0)
                        #C(3d0 1.5d0)  #C(2d0 0d0)     #C(2.2d0 -1.7d0)
                        #C(5d0 2d0)    #C(2.2d0 1.7d0) #C(4d0 0d0)))
         (a (magicl:from-list entries (list n n) :type '(complex double-float))))
    (multiple-value-bind (eigenvalues eigenvectors) (magicl:eig a)
      ;; Computed with TREF alone, rather than by slicing out a column and
      ;; multiplying, so that this check does not depend on the shape
      ;; conventions of the higher-level operators it would otherwise use.
      (loop :with worst := 0d0
            :for j :below n
            :for lambda := (elt eigenvalues j)
            :do (dotimes (i n)
                  (let ((av (loop :for k :below n
                                  :sum (* (magicl:tref a i k)
                                          (magicl:tref eigenvectors k j)))))
                    (setf worst (max worst
                                     (abs (- av (* lambda
                                                   (magicl:tref eigenvectors i j))))))))
            :finally (return worst)))))

(defun check-lapack-correctness ()
  (let ((residual (handler-case (%eigen-residual-3x3)
                    (error (condition)
                      (backend-check-error
                       "The LAPACK loaded by magicl could not solve a 3x3 complex~
                        ~%eigenproblem: ~A~
                        ~%Loaded from: ~A"
                       condition (loaded-lapack-description))))))
    (when (> residual *backend-check-tolerance*)
      (backend-check-error
       "The LAPACK loaded by magicl returns incorrect eigenvectors.~
        ~%~
        ~%A 3x3 complex eigenproblem gave a residual |A v - lambda v| of ~,3E,~
        ~%where round-off would be around 1d-15. The returned vectors are not~
        ~%eigenvectors, and results computed with this library cannot be trusted.~
        ~%Loaded from: ~A~
        ~%~
        ~%A known cause on arm64 macOS is Homebrew's reference `lapack' 3.12.1,~
        ~%miscompiled by gfortran (GCC PR122408, Reference-LAPACK#1160). Use~
        ~%OpenBLAS (`brew install openblas'), rebuild it (`brew install~
        ~%--build-from-source lapack'), or point MAGICL_LAPACK_PATH elsewhere."
       residual (loaded-lapack-description)))))

;;; Entry point

(defun check-lapack-backend ()
  "Verify that the loaded LAPACK is complete enough and computes correctly.
Signals BACKEND-CHECK-FAILED otherwise."
  (check-lapack-completeness)
  (check-lapack-correctness)
  t)

;; Skippable, because a startup check that cannot be turned off is a liability in
;; any environment where it turns out to be wrong.
(unless (environment-variable "MAGICL_SKIP_BACKEND_CHECK")
  (check-lapack-backend))
