(in-package #:magicl.foreign-libraries)

;;;; Accelerate's second LAPACK interface.
;;;;
;;;; macOS ships two LAPACKs inside Accelerate. The legacy one, exported under
;;;; the usual `zgeev_' names, is LAPACK 3.2.1 and is missing 40 of the 142
;;;; symbols magicl binds -- including zuncsd_, which MAGICL:CSD needs. Since
;;;; macOS 13.3 there is also a LAPACK 3.9.1 interface which has all of them.
;;;;
;;;; The newer symbols drop the trailing underscore before a suffix:
;;;;
;;;;     legacy      zuncsd_
;;;;     new         zuncsd$NEWLAPACK
;;;;     new, ILP64  zuncsd$NEWLAPACK$ILP64
;;;;
;;;; magicl's bindings are 32-bit-integer, so the plain $NEWLAPACK variant is the
;;;; right one; $NEWLAPACK$ILP64 would silently misread every dimension argument.
;;;; The calling convention is otherwise the ordinary Fortran one -- arguments by
;;;; reference, hidden lengths after character arguments -- so a routine resolved
;;;; this way is called exactly like its legacy counterpart.
;;;;
;;;; Only the routines magicl actually reaches are handled here. Migrating all
;;;; 142 would mean regenerating src/bindings/*-cffi.lisp with suffixed names,
;;;; which is a larger change; the startup checks in validate-backend.lisp catch
;;;; anything still missing.

(defun lapack-routine-pointer (base-name)
  "Pointer to LAPACK routine BASE-NAME, given without a trailing underscore.

Tries the standard Fortran symbol first, then Accelerate's new-LAPACK spelling.
Returns NIL if neither is present."
  (or (cffi:foreign-symbol-pointer (concatenate 'string base-name "_"))
      (cffi:foreign-symbol-pointer (concatenate 'string base-name "$NEWLAPACK"))))

(defun require-lapack-routine (base-name)
  (or (lapack-routine-pointer base-name)
      (error "The loaded LAPACK provides neither ~A_ nor ~:*~A$NEWLAPACK.~
              ~%Loaded from: ~A"
             base-name (or *lapack-library-path* "an unknown path"))))

;;; ZUNCSD is resolved lazily rather than at load time: it is only needed by
;;; MAGICL:CSD, and a backend that lacks it is still perfectly usable for
;;; everything else. Erring here at load would take that away.

(defvar *zuncsd-pointer* nil)

(defun zuncsd-pointer ()
  (or *zuncsd-pointer*
      (setf *zuncsd-pointer* (require-lapack-routine "zuncsd"))))

(defun call-zuncsd (jobu1 jobu2 jobv1t jobv2t trans signs m p q
                    x11 ldx11 x12 ldx12 x21 ldx21 x22 ldx22
                    theta u1 ldu1 u2 ldu2 v1t ldv1t v2t ldv2t
                    work lwork rwork lrwork iwork info)
  "Call ZUNCSD through whichever symbol the loaded LAPACK provides.

Called through a pointer rather than through the generated MAGICL.LAPACK-CFFI
binding, because that binding names `zuncsd_' literally and is declaimed inline,
so it can be neither redefined nor redirected after the fact."
  (cffi:foreign-funcall-pointer
   (zuncsd-pointer) ()
   :string jobu1 :string jobu2 :string jobv1t :string jobv2t
   :string trans :string signs
   :pointer m :pointer p :pointer q
   :pointer x11 :pointer ldx11 :pointer x12 :pointer ldx12
   :pointer x21 :pointer ldx21 :pointer x22 :pointer ldx22
   :pointer theta
   :pointer u1 :pointer ldu1 :pointer u2 :pointer ldu2
   :pointer v1t :pointer ldv1t :pointer v2t :pointer ldv2t
   :pointer work :pointer lwork :pointer rwork :pointer lrwork
   :pointer iwork :pointer info
   :void))
