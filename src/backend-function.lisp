;;;; backend-function.lisp

(in-package #:magicl.backends)

;;;; This file implements functions that may have different
;;;; "backends", specifically to support different kinds of vector and
;;;; matrix arithmetic routines. For instance, we might want both Lisp
;;;; and C implementations of the same function, and we might want the
;;;; user to be able to select such functions at run-time.
;;;;
;;;; Terminology:
;;;;
;;;;     BACKEND: A name of a backend, usually a keyword. Make a new
;;;;     one with DEFINE-BACKEND.
;;;;
;;;;     BACKEND FUNCTION: A function that "wraps"
;;;;     implementations. This is sort of like a Common Lisp generic
;;;;     function. Make a new one with DEFINE-BACKEND-FUNCTION.
;;;;
;;;;     BACKEND IMPLEMENTATION: A function that actually contains the
;;;;     code for a specific function's backend. This is sort of like
;;;;     a Common Lisp method. Make a new one with
;;;;     DEFINE-BACKEND-IMPLEMENTATION.

;;; first some utilities

(deftype function-designator ()
  '(or symbol function))

(defun normalize-optional/keyword-argument (arg)
  (destructuring-bind (arg init supplied) (alexandria:ensure-list arg)
    (list arg init (if (null supplied)
                       (gensym "SUPPLIEDP")
                       supplied))))

(defun normalize-lambda-list (lambda-list)
  (multiple-value-bind (required-args
                        optional-args                 ; List of (NAME INIT SUPPLIEDP)
                        rest-arg
                        kw-args                  ; List of ((KW NAME) INIT SUPPLIEDP)
                        allow-other-keys
                        aux-args)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (let ((normalized required-args))
      (when rest-arg
        (setf normalized (append normalized '(&rest) (list rest-arg))))
      (when optional-args
        (setf normalized
              (append normalized '(&optional)
                      (mapcar #'normalize-optional/keyword-argument optional-args))))
      (when kw-args
        (setf normalized
              (append normalized '(&key)
                      (mapcar #'normalize-optional/keyword-argument kw-args))))
      (when allow-other-keys
        (setf normalized (append normalized '(&allow-other-keys))))
      (when aux-args
        (setf normalized (append normalized '(&aux) aux-args)))
      normalized)))

;;; now the meat

(define-condition no-applicable-implementation (simple-error)
  ((function-name :initarg :function-name :reader no-applicable-implementation-function-name)
   (failed-callees :initarg :failed-callees :reader no-applicable-implementation-failed-callees
                   :documentation "A list of NO-APPLICABLE-IMPLEMENTATION conditions that were a result of callee failures."))
  (:documentation "The error to signal when there's no applicable implementation, whether from a backend function or a CLOS generic function."))

(defun describe-callees (s conditions &optional (indent 1))
  ;; used to format NO-APPLICABLE-IMPLEMENTATION errors
  (etypecase conditions
    ((member :|<unavailable>|)
     (format s "~vT* <unavailable>~%" (* 4 indent)))
    (no-applicable-implementation
     (format s "~vT* ~S~%" (* 4 indent)
             (no-applicable-implementation-function-name conditions))
     (describe-callees s (no-applicable-implementation-failed-callees conditions) (1+ indent)))
    (list
     (dolist (c conditions)
       (describe-callees s c indent)))))

(defun nai-fmt (s arguments &optional colon-modifier at-modifier)
  ;; used to format NO-APPLICABLE-IMPLEMENTATION errors
  (declare (ignore colon-modifier at-modifier))
  (destructuring-bind (name callees) arguments
    (let ((*package* (find-package "KEYWORD")))
      (format s "No applicable implementations found for the (supposed) ~
                 backend function ~S.~2%" name)
      (describe-backend-function name s)
      (terpri s)
      (format s "There was no implementation of ~S that was applicable to the given ~
                 arguments. This may be because:~2%" name)
      (format s "    * An implementation exists, but a function this implementation ~
                       itself uses doesn't have an implementation. (See below.)~2%")
      (format s "    * No implementation exists in a currently active backend,~2%")
      (format s "    * A generic function implements the backend, but the generic ~
                       function does not specialize on the argument, or~2%")
      (format s "    * An implementation exists, but signaled that it wasn't ~
                       applicable for the given arguments.~2%")
      (unless (null callees)
        (format s "The callees that failed to find an implementation were:~2%")
        (describe-callees s callees))
      (terpri s)
      (format s "Ensure the proper backends are activated, and the implementations ~
                 in those backends can handle the argument types appropriately. If ~
                 an implementation does not exist, consider writing one!"))))

(defun no-applicable-implementation (name &key (failed-callees ':|<unavailable>|))
  "Call this function to signal an error indicating the caller (or any callers above it) are not applicable to the current backend function being invoked."
  (error 'no-applicable-implementation
         :function-name name
         :failed-callees failed-callees
         :format-control "~/magicl.backends::nai-fmt/"
         :format-arguments (list (list name failed-callees))))

(defmacro define-compatible-no-applicable-method-behavior (&rest generic-function-names)
  "Ensure the (unquoted symbol) generic function names GENERIC-FUNCTION-NAMES will behave correctly when used as implementations for backend functions.

The specific behavior that is made \"compatible\" is ensuring that if generic function dispatch reaches a call to NO-APPLIABLE-METHOD, then an appropriate condition will be signaled to instruct the backend function to continue searching.

Without using this, a backend function may error if no method is found."
  (assert (every #'symbolp generic-function-names))
  `(progn
     ,@(loop
         :for name :in generic-function-names
         :collect
         `(defmethod cl:no-applicable-method ((gf (eql #',name)) &rest args)
            (declare (ignore args))
            (no-applicable-implementation ',name :failed-callees nil)))))

;;; Backend Names

;;; Eval at compile-time because DEFINE-BACKEND and the DEFTYPE below
;;; will need it for proper expansion later in the file.
;;;
;;; (h/t phoe 3/31/21)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *known-backends* '()
    "A list of known backends.")

  (defvar *backend* '()
    "A list in priority order the backends that MAGICL should use for functionality.

It is preferable to use WITH-BACKENDS instead of this.")

  (defun backend-name-p (x)
    (and (symbolp x)
         (find x *known-backends*)
         t))
)                                       ; EVAL-WHEN

(defun active-backends ()
  "Return a list of the active backends in priority order. Useful for debugging."
  *backend*)

(deftype backend-name ()
  '(satisfies backend-name-p))

(defmacro define-backend (name &key documentation
                                    (default nil))
  "Define the existence of a backend named NAME. If DEFAULT is T, then add it to the list of default backends."
  (check-type name symbol)
  (check-type documentation (or null string))
  (check-type default boolean)
  `(progn
     (pushnew ',name *known-backends*)
     (when ',default
       (pushnew ',name *backend*))))

(define-backend :lisp
  :documentation "The pure Lisp backend."
  :default t)

(defmacro with-backends ((&rest backends) &body body)
  "Execute BODY with the (static) list of BACKENDS, written in the priority they should be used.

If you need fine-grained control over the backends selected, you may instead directly bind *BACKEND*, but it is not recommended."
  (dolist (backend backends)
    (unless (typep backend 'backend-name)
      (error "Not a backend name: ~A" backend)))
  `(let ((*backend* ',backends))
     ,@body))


;;; Storage of Backend Implementations
;;;
;;; We store backend implementations in the symbol plist. The
;;; implementations themselves form a plist with backends as keys and
;;; the implementation functions as values.

(defconstant +backends-key+ 'backend-implementations
  "The key to the symbol plist to get the backend implementations.")

(defun backend-function-p (x)
  "Does X name a backend function?"
  (and (symbolp x)
       (fboundp x)
       (not (eq '#1=#:not-found
                (getf (symbol-plist x) +backends-key+ '#1#)))))

(defun initialize-function-for-implementations (x)
  (check-type x symbol)
  (unless (backend-function-p x)
    (setf (getf (symbol-plist x) +backends-key+) nil)))

(defun %backend-implementation (sym backend)
  (getf (getf (symbol-plist sym) +backends-key+) backend))

(defun (setf %backend-implementation) (new-value sym backend)
  (check-type new-value function-designator)
  (check-type backend backend-name)
  (setf (getf (getf (symbol-plist sym) +backends-key+) backend) new-value))

(defun backend-implementation (sym backend)
  "Return the backend implementation for the function named SYM and backend named BACKEND."
  (%backend-implementation sym backend))

(define-compiler-macro backend-implementation (&whole whole sym backend &environment env)
  (cond
    ((and (constantp sym env)     (typep sym '(cons (member quote)
                                               (cons symbol null)))
          (constantp backend env) (typep backend '(or
                                                   backend-name
                                                   (cons (member quote)
                                                    (cons backend-name null)))))
     (let ((sym (second sym))           ; de-QUOTE
           (backend (if (typep backend 'backend-name)
                        backend
                        (second backend)))) ; de-QUOTE
       (%backend-implementation sym backend)))
    (t
     whole)))


;;; Definition of Backend Functions and Implementations

(defmacro define-backend-function (name lambda-list &optional doc)
  "Define a backend function named NAME with lambda list LAMBDA-LIST.

Backend implementations are defined with DEFINE-BACKEND-IMPLEMENTATION."
  (check-type name symbol)
  (check-type doc (or null string))
  (unless (eq (symbol-package name) (find-package "MAGICL"))
    (warn "Defining a backend function ~S in ~
           a package that's not MAGICL. Are you sure?"
          name))
  (setf lambda-list (normalize-lambda-list lambda-list))
  (let ((backend             (gensym "PROFILE"))
        (func                (gensym "FUNC")))
    `(progn
       (initialize-function-for-implementations ',name)
       (defun ,name ,lambda-list
         ,@(and doc (list doc))
         (let ((callees nil))
           (dolist (,backend *backend*)
             (handler-case
                 (let ((,func (backend-implementation ',name ,backend)))
                   (unless (null ,func)
                     (return-from ,name
                       ,(interface:calling-form func lambda-list))))
               (no-applicable-implementation (c)
                 (push c callees))))
           (no-applicable-implementation ',name :failed-callees callees))))))

(defmacro define-backend-implementation (name backend funcallable-expression)
  "Define the implementation of the backend function named NAME, for the backend BACKEND, as the function FUNCALLABLE-EXPRESSION (evaluated).

NOTE: If your implementation is a generic function, then the generic function's behavior with NO-APPLICABLE-METHOD should be made compatible with the backends framework. This can be done with the macro DEFINE-COMPATIBLE-NO-APPLICABLE-METHOD-BEHAVIOR."
  (check-type name symbol)
  (check-type backend backend-name)
  `(progn
     (setf (%backend-implementation ',name ',backend) ,funcallable-expression)
     ',name))


;;; For Error Output and Debugging

(defun function-p (name)
  (ignore-errors (fdefinition name)))

(defun generic-function-p (name)
  (and (function-p name)
       (typep (fdefinition name) 'generic-function)))

(defun describe-backend-function (name &optional (stream *standard-output*))
  (assert (symbolp name))
  (let ((*package* (find-package "KEYWORD")))
    (cond
      ((not (backend-function-p name))
       (format stream "~S does not name a backend function.~%" name))
      (t
       (let* ((active (active-backends))
              (inactive (set-difference *known-backends* active)))
         (flet ((print-info (b)
                  (let ((impl (backend-implementation name b)))
                    (format stream "* Backend ~S has " b)
                    (cond
                      ((null impl)
                       (format stream "no implementations"))
                      ((generic-function-p impl)
                       (format stream "~S [generic function]" impl))
                      ((function-p impl)
                       (format stream "~S [non-generic function]" impl))
                      (t
                       (format stream "~S [undefined]" impl))))))
           (format stream "~S names a backend function.~2%" name)
           (cond
             ((null active)
              (format stream "There are no active backends.~%"))
             (t
              (format stream "Active Backends:~%")
              (dolist (b active)
                (format stream "~4T")
                (print-info b)
                (terpri stream))))
           (terpri stream)
           (cond
             ((null inactive)
              (format stream "There are no loaded backends that are inactive.~%"))
             (t
              (format stream "Inactive Backends:~%")
              (dolist (b inactive)
                (format stream "~4T")
                (print-info b)
                (terpri stream))))))))))
