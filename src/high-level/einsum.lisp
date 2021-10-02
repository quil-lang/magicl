;;;; einsum.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:magicl)

(deftype vector-length ()
  "A valid dimension size for an array."
  `(integer 0 ,array-total-size-limit))

(deftype vector-index ()
  "A valid index into a vector."
  `(integer 0 (,array-total-size-limit)))

(defmacro einsum ((output-array . output-indices) &rest factors)
  "Evaluates a sum of products of multidimensional arrays, summing
over paired indices, storing the results in OUTPUT-ARRAY. 

As an example, 

 (einsum (A i j) (B i k) (C k j)) 

results in the the updates
 
 A[i,j] = \\sum_k B[i,k]C[k,j],

which is equivalent to matrix multiplication.

If OUTPUT-ARRAY is  _, then a newly allocated array will store the results, 
e.g. 

 (einsum (_ i) (A i j) (B j))

computes the matrix-vector product AB and stores the results in a fresh array."
  (%einsum
   (if (and (symbolp output-array) (string= "_" output-array)) nil output-array)
   output-indices
   factors))


;;; A FACTOR is a list (A i1 ... ik) where - A is a symbol naming an
;;; k-dimensional array - i1 ... ik are INDEX VARIABLES so that the
;;; value of the factor, given a specific array and assignment of
;;; index variables is A[i1,...,ik].

(defun factor-aref-expr (factor)
  `(aref ,@factor))

(defun check-factor-indices (factor)
  "Generate an expression to check whether FACTOR has an appropriate number of indices."
  (destructuring-bind (array &rest indices) factor
    `(unless (= ,(length indices) (array-rank ,array))
       (error "Incompatible dimensions: given array ~A with dimensions ~A and indices ~A"
              ,array
              (array-dimensions ,array)
              ',indices))))

;;; The INDEX-TABLE maintains a mapping from index variables to their
;;; usage in FACTORS, so that we can answer questions like "where is
;;; this index variable used?".

(defun build-index-table (factors)
  "Builds a table mapping index variables to their usage in
FACTORS. Here FACTORS is a list of expressions of the form (A i1 i2 ... ik), 
where A is a k-dimensional array and i1 i2 ... ik are symbols. The corresponding 
table entries are i1 -> ((A . 1)), i2 -> ((A . 2),  ..., ik -> ((A . 2))"
  (let ((index-table (make-hash-table)))
    (dolist (factor factors index-table)
      (destructuring-bind (array &rest indices) factor
        (loop :for idx :in indices
              :for pos :from 0              
              :do (check-type idx symbol)
                  (let ((entry (cons array pos)))
                    (cond ((gethash idx index-table)
                           (push entry (gethash idx index-table)))
                          (t
                           (setf (gethash idx index-table) (list entry))))))))))

(defun check-index-factors (idx factor-entries)
  "Generate an expression to check whether paired arrays have
compatibile dimensions along their paired indices. Here IDX is an
index variable and FACTOR-ENTRIES are its uses as recorded in the
index table."
  (flet ((dim-expr (factor)
           (destructuring-bind (array . pos) factor
             `(array-dimension ,array ,pos))))
    ;; Only generate code if we have multiple things to test equality
    ;; for.
    (when (< 1 (length factor-entries))
      `(unless (= ,@(mapcar #'dim-expr factor-entries))
         (error "Index variable ~A applied to arrays with incompatible dimensions" ',idx)))))


(defun index-dim-binding (idx idx-dim index-table)
  "Returns a binding associating the symbol IDX-DIM with the array dimension of index variable IDX."
  (destructuring-bind (array . pos)
      (first (gethash idx index-table)) ; Since we've checked that sizes agree, we can just use the first entry.
    `(,idx-dim (array-dimension ,array ,pos))))


(defun output-dimensions (output-indices index-table)
  "Compute the dimensions of an output array from a list
OUTPUT-INDICES and a computed INDEX-TABLE."
  (flet ((calc-dim (idx)
           (let ((entries (gethash idx index-table)))
             (cond ((null entries)
                    (error "Output index variable ~A does not appear in a factor." idx))
                   ((< 1 (length entries))
                    (error "Output index variable ~A appears more than once in factor expression." idx))
                   (t
                    (destructuring-bind (array . pos) (first entries)
                      (array-dimension array pos)))))))
    (mapcar #'calc-dim output-indices)))


;;; To evaluate the einsum expression, the %EINSUM function below
;;; generates a bunch of nested DOTIMES loops.  This could be improved
;;; in at least three possible directions:
;;; 
;;; 1. This top-level EINSUM macro could look at the size of the
;;;    involved arrays, and generate several smaller calls to %EINSUM
;;;    which are subsequently combined, akin to the dynamic programming
;;;    approach to matrix multiplication.
;;; 2. Within a single instance of %einsum, there is room for improving
;;;    the ordering of indices in our loops, or perhaps even blocking for
;;;    better cache usage.
;;; 3. Parts of this could be parallelized.


(defun %einsum (output-array output-indices factors &key (dotimes-iterator 'cl:dotimes))
  "Translates an einsum expression into corresponding iterative code
to perform the summation, updating and returning OUTPUT-ARRAY. If
OUTPUT-ARRAY is null, a newly allocated array is used.

The einsum expression contains a number of FACTORS, which are
expressions of the form (A i1 i2 ... ik) where A is a k-dimensional
array and i1, i2, ..., ik are symbols denoting indices. For the
expression to be meaningful, each index should appear exactly twice,
either paired with another index in FACTORS, or paired with an index
in OUTPUT-INDICES, and the associated arrays should have the same size
along paired indices.

DOTIMES-ITERATOR is a DOTIMES-like iterator, that can be changed to
suit different needs (e.g., LPARALLEL:PDOTIMES.)
"

  ;; 
  (let* ((index-table (build-index-table factors))
         (output-dims (loop :repeat (length output-indices) :collect (gensym "OUTPUT-DIM")))
         (summation-indices (loop :for idx :being :the :hash-keys :of index-table
                                    :using (hash-value entries)
                                  :when (= 2 (length entries))
                                    :collect idx))
         (summation-dims (loop :repeat (length summation-indices) :collect (gensym "SUM-DIM")))
         (result-array (gensym "RESULT")) ; array to store results
         (sum-var (gensym)))            ; updated in the innermost nested loop

    ;; The result of all of this is basically a few nested loops
    ;; enclosed in some let expressions which introduce local
    ;; variables. We build this "from the inside out", by starting
    ;; with what goes in the innermost loop (INNER-SUM-TALLY) below,
    ;; and then defining functions to successsively wrap this
    ;; expression accordingly.
    (flet ((inner-sum-tally ()
             "Compute the product of factors and update the summand."
             `(incf ,sum-var (* ,@(mapcar #'factor-aref-expr factors))))
           (indices-loop (indices dims expr)
             "Wrap EXPR in nested loops, one for each element of INDICES."
             (loop :for idx :in indices
                   :for dim :in dims
                   :do (setf expr `(,dotimes-iterator (,idx ,dim)
                                     (declare (type vector-index ,idx))
                                     ,expr)))
             expr)
           (update-output-array (expr)
             "Perform EXPR and then update the result array with the resulting sum variable."
             `(let ((,sum-var 0))
                ,expr
                (setf (aref ,result-array ,@output-indices) ,sum-var)))
           (dim-bindings (indices dims expr)
             "Wrap EXPR in a let expression which introduces variables indicating the dimensions of associated index-variables."
             (let ((bindings (loop :for idx :in indices
                                   :for dim :in dims
                                   :collect (index-dim-binding idx dim index-table))))
               (setf expr `(let ,bindings
                             (declare (type vector-length ,@dims))
                             ,expr))))
           (let-result-array (expr)
             "Evaluate EXPR and return the result erray."
             `(let ((,result-array ,(or output-array `(rray (list ,@output-dims) :initial-element 0.0))))
                ,expr
                ,result-array)))
      
      (let ((safety-checks
              (append (mapcar #'check-factor-indices factors)
                      (loop :for idx :being :the :hash-keys :of index-table
                              :using (hash-value entries)
                            :for check-form := (check-index-factors idx entries)
                            :unless (null check-form)
                              :collect check-form))))
        ;; Given our convention above, the following "looks" a lot
        ;; like the code that it generates. In particular, you can
        ;; observe that the core computation occurs in two sets of
        ;; nested loops: an innermost loop over summation indices, and
        ;; which is contained in an outer loop over output indices.
        `(progn ,@safety-checks
                ,(dim-bindings output-indices output-dims
                    (let-result-array
                      (dim-bindings summation-indices summation-dims
                        (indices-loop output-indices output-dims
                          (update-output-array 
                             (indices-loop summation-indices summation-dims
                                           (inner-sum-tally))))))))))))
