# High Level Bindings

The purpose of the high-level magicl bindings is to allow for MATLAB-like multidimensional arrays in lisp. 

## Constructors

The construction of tensors can be done with any of the given constructors. The constructors take a shape and arguments for method of construction.

Tensors are specialized on both the shape and the element type. The class of a tensor will be of the form `$CLASS/$TYPE` (e.g. `MATRIX/DOUBLE-FLOAT`).

| Number of dimensions | Tensor Class    |
|----------------------|-----------------|
| 1                    | `VECTOR`        |
| 2                    | `MATRIX`        |
| *                    | `TENSOR`        |

| Element Type             | Class Suffix           |
|--------------------------|------------------------|
| `SINGLE-FLOAT`           | `SINGLE-FLOAT`         |
| `DOUBLE-FLOAT`           | `DOUBLE-FLOAT`         |
| `(COMPLEX SINGLE-FLOAT)` | `COMPLEX-SINGLE-FLOAT` |
| `(COMPLEX DOUBLE-FLOAT)` | `COMPLEX-DOUBLE-FLOAT` |
| `(SIGNED-BYTE 32)`       | `INT32`                |

The type of the elements of the tensor can be specified with the `:type` keyword, or the constructor will attempt to find an appropriate type from the given arguments. The default element type for a tensor is `DOUBLE-FLOAT`.

The layout of the tensor (column-major or row-major) can be specified with the `:layout` keyword. This affects the underlying storage of the tensor and will affect how it carries out operations with LAPACK. 


## Other Library Equivalents

This table was adapted largely from the [NumPy Equivalents Table](https://docs.scipy.org/doc/numpy/user/numpy-for-matlab-users.html#linear-algebra-equivalents).

### Basic Accessors

| MAGICL         | MATLAB     | NumPy                   | Description                                                   |
|----------------|------------|-------------------------|---------------------------------------------------------------|
| `(order a)`    | `ndims(a)` | `ndim(a)` or `a.ndim`   | Get the number of dimensions of the array.                    |
| `(size a)`     | `numel(a)` | `size(a)` or `a.size`   | Get the number of elements of the array.                      |
| `(shape a)`    | `size(a)`  | `shape(a)` or `a.shape` | Get the shape of the array.                                   |
| `(tref a 1 4)` | `a(2,5)`   | `a[1, 4]`               | Get the element in the second row, fifth column of the array. |

### Constructors

| MAGICL                                          | MATLAB             | NumPy                             | Description                                                                          |
|-------------------------------------------------|--------------------|-----------------------------------|--------------------------------------------------------------------------------------|
| `(from-list '(1d0 2d0 3d0 4d0 5d0 6d0) '(2 3))` | `[ 1 2 3; 4 5 6 ]` | `array([[1.,2.,3.], [4.,5.,6.]])` | Create a 2x3 matrix from given elements.                                             |
| `(zeros '(2 3 4))` or `(const 0d0 '(2 3 4))`    | `zeros(2,3,4)`     | `zeros((2,3,4))`                  | Create a 2x3x4 dimensional array of zeroes of double-float element type.             |
| `(ones '(3 4))` or `(const 1d0 '(3 4))`         | `ones(3,4)`        | `ones((3,4))`                     | Create a 3x4 dimensional array of ones of double-float element type.                 |
| `(eye '(3 3) :value 1d0)`                       | `eye(3)`           | `eye(3)`                          | Create a 3x3 identity array of double-float element type.                            |
| `(from-diag '(1 2 3))`                          | `diag([1 2 3])`    | `diag([1.,2.,3.])`                | Create a tensor from a list, placing given elements along the diagonal |
| `(rand '(3 4))`                                 | `rand(3,4)`        | `random.rand(3,4)`                | Create a random 3x4 array.                                                           |

#### Block Matrix Constructors

Magicl provides some "block matrix" constructors: these construct matrices from their constituent blocks. In what follows, `A,B,C,D` denote matrices.

| MAGICL                                  | MATLAB           | NumPy                              | Description                                            |
|-----------------------------------------|------------------|------------------------------------|--------------------------------------------------------|
| `(block-matrix (list A B C D) '(2 2))`  | `[A B; C D]`     | `block([[A,B], [C, D]])`           | Create a block matrix from matrices A,B,C,D.           | 
| `(block-diag (list A B C))`             | `blkdiag(A,B,C)` | `scipy.linalg.block_diag([A,B,C])` | Create a block diagonal matrix from matrices A,B,C.    |
| `(hstack (list A B C))`                 | `[A B C]`        | `hstack((A,B,C))`                  | Concatenate matrices A,B,C horizontally (column-wise). |
| `(vstack (list A B C))`                 | `[A; B; C]`      | `vstack((A,B,C))`                  | Concatenate matrices A,B,C vertically (row-wise).      |

### Basic Operations

| MAGICL       | MATLAB     | NumPy             | Description                    |
|--------------|------------|-------------------|--------------------------------|
| `(@ a b)`    | `a * b`    | `a @ b`           | Matrix multiplication          |
| `(.+ a b)`   | `a + b`    | `a + b`           | Element-wise add               |
| `(.- a b)`   | `a - b`    | `a - b`           | Element-wise subtract          |
| `(.* a b)`   | `a .* b`   | `a * b`           | Element-wise multiply          |
| `(./ a b)`   | `a./b`     | `a/b`             | Element-wise divide            |
| `(.^ a b)`   | `a.^b`     | `np.power(a,b)`   | Element-wise exponentiation    |
| `(.exp a)`   | `exp(a)`   | `np.exp(a)`       | Element-wise exponential       |
| `(.log a)`   | `log(a)`   | `np.log(a)`       | Element-wise natural logarithm |

### Linear Algebra

| MAGICL                                                  | MATLAB            | NumPy                                                          | Description                                |
|---------------------------------------------------------|-------------------|----------------------------------------------------------------|--------------------------------------------|
| `(det a)`                                               | `det(a)`          | `linalg.det(a)`                                                | Determinant of matrix                      |
| `(trace a)`                                             | `trace(a)`        | `trace(a)`                                                     | Trace (sum of diagonal elements) of matrix |
| `(upper-triangular a)`                                  | `triu(a)`         | `triu(a)`                                                      | Upper triangular part of matrix            |
| `(lower-triangular a)`                                  | `tril(a)`         | `tril(a)`                                                      | Lower triangular part of matrix            |
| `(transpose a)`                                         | `a.'`             | `a.transpose()` or `a.T`                                       | Transpose of matrix                        |
| `(conjugate-transpose a)` or `(dagger a)`               | `a'`              | `a.conj().transpose()` or `a.H`                                | Conjugate transpose of matrix              |
| `(inv a)`                                               | `inv(a)`          | `linalg.inv(a)`                                                | Inverse of matrix                          |
| `(svd a)` (Returns `(VALUES U SIGMA Vt)`)               | `[U,S,V]=svd(a)`  | `U, S, Vh = linalg.svd(a), V = Vh.T`                           | Singular value decomposition of matrix     |
| `(eig a)` (Returns `(VALUES EIGENVALUES EIGENVECTORS)`) | `[V,D]=eig(a)`    | `D,V = linalg.eig(a)`                                          | Eigenvalues and eigenvectors of matrix     |
| `(qr a)` (Returns `(VALUES Q R)`)                       | `[Q,R,P]=qr(a,0)` | `Q,R = scipy.linalg.qr(a)`                                     | QR factorization of matrix                 |
| `(ql a)` (Returns `(VALUES Q L)`)                       |                   |                                                                | QL factorization of matrix                 |
| `(rq a)` (Returns `(VALUES R Q)`)                       |                   | `R,Q = scipy.linalg.rq(a)`                                     | RQ factorization of matrix                 |
| `(lq a)` (Returns `(VALUES L Q)`)                       |                   |                                                                | LQ factorization of matrix                 |
| `(lu a)` (Returns `(VALUES LU IPIV)`)                   | `[L,U,P]=lu(a)`   | `L,U = scipy.linalg.lu(a)` or `LU,P=scipy.linalg.lu_factor(a)` | LU decomposition of matrix                 |
| `(csd a)` (Returns `(VALUES U SIGMA VT)`)               |                   |                                                                | Cosine-sine decomposition of matrix        |
