# Reference BLAS 3.8.0

Updated November 2017

See [http://www.netlib.org/blas/](http://www.netlib.org/blas/) for more details.


## Notes

- `xerbla_array.f`, containing `XERBLA_ARRAY`, has been removed due to difficulties with f2cl. This routine is a simple wrapper around `XERBLA` (the BLAS error handler) which takes an array rather than string argument. We do not use it.