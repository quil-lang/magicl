# Fortran Functions

## Re-Generating Fortran Bindings

This library takes the approach of automatically generating the
bindings to BLAS, LAPACK, and Expokit without relying on any special
tools.

In order to generate the bindings, you will need to download the
Fortran 90 source tarballs for
[BLAS/LAPACK](http://www.netlib.org/lapack/) and
[Expokit](https://www.maths.uq.edu.au/expokit/download.html).  Once
downloaded, extract the tarballs into a directory and re-generate the
bindings with the following commands:

```
(ql:quickload :magicl-gen)
(in-package :magicl.generate-interface)
(generate-blas-files #P"/path/to/lapack-3.9.0/")
(generate-lapack-files #P"/path/to/lapack-3.9.0/")
(generate-expokit-files #P"/path/to/expokit/")
```

Currently this will write to the source distribution directory of
MAGICL, namely the files `blas-cffi.lisp`, `lapack-cffi.lisp`, and
`expokit-cffi.lisp`.

At present, since MAGICL doesn't use most of the functionality offered
by BLAS and LAPACK, we only write out bindings for select
functions. These are determined by the file "src/functions.txt". If
you wish to include *every* BLAS and LAPACK function, then issue the
following *before* the `generate-*` commands:

```
(setf *inclusion-criterion* (constantly t))
```



## Seeing What Fortran Functions Are Available

Since MAGICL binds to lots of Fortran code, we try to make it more
convenient to both show and search which functions are available.

Some distributions of a library don't actually provide all of the
functions of the reference BLAS and LAPACK. One can look at a summary
of available and unavailable functions with the function
`magicl:print-availability-report`. By default, it will show all
functions and their availability. There are three arguments to
fine-tune this behavior:

1. Key `:show-available <boolean>` (default `t`): show available
functions

2. Key `:show-unavailable <boolean>` (default `t`): show unavailable
functions

3. Key `:search <string>`: only show functions which have `<string>`
as a substring. **This argument takes into account the previous two
arguments.**

For example, we can look for all available functions which might
relate to `svd` by doing the following:

```
CL-USER> (magicl:print-availability-report :search "svd" :show-unavailable nil)
        Fortran Function        Lisp Function
------------------------------------------------------------------------

Library LIBBLAS: /usr/local/opt/lapack/lib/libblas.dylib

Library LIBLAPACK: /usr/local/opt/lapack/lib/liblapack.dylib
    [x] CGESVD                  MAGICL.LAPACK-CFFI:%CGESVD
    [x] CGESVDX                 MAGICL.LAPACK-CFFI:%CGESVDX
    [x] CGGSVD3                 MAGICL.LAPACK-CFFI:%CGGSVD3
    [x] DBDSVDX                 MAGICL.LAPACK-CFFI:%DBDSVDX
    [x] DGESVD                  MAGICL.LAPACK-CFFI:%DGESVD
    [x] DGESVDX                 MAGICL.LAPACK-CFFI:%DGESVDX
    [x] DGGSVD3                 MAGICL.LAPACK-CFFI:%DGGSVD3
    [x] SBDSVDX                 MAGICL.LAPACK-CFFI:%SBDSVDX
    [x] SGESVD                  MAGICL.LAPACK-CFFI:%SGESVD
    [x] SGESVDX                 MAGICL.LAPACK-CFFI:%SGESVDX
    [x] SGGSVD3                 MAGICL.LAPACK-CFFI:%SGGSVD3
    [x] ZGESVD                  MAGICL.LAPACK-CFFI:%ZGESVD
    [x] ZGESVDX                 MAGICL.LAPACK-CFFI:%ZGESVDX
    [x] ZGGSVD3                 MAGICL.LAPACK-CFFI:%ZGGSVD3
```
