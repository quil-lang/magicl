# MAGICL

[![pipeline status](https://gitlab.com/rigetti/magicl/badges/master/pipeline.svg)](https://gitlab.com/rigetti/magicl/commits/master)

_Matrix Algebra proGrams In Common Lisp_ by [Rigetti Computing](http://www.rigetti.com). (née FLAIL: _Finally, Linear Algebra In Lisp!_)

(**Note**: The high-level interface is experimental and subject to rapid change.)

## Requirements

 * SBCL (> 1.3.19) or CCL (>= 1.11) on AMD64
 * quicklisp
 * libffi
 * BLAS and LAPACK

 Detailed instructions on how to install `libffi` and BLAS/LAPACK can
 be found [here](doc/requirements.md).

 Currently this library is SBCL- and CCL-only. The non-portable code
 is in `with-array-pointers.lisp` and `magicl.lisp`.

## Installation

First ensure you have the necessary requirements installed, as
described in the previous section.

To install MAGICL, clone this repository into your Quicklisp's
`local-projects` folder. You can quickly check where this is by
running `sbcl` and evaluating `ql:*local-project-directories*`. Once
installed, confirm that MAGICL is working properly by running the
tests, as described in the next section.

## Testing MAGICL

You can run the MAGICL tests from your Lisp REPL with:

```
(ql:quickload :magicl-tests)
(asdf:test-system :magicl)
```

This repository is also set up with Sempahore CI, and uses Docker for building MAGICL and running its tests. You can emulate Semaphore's behavior by using the following commands from the top-level directory:

```bash
make -C docker
make -C docker test
```

Note, doing this requires that you have `docker` installed on your machine.

## Showing Available Functions

Some distributions of a library don't actually provide all of the functions of the reference BLAS and LAPACK. One can look at a summary of available and unavailable functions with the function `magicl:print-availability-report`. By default, it will show all functions and their availability. There are three arguments to fine-tune this behavior:

1. Key `:show-available <boolean>` (default `t`): show available functions
2. Key `:show-unavailable <boolean>` (default `t`): show unavailable functions
3. Key `:search <string>`: only show functions which have `<string>` as a substring. **This argument takes into account the previous two arguments.**

For example, we can look for all available functions which might relate to `svd` by doing the following:

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

## Generating BLAS and LAPACK Bindings

This library takes the approach of automatically generating the bindings to BLAS, LAPACK, and Expokit without relying on any special tools.

In order to generate the bindings, you will need to download the Fortran 90 source tarballs for
[BLAS/LAPACK](http://www.netlib.org/lapack/) and [Expokit](https://www.maths.uq.edu.au/expokit/download.html).
Once downloaded, extract the tarballs into a directory and re-generate the bindings with the following commands:

```
(ql:quickload :magicl-gen)
(in-package :magicl.generate-interface)
(generate-blapack-files #P"/path/to/lapack-3.7.1/")
(generate-expokit-files #P"/path/to/expokit/")
```

Currently this will write to the source distribution directory of MAGICL,
namely the files `blas-cffi.lisp`, `lapack-cffi.lisp`, and `expokit-cffi.lisp`.

## History and Credits

MAGICL development started at Rigetti Computing by Robert Smith and Joe Lin in 2017.

[CL-BLAPACK](https://github.com/blindglobe/cl-blapack) is a library developed by Ryan Rifkin and Evan Monroig. Rigetti Computing created a fork of this library and renamed it MAGICL, and made significant changes that departed from the original design, including:

* Fixing several bugs in the Fortran parsing to make it work with the latest reference BLAS and LAPACK, leading to significant refactoring.
* Adding support for matrix exponentiation with Expokit.
* Adding support for loading various BLAS and LAPACK implementations.
* Removing the use of the FNV library in favor of native Lisp arrays.
* Adding a high-level interface to various functions.
* Adding function availability reporting.

The most important common design decision between CL-BLAPACK and MAGICL is allowing direct access
to the Fortran library functions by way of automatically generated Lisp bindings from the reference sources.
