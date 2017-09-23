# MAGICL

_Matrix Algebra proGrams In Common Lisp_ by [Rigetti Computing](http://www.rigetti.com). (née FLAIL: _Finally, Linear Algebra In Lisp!_)

Load it with `(ql:quickload :magicl)`.

## Requirements

MAGICL has only been tested with recent (> 1.3.19) SBCLs on AMD64.

All UNIX systems require `libffi`. You can usually install this with
your package manager.

All UNIX systems also require [Expokit](https://www.maths.uq.edu.au/expokit/) for matrix exponentiation. Download and setup instructions can be found in the Expokit subsection below.

### Linux

Install both gfortran and LAPACK with your favorite package manager. All that's expected is that you have `libgfortran.so.3`, `libblas.so`, `liblapack.so`.

### macOS and OS X

On macOS, there are at least two options for using BLAS and LAPACK. The first is to use the system-provided libraries, and the second are to install them yourself.

#### Homebrew-Provided LAPACK

[Homebrew](https://brew.sh/) is a package manager for macOS, which has LAPACK available. One can install it with:

```
brew install gfortran lapack
```

By default, `libgfortran` is searched for where Homebrew installs it, namely `/usr/local/opt/gcc/lib/gcc/7/`. Likewise with `libblas` and `liblapack`, located in `/usr/local/opt/lapack/lib/`. Refer to [CFFI's documentation](https://common-lisp.net/project/cffi/manual/cffi-manual.html#g_t_002aforeign_002dlibrary_002ddirectories_002a) on search paths to configure this more specifically.

Libraries are searched here by default.

#### System-Provided _Accelerate_ Framework

MacOS provides optimized BLAS and LAPACK libraries as a part of the [_Accelerate_](https://developer.apple.com/documentation/accelerate) framework. The libraries are `libBLAS.dylib` and `libLAPACK.dylib`.

While the Accelerate framework includes these libraries, they are not a complete. We count more than 500 missing functions provided in the standard LAPACK distribution.

In order to use the system-provided libraries, add `:magicl.use-accelerate` to your `*features*` before compilation.

### Intel processors

Intel's Math Kernel Library, or MKL, contains math routines, including BLAS and LAPACK, that are specifically optimized for Intel processors. To install MKL, download the package from the [Intel website](https://software.intel.com/en-us/mkl) and follow the instructions within the Install_Guide.pdf file. The particular library of interest that will be installed is `libmkl_rt.so`.

It is also important to setup the proper environmental variables, especially the `LD_LIBRARY_PATH` that specifies where to look for `libmkl_rt.so`; directions can be found [here](https://software.intel.com/en-us/mkl-linux-developer-guide-automating-the-process-of-setting-environment-variables).

In order to use MKL in MAGICL, add `:magicl.use-mkl` to your `*features*` before compilation.

### Expokit

For all platforms, you will need to build Expokit, a Fortran library for matrix exponentiation. This usually is not included in mainstream software distribution mechanisms. As of right now, support is only available for the "small dense routines", i.e. those using Pade or Chebyshev (see the expokit `README` file for the exact files). 

You will need to download `expokit.tar.gz` from the [Expokit download page](https://www.maths.uq.edu.au/expokit/download.html). Extract the archive and `cd` into the directory `expokit/fortran/`. There should be a file named `expokit.f`. This is the only file needed to make the shared library.

#### Linux

To make a shared library out of `expokit.f`, run the following command two commands: 

```
gfortran -fPIC -c expokit.f
gfortran -shared -o expokit.so expokit.o -lblas -L<path-to-libblas.so> -llapack -L<path-to-liblapack.so>
```

Replace the placeholder paths to `libblas.so` and `liblapack.so` appropriately. After running these commands, you should see an `expokit.so` file in your current directory.

Finally, add the following command into your `~/.bashrc`:

`LD_LIBRARY_PATH="$LD_LIBRARY_PATH:<path-to-expokit.so-directory>"; export LD_LIBRARY_PATH;`

where the placeholder should be replaced by the path to the directory that the newly created `expokit.so` file is. Run `source ~/.bashrc` for this to take effect.

#### macOS and OS X

On a Mac, the commands are similar to Linux:

```
gfortran -fPIC -c expokit.f
gfortran -dynamiclib -o expokit.dylib expokit.o -lblas -llapack
```

If BLAS and LAPACK aren't in a regular location, then you may need to pass the linked path option `-L<path-to-lib>` for each library to the second command.

You will need to put `expokit.dylib` somwhere your system understands, like `/usr/local/lib/`. One may change the variable `DYLD_FALLBACK_LIBRARY_PATH` in a manner like below if the location shall be custom:

```
DYLD_FALLBACK_LIBRARY_PATH="$DYLD_FALLBACK_LIBRARY_PATH:<path-to-expokit.dylib-directory>"
export DYLD_FALLBACK_LIBRARY_PATH
```

## Showing Available Functions

As said, some distributions of a library don't actually provide all of the functions of a the reference BLAS and LAPACK. One can look at a summary of available and unavailable functions with the function `magicl:print-availability-report`. By default, it will show all functions and their availability. There are three arguments to fine-tune this behavior:

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

This library takes the approach of automatically generating the bindings to BLAS and LAPACK without relying on any special tools.

In order to generate the bindings, you will need to download the Fortran 90 source tarballs for [BLAS/LAPACK](http://www.netlib.org/lapack/) and [Expokit](https://www.maths.uq.edu.au/expokit/download.html). Once downloaded, extract the tarballs into a directory and re-generate the bindings with the following commands:

```
(ql:quickload :magicl-gen)
(in-package :magicl.generate-interface)
(generate-blapack-files #P"/path/to/lapack-3.7.1/")
(generate-expokit-files #P"/path/to/expokit/")
```

Currently this will write to the source distribution directory of MAGICL, namely the files `blas-cffi.lisp`, `lapack-cffi.lisp`, and `expokit-cffi.lisp`.

## Running Tests

Load MAGICL and do `(asdf:test-system :magicl)`.

## Quirks and Issues

Currently this library is SBCL-only. The non-portable code is in `with-array-pointers.lisp`.


## History and Credits

MAGICL development started at Rigetti Computing by Robert Smith and Joe Lin in 2017.

[CL-BLAPACK](https://github.com/blindglobe/cl-blapack) is a library developed by Ryan Rifin and Evan Monroig. Rigetti Computing created a fork of this library and renamed it MAGICL, and made significant changes that departed from the original design, including:

* Fixing several bugs in the Fortran parsing to make it work with the latest reference BLAS and LAPACK, leading to significant refactoring.
* Adding support for matrix exponentiation with Expokit.
* Adding support for loading various BLAS and LAPACK implementations.
* Removing the use of the FNV library in favor of native Lisp arrays.
* Adding a high-level interface to various functions.
* Adding function availability reporting.

The most important common design decision between CL-BLAPACK and MAGICL is allowing direct access to the Fortran library functions by way of automatically generated Lisp bindings from the reference sources.