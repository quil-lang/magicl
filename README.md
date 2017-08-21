# MAGICL

_Matrix Algebra proGrams In Common Lisp_ by [Rigetti Computing](http://www.rigetti.com). (née FLAIL: _Finally, Linear Algebra In Lisp!_)

Load it with `(ql:quickload :magicl)`.

## Requirements

MAGICL has only been tested with recent (> 1.3.19) SBCLs on AMD64,
though there are no internal APIs being used.

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
Note that this setup has only been tested for Linux. No testing has yet been done on macOS and OS X. Support for automatic setup is in progress.

Download `expokit.tar.gz` from the [Expokit download page](https://www.maths.uq.edu.au/expokit/download.html). Extract the package, and change to the `expokit/fortran/` directory. You should find a file named `expokit.f`, which is the source code for the subroutines we wish to call. To make a shared library out of `expokit.f`, run the following command two commands: 

`gfortran -fPIC -c expokit.f`

`gfortran -shared -o expokit.so expokit.o -lblas -L<path-to-libblas.so> -llapack -L<path-to-liblapack.so>`

replacing the placeholder paths to `libblas.so` and `liblapack.so` appropriately. After running these commands, you should see an `expokit.so` file in your current directory.

Finally, into your `~/.bashrc` add the following command:

`LD_LIBRARY_PATH="$LD_LIBRARY_PATH:<path-to-expokit.so-directory>"; export LD_LIBRARY_PATH;`

where the placeholder should be replaced by the path to the directory that the newly created `expokit.so` file is. Run `source ~/.bashrc` for this to take effect.

As of right now, support is only availabe for the "small dense routines", i.e. those using Pade or Chebyshev (see the expokit `README` file for the exact files). 

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

In order to generate the bindings, you will need to download the Fortran 90 source files for BLAS and LAPACK. These can be found [here on netlib](http://www.netlib.org/lapack/). Once downloaded, you can re-generate the files with:

```
(ql:quickload :magicl-gen)
(in-package :magicl.generate-interface)
(generate-blapack-files #P"/path/to/lapack-3.7.1/")
```

Currently this will write to the source distribution directory of MAGICL, namely the files `blas-cffi.lisp` and `lapack-cffi.lisp`.


## History

MAGICL started as a fork of [CL-BLAPACK](https://github.com/blindglobe/cl-blapack).
