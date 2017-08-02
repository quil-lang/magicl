# MAGICL

_Matrix Algebra proGrams In Common Lisp_ by [Rigetti Computing](www.rigetti.com). (née FLAIL: _Finally, Linear Algebra In Lisp!_)

Load it with `(ql:quickload :magicl)`.

## Requirements

MAGICL has only been tested with recent (> 1.3.19) SBCLs on AMD64,
though there are no internal APIs being used.

All UNIX systems require `libffi`. You can usually install this with
your package manager.

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