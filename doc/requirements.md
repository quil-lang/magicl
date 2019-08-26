# Requirements

 * SBCL (> 1.3.19) or (>= 1.11) CCL on AMD64
 * gfortran
 * quicklisp
 * [libffi](#libffi)
 * [BLAS and LAPACK](#blas-and-lapack)

## libffi

### Linux

On Linux, this can be installed with your favorite package manager.
For example, it can be installed on Ubuntu with:

```bash
apt-get install libffi-dev
```

### macOS and OS X

On new versions of macOS, this comes preinstalled.
If you find this to not be the case for you (this could potentially be an issue on OS X),
it can be installed with Homebrew using:

```bash
brew install libffi
```

## BLAS and LAPACK

### Linux

#### Package Manager

On Linux, you can install BLAS and LAPACK with your favorite package manager.
All that's expected is that you have `libblas.so` and `liblapack.so`.

For example, on Ubuntu this can be done with:

```bash
apt-get install liblapack-dev libblas-dev libgfortran3
```

You may need `libgfortran` if you built Fortran libraries. You can
install that with:

```bash
apt-get install libgfortran3
```

#### MKL

Intel's Math Kernel Library, or MKL, contains math routines, including BLAS and LAPACK,
that are specifically optimized for Intel processors. If the versions of BLAS and LAPACK
mentioned above do not suit your needs, you may consider installing MKL. To install MKL,
download the package from the [Intel website](https://software.intel.com/en-us/mkl)
and follow the instructions within the Install_Guide.pdf file.
The particular library of interest that will be installed is `libmkl_rt.so`.

It is also important to setup the proper environmental variables, especially the `LD_LIBRARY_PATH`
that specifies where to look for `libmkl_rt.so`; directions can be found
[here](https://software.intel.com/en-us/mkl-linux-developer-guide-automating-the-process-of-setting-environment-variables).

In order to use MKL in MAGICL, add `:magicl.use-mkl` to your `*features*` before compilation.

### macOS and OS X

#### Homebrew

On macOS, Homebrew is the easiest way to get BLAS and LAPACK.
You can install them with the following:

```bash
brew install gcc lapack
```

If you need `gfortran`, you can get that with

```bash
brew install gcc
```

By default, `libgfortran` is searched for where Homebrew installs it,
namely `/usr/local/opt/gcc/lib/gcc/7/`. Likewise with `libblas` and `liblapack`,
located in `/usr/local/opt/lapack/lib/`.
Refer to [CFFI's documentation](https://common-lisp.net/project/cffi/manual/cffi-manual.html#g_t_002aforeign_002dlibrary_002ddirectories_002a)
on search paths to configure this more specifically. Libraries are searched here by default.

#### Accelerate

Another way to get BLAS and LAPACK on macOS is through the [_Accelerate_](https://developer.apple.com/documentation/accelerate) framework.
The required libraries are `libBLAS.dylib` and `libLAPACK.dylib`. While the Accelerate framework includes these libraries,
they are not complete. We count more than 500 missing functions provided in the standard LAPACK distribution.
In order to use the system-provided libraries, add `:magicl.use-accelerate` to your `*features*` before compilation.

#### MKL

Intel's Math Kernel Library is also available for macOS, and directions for installing
it can be found here [here](https://software.intel.com/en-us/get-started-with-mkl-for-osx).

In order to use MKL in MAGICL, add `:magicl.use-mkl` to your `*features*` before compilation.

