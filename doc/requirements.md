# Requirements

 * SBCL (> 1.3.19) or (>= 1.11) CCL on AMD64
 * quicklisp
 * [libffi](#libffi)
 * [BLAS and LAPACK](#blas-and-lapack)
 * [Expokit](#expokit)

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

On Linux, you can install BLAS and LAPACK (and gfortran) with your favorite package manager.
All that's expected is that you have `libgfortran.so.3`, `libblas.so`, `liblapack.so`.

For example, on Ubuntu this can be done with:

```bash
apt-get install liblapack-dev libblas-dev libgfortran3
```

### macOS and OS X

#### Homebrew

On macOS, Homebrew is the easiest way to get BLAS and LAPACK.
You can install them (along with `gfortran`, which is also required, and provided as part of `gcc`) with the following:

```bash
brew install gcc lapack
```

By default, `libgfortran` is searched for where Homebrew installs it,
namely `/usr/local/opt/gcc/lib/gcc/7/`. Likewise with `libblas` and `liblapack`,
located in `/usr/local/opt/lapack/lib/`.
Refer to [CFFI's documentation](https://common-lisp.net/project/cffi/manual/cffi-manual.html#g_t_002aforeign_002dlibrary_002ddirectories_002a)
on search paths to configure this more specifically. Libraries are searched here by default.

#### Accelerate

Another way to get BLAS and LAPACK on macOS is through the [_Accelerate_](https://developer.apple.com/documentation/accelerate) framework.
The required libraries are `libBLAS.dylib` and `libLAPACK.dylib`. While the Accelerate framework includes these libraries,
they are not a complete. We count more than 500 missing functions provided in the standard LAPACK distribution.
In order to use the system-provided libraries, add `:magicl.use-accelerate` to your `*features*` before compilation.

## Expokit

For all platforms, you will need to build Expokit, a Fortran library for matrix exponentiation. This usually is not included in mainstream software distribution mechanisms. As of right now, support is only available for the "small dense routines", i.e. those using Pade or Chebyshev (see the expokit `README` file for the exact files). 

You will need to download `expokit.tar.gz` from the [Expokit download page](https://www.maths.uq.edu.au/expokit/download.html). Extract the archive and `cd` into the directory `expokit/fortran/`. There should be a file named `expokit.f`. This is the only file needed to make the shared library.

### Linux

To make a shared library out of `expokit.f`, run the following command two commands: 

```bash
gfortran -fPIC -c expokit.f
gfortran -shared -o expokit.so expokit.o -lblas -L<path-to-libblas.so> -llapack -L<path-to-liblapack.so>
```

Replace the placeholder paths to `libblas.so` and `liblapack.so` appropriately. After running these commands, you should see an `expokit.so` file in your current directory.

Finally, add the following command into your `~/.bashrc`:

```bash
LD_LIBRARY_PATH="$LD_LIBRARY_PATH:<path-to-expokit.so-directory>"; export LD_LIBRARY_PATH;
```

where the placeholder should be replaced by the path to the directory that the newly created `expokit.so` file is. Run `source ~/.bashrc` for this to take effect.

### macOS and OS X

On a Mac, the commands are similar to Linux:

```bash
gfortran -fPIC -c expokit.f
gfortran -dynamiclib -o expokit.dylib expokit.o -lblas -llapack
```

If BLAS and LAPACK aren't in a regular location, then you may need to pass the linked path option `-L<path-to-lib>` for each library to the second command.

You will need to put `expokit.dylib` somwhere your system understands, like `/usr/local/lib/`. One may change the variable `DYLD_FALLBACK_LIBRARY_PATH` in a manner like below if the location shall be custom:

```bash
DYLD_FALLBACK_LIBRARY_PATH="$DYLD_FALLBACK_LIBRARY_PATH:<path-to-expokit.dylib-directory>"
export DYLD_FALLBACK_LIBRARY_PATH
```

## Intel's Math Kernel Library

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