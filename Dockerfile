# Dockerfile for running the MAGICL test suite
FROM ubuntu:16.04

# update package list
RUN apt-get update -y

# install prereqs for build
RUN apt-get install -y \
    build-essential \
    liblapack-dev \
    libgfortran3 \
    libblas-dev \
    libffi-dev \
    libz-dev \
    sbcl \
    wget \
    curl \
    git

# sbcl (needs sbcl, wget, libz-dev, build-essential for make)
WORKDIR /src
RUN wget http://prdownloads.sourceforge.net/sbcl/sbcl-1.4.3-source.tar.bz2
RUN tar -xf sbcl-1.4.3-source.tar.bz2
WORKDIR /src/sbcl-1.4.3
RUN bash make.sh --fancy
RUN bash install.sh
RUN rm /src/sbcl-1.4.3-source.tar.bz2
RUN rm -rf /src/sbcl-1.4.3

# quicklisp (needs curl)
RUN curl -o /tmp/quicklisp.lisp 'http://beta.quicklisp.org/quicklisp.lisp'
RUN sbcl --noinform --non-interactive --load /tmp/quicklisp.lisp --eval '(quicklisp-quickstart:install)'
RUN echo >> ~/.sbclrc
RUN echo '#-quicklisp(let ((i(merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))(when(probe-file i)(load i)))' >> ~/.sbclrc
RUN echo '#+quicklisp(push "/src" ql:*local-project-directories*)' >> ~/.sbclrc
RUN rm -f /tmp/quicklisp.lisp
RUN sbcl --eval "(ql:update-all-dists :prompt nil)" --quit

# expokit (needs blas, gfortran, lapack)
WORKDIR /src
RUN wget https://www.maths.uq.edu.au/expokit/expokit.tar.gz
RUN tar -xf expokit.tar.gz
WORKDIR /src/expokit/fortran
RUN gfortran -fPIC -c expokit.f
RUN gfortran -shared -o expokit.so expokit.o -lblas -L/usr/lib/libblas.so -llapack -L/usr/lib/liblapack.so
RUN mv expokit.so /usr/lib

# add source, load systems, and run tests 
ADD . /src/magicl
RUN sbcl --noinform --non-interactive --eval '(ql:quickload :magicl)' --quit
RUN sbcl --noinform --non-interactive --eval '(ql:quickload :magicl-tests)' --quit
ENTRYPOINT sbcl --noinform --non-interactive --eval '(asdf:test-system :magicl)' --quit
