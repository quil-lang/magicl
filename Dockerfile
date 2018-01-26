# Dockerfile for running the MAGICL test suite
FROM rigetticomputing/lisp-base

# add source, load systems, and run tests 
ADD . /src/magicl
RUN sbcl --noinform --non-interactive --eval '(ql:quickload :magicl)' --quit
RUN sbcl --noinform --non-interactive --eval '(ql:quickload :magicl-tests)' --quit
ENTRYPOINT sbcl --noinform --non-interactive --eval '(asdf:test-system :magicl)' --quit
