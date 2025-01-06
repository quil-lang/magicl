# Developer How-To

This document describes how to add new functionality to MAGICL.

## Determining the Scope of your Contribution

So, you'd like to improve MAGICL. The first question to answer is,
"what is the scope of the improvement?" We offer here a few different
scenarios to help you get started.

- Writing a new function

- Implementing a backend for an existing function

- Implementing a new extension and backend

The following sections describe these scenarios.


## Writing a New Function - Quick and Lazy Guide

This is the quick and lazy explanation. See the next section for the
full explanation.

Let's say we want to add a fused multiply-add function called
`fma`. All we need to do is write:

```
(define-extensible-function (fma fma-lisp) (a x b)
  (:method (a x b)
    (+ b (* a x))))
```

This defines `fma` as the main API function, and `fma-lisp` as the
generic function implementing `fma`. If we want to implement `fma` in
a specific backend, we can use `extend-function` to extend `fma` into the backend. For example, if we are extending this function
with BLAS acceleration, we can specify the `:blas` backend and write,

```
(extend-function (fma fma-blas :blas) (a x b)
  (:method (a x b)
    (some-funny-blas-function a x b)))
```

For a full explanation of this, as well as different and possibly more
efficient ways of doing this, see the next section.


## Writing a New Function

If you want to add an entirely new linear algebra routine, then there
are a few decisions to make. In this section, let's suppose we are
implementing `matmul` for matrix multiplication. (This is already
implemented, of course, as `magicl:@` and `magicl:mult`.)

The first decision is:

> Should the routine support being implemented with different backends?

Allowing for different backends in turn allows the function to be
implemented in various ways. For instance, we could implement a native
Lisp matmul as well as a LAPACK-accelerated one, so it seems our
answer is "yes" to this question. (With that said, not all functions
are deserving of such extension. For instance, more likely than not, a
"double matrix" function can be implemented cleanly using existing
MAGICL primitives. With that said, it's always possible to take a
non-extensible function and turn it into an extensible one.)

Since we answered yes, you'll want to begin with a *backend function
definition*. A **backend function** is sort of like a Common Lisp
generic function in that it can have multiple implementations. Backend
functions themselves don't actually implement anything, they instead
just stipulate the existence and documented behavior of a
function. For our example, we'd write (in the `MAGICL` package):

```
(define-backend-function matmul (a b)
  "Multiply the matrices A and B.")
```

This would be the public-facing function that users could use and
refer to. What's next is to actually implement it, but we have another
decision to make about the implementation.

The second decision is:

> Do we want to implement this function with CLOS extensibility in
mind?

There are several reasons one might want CLOS extensibility. A few are:

1. You want a polymorphic function. For instance, we might instead
want `matmul` to work on `MATRIX`/`MATRIX` and `MATRIX`/`VECTOR`
arguments. It is trivial to do this with a pair of CLOS methods.

2. You want dispatch on the matrix element type, e.g.,
`MATRIX/DOUBLE-FLOAT`, for efficiency. It is trivial to dispatch on
this in a CLOS method.

In any case, suppose we decide to implement `matmul` with a generic
function. What should be the behavior if a method is not found for a
given set of arguments? Typically, this would result in an error, but
in our case, we'd like to have MAGICL continue searching for
alternative implementations of `matmul`. We can do this by "defining
MAGICL-compatible behavior" on the generic function, like so:

```
(defgeneric matmul-lisp (a b)
  (:method ((a matrix) (b matrix))
    ; ...
    ))
(define-compatible-no-applicable-method-behavior matmul-lisp)
```

Without the `define-compat...` line, a failure to find the method will
ultimately be a failure of `matmul` itself.

Note that in this case, we had to name the generic function something
other than `matmul`. Indeed, `matmul` (as defined by
`define-backend-function`) is a true function.

Finally, we can tell MAGICL that `matmul-lisp` implements `matmul` in the
`:lisp` backend like so:

```
(define-backend-implementation matmul :lisp 'matmul-lisp)
```

If we wanted to accelerate `matmul` with BLAS, we could implement
`matmul` in the BLAS extension. There, we might write:

```
(defgeneric matmul-blas (a b)
  (:method ((a matrix/double-float) (b matrix/double-float))
    ;; ...
    ))
(magicl:define-compatible-no-applicable-method-behavior matmul-blas)
(magicl:define-backend-implementation magicl:matmul :blas 'matmul-blas)
```

Note that we did *not* use the `matmul-lisp` generic function and
instead created another one specifically for use within BLAS, and in
this case, the BLAS-accelerated function only works for
`matrix/double-float`.

In total, we've presented *two* extension points for `matmul`:

1. We can extend it with new _backend implementations_. For instance,
BLAS offers matrix multiplication, and we could implement it with the
`:blas` backend, like shown above. We can implement `matmul` in any
number of backends this way.

2. We can extend the Lisp implementation by writing additional methods
on the generic function `matmul-lisp`. We might want to do this if we
want to extend `matmul` to work on additional types. However, do note
that in good coding style, `matmul-lisp` should always conform to the
documentation of the _backend function_, **not** the _generic
function_.

**Important**: Do _not_ write generic functions in `MAGICL/CORE` and
methods in extensions! Methods are *not* the way to extend
functionality for different backends. Using methods like this is
problematic because extensions might inadvertently overwrite one
another's definitions. Instead, we recommend limiting the scope of a
generic function to an extension or backend, as described above.

If we wanted to forego making a generic function, we can instead just
write a lambda directly in the backend implementation:

```
;; assuming no MATMUL-LISP generic function
(define-backend-implementation matmul :lisp
  (lambda (a b)
    ;; ...
    ))
```

### Conveniently Defining Extensible Functions

This pattern of defining a generic function as an implementation of a
backend function is common. For convenience, one can use
`define-extensible-function` in MAGICL to do the ceremony of defining
the backend function, defining compatible method behavior, and
defining a generic function implementing the backend function for
`:lisp` (or any other backend):

```
;; in MAGICL core
(define-extensible-function (matmul matmul-lisp) (a b)
  (:method (a b)
    ;; ...
    ))
```

The syntax mostly follows `defgeneric`, except that you need to
specify two names. The above code makes the `matmul` backend function
as implemented by the `matmul-lisp` generic function.

Later on, when implementing the BLAS accelerated `matmul`, we can use
`extend-function` and specify the backend we're extending it
with.

```
;; in BLAS extension
(extend-function (matmul matmul-blas :blas) (a b)
  (:method (a b)
    ;; ...
    ))
```

As the names imply, `define-extensible-function` defines a backend function and extends it,
and `extend-function` extends an already-defined backend function.

In general, we suggest always having a pure Lisp version if feasible.


## Implementing a Backend for an Existing Function

Implementing a backend for an existing function is easy, assuming that
function is already a "backend function". The previous section covers
this, except it's not necessary to make the backend function to begin
with.

It is imperative that the implementation of the function follows the
semantics described in the function's documentation.


## Writing an Extension

Extensions are intended to add "foreign" functionality to MAGICL that
can't be achieved with pure Lisp. For instance, binding to C or
Fortran code should be done in an extension. If the code you intend to
write is pure Lisp, then an extension is probably not appropriate.

As a matter of terminology, an **extension** is a module of code that
can be loaded after `MAGICL/CORE` is loaded, and a **backend** is a
symbol that is used to discriminate between different implementations
of a given function. Most often, an extension will define a new
backend.

Let's suppose we are making an extension called `FOO`. You'll need to
create a new ASDF system. In `magicl.asd`, add `#:magicl/ext-foo` and
create a folder `src/extensions/foo`. (If you see it as a good
default, also add your system as a dependency to `#:magicl`.)
You should define a new package for your extension, like
`#:magicl-foo`. If your extension has C bindings, then you'll want a
`load-libs.lisp` file. Use `src/extensions/expokit/load-libs.lisp` as
a model. Of important note are:

1. The definition of the foreign library.
2. Recording the foreign library in the `*foreign-libraries*` variable.
3. Loading the library.
4. Defining a backend named `:foo` for this extension.

Now you can add functionality as usual. Your extension may expose new
functionality through its package, but when possible, it is preferred
to expose it through the backend framework.