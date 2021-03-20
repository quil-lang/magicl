# Developer How-To

This document describes how to add new functionality to MAGICL.

## Determining the Scope of your Contribution

So, you'd like to improve MAGICL. The first question to answer is,
"what is the scope of the improvement?" We offer here a few different
scenarios to help you get started.

## Writing a New Function

If you want to add an entirely new linear algebra routine, then there
are a few decisions to make. In this section, let's suppose we are
implementing `matmul` for matrix multiplication.

The first decision is: Should the routine support being implemented
with different backends? If yes, then you'll want to begin with a
*backend function definition*. a **backend function** is sort of like
a Common Lisp generic function. Backend functions themselves don't
actually implement anything, they instead just stipulate the existence
and documented behavior of a function. For our exmaple, we'd write (in
the `MAGICL` package):

```
(define-backend-function matmul (a b)
  "Multiply the matrices A and B.")
```

The second decision is: Do we want to implement this function with
CLOS extensibility in mind, or can it be written in pure MAGICL code?
Frequently, it's possible to do things more efficiently if you, say,
know you're working with `double-float` matrices. The cleanest way to
do this is to define a generic function and implement methods on
it. Since we will use this generic function to implement our backend
function, we need to tell it what to do if there are no methods found
for the generic function, and to continue searching in the next
backend. We can do this like so:

```
(define-compatible-no-applicable-method-behavior matmul-lisp)
(defgeneric matmul-lisp (a b)
  (:method ((a matrix) (b matrix))
    ; ...
    ))
```

Now we can tell MAGICL that `matmul-lisp` implements `matmul` in the
`:lisp` backend like so:

```
(define-backend-implementation matmul :lisp 'matmul-lisp)
```

In the LAPACK extension, we might write:

```
(define-compatible-no-applicable-method-behavior matmul-lapack)
(defgeneric matmul-lapack (a b)
  (:method ((a matrix/double-float) (b matrix/double-float))
    ;; ...
    ))
(define-backend-implementation matmul :lapack 'matmul-lapack)
```

In this scenario, we've presented *two* extension points for `matmul`:

1. We can extend it with new _backend implementations_. For instance,
BLAS offers matrix multiplication, and we could implement it with the
`:lapack` backend.

2. We can extend the Lisp implementation by writing additional methods
on the generic function `matmul-lisp`. We might want to do this if we
want to extend `matmul` to work on additional types. However, do note
that in good coding style, `matmul-lisp` should always conform to the
documentation of the _backend function_, **not** the _generic
function_.

If we wanted to forego making a generic function, we can instead just
write a lambda directly in the backend implementation:

```
;; assuming no MATMUL-LISP generic function
(define-backend-implementation matmul :lisp
  (lambda (a b)
    ;; ...
    ))
```

Of course, if no extensibility is desired whatsoever, one can just write 

**Important**: Do _not_ use generic functions for which methods are
implemented in extensions. This is problematic because extensions
might inadvertently overwrite one another's methods. Instead, we
recommend limiting the scope of a generic function to a backend, as
described above.

## Writing an Extension

Extensions are intended to add "foreign" functionality to MAGICL that
can't be achieved with pure Lisp. For instance, binding to C or
Fortran code should be done in an extension. If the code you intend to
write is pure Lisp, then an extension is probably not appropriate.

Let's suppose we are making an extension called `FOO`. You'll need to
create a new ASDF system. In `magicl.asd`, add `#:magicl/ext-foo` and
create a folder `src/extensions/foo`. (If you see it as a good
default, also add your system as a dependency to `#:magicl/fancy`.)
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