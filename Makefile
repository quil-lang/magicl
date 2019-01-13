SBCL_BIN=sbcl
SBCL=$(SBCL_BIN) --noinform --no-userinit --no-sysinit --non-interactive

QUICKLISP_HOME=$(HOME)/quicklisp
QUICKLISP_SETUP=$(QUICKLISP_HOME)/setup.lisp
QUICKLISP=$(SBCL) --load $(QUICKLISP_HOME)/setup.lisp \
	--eval '(push (truename ".") asdf:*central-registry*)'

test:
	$(QUICKLISP) \
		 --eval "(ql:quickload :magicl-tests)" \
		 --eval '(asdf:test-system :magicl)'
