SBCL_BIN=sbcl
SBCL=$(SBCL_BIN) --noinform --no-userinit --no-sysinit --non-interactive

QUICKLISP_HOME=$(HOME)/quicklisp
QUICKLISP_SETUP=$(QUICKLISP_HOME)/setup.lisp
QUICKLISP=$(SBCL) --load $(QUICKLISP_HOME)/setup.lisp \
	--eval '(push (truename ".") asdf:*central-registry*)'

all: test

install-test-deps:
ifeq ($(UNAME_S),Linux)
ifeq ($(shell sed -n "s/^ID=//p" /etc/os-release),debian)
	echo "deb $(ZMQ_REPO) ./" >> /etc/apt/sources.list
	curl $(ZMQ_REPO)/Release.key | apt-key add -
	apt-get install -y libblas-dev libffi-dev liblapack-dev
else
	echo "Centos-based platforms unsupported"
endif
else
	echo "Non-Linux-based platforms unsupported"
endif

test:
	$(QUICKLISP) \
		 --eval "(ql:quickload :magicl-tests)" \
		 --eval '(asdf:test-system :magicl)'
