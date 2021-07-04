SBCL_BIN=sbcl
SBCL=$(SBCL_BIN) --noinform --no-userinit --no-sysinit --non-interactive

QUICKLISP_HOME=$(HOME)/quicklisp
QUICKLISP_SETUP=$(QUICKLISP_HOME)/setup.lisp
QUICKLISP=$(SBCL) --load $(QUICKLISP_HOME)/setup.lisp \
	--eval '(push (truename ".") asdf:*central-registry*)'

UNAME_S=$(shell uname -s)

QUICKLISP_BOOTSTRAP_URL=https://beta.quicklisp.org/quicklisp.lisp

all: test

###############################################################################
# SETUP
###############################################################################

$(QUICKLISP_SETUP):
	mkdir -p $(QUICKLISP_HOME)
	curl -o $(QUICKLISP_HOME)/quicklisp-bootstrap.lisp \
		$(QUICKLISP_BOOTSTRAP_URL)
	$(SBCL) --load $(QUICKLISP_HOME)/quicklisp-bootstrap \
		--eval "(quicklisp-quickstart:install :path \"$(QUICKLISP_HOME)\")"

system-index.txt: $(QUICKLISP_SETUP)
	$(QUICKLISP) \
		$(FOREST_SDK_FEATURE) \
		--eval '(ql:quickload "cffi-grovel")' \
		--eval '(ql:quickload "qvm-app")' \
		--eval '(ql:quickload "qvm-app-ng")' \
		--eval '(ql:write-asdf-manifest-file "system-index.txt")'

install-test-deps:
ifeq ($(UNAME_S),Linux)
ifeq ($(shell sed -n "s/^ID=//p" /etc/os-release),debian)
	apt-get install -y libblas-dev libffi-dev liblapack-dev
else ifeq ($(shell sed -n "s/^ID_LIKE=//p" /etc/os-release),debian)
	apt-get install -y libblas-dev libffi-dev liblapack-dev
else ifneq (,$(wildcard /etc/redhat-release))
	yum install -y blas-devel libffi-devel lapack-devel
else
	echo "Only Debian and RedHat based platforms are supported"
endif
else
	echo "Non-Linux-based platforms unsupported"
endif

test:
	$(QUICKLISP) \
		 --eval "(ql:quickload :magicl-tests)" \
		 --eval '(asdf:test-system :magicl)'

###############################################################################
# QUICKLISP UTILITES
###############################################################################

# Download and install Quicklisp.
quicklisp:
	curl -o /tmp/quicklisp.lisp "http://beta.quicklisp.org/quicklisp.lisp"
	sbcl --noinform --non-interactive \
             --load /tmp/quicklisp.lisp \
             --eval '(quicklisp-quickstart:install)'
	echo >> ~/.sbclrc
	echo '#-quicklisp(let ((i(merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))(when(probe-file i)(load i)))' >> ~/.sbclrc
	echo "#+quicklisp(push \"$(shell pwd | xargs dirname)/\" ql:*local-project-directories*)" >> ~/.sbclrc
	rm -f /tmp/quicklisp.lisp

# Update Quicklisp.
deps: $(QUICKLISP_SETUP)
	rm -f system-index.txt
	$(QUICKLISP) --eval '(ql:update-client :prompt nil)'
	$(QUICKLISP) --eval '(ql:update-dist "quicklisp" :prompt nil)'
