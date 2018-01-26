.PHONY: all
all: magicl-tests

.PHONY: magicl-tests
magicl-tests: Dockerfile
	docker build -t magicl-tests -f Dockerfile ../

.PHONY: lisp-base
lisp-base: Dockerfile.base
	docker build -t rigetticomputing/lisp-base -f Dockerfile.base ../

.PHONY: test
test:
	docker run magicl-tests
