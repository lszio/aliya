pwd := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: test

test:
	ros run -e "(asdf:load-asd \"$(pwd)/aliya.asd\") (ql:quickload :aliya) (asdf:test-system :aliya) (uiop:quit 0)"
