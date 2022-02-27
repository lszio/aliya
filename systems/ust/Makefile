pwd := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: test

test:
	ros run -e "(asdf:load-asd \"$(pwd)/ust.asd\") (ql:quickload :ust) (asdf:test-system :ust) (uiop:quit 0)"
