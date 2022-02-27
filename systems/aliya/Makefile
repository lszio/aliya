makefile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
pwd := $(patsubst %/,%,$(dir $(makefile_path)))

.PHONY: test

test:
	ros run -e "(asdf:load-asd #p\"$(pwd)/aliya.asd\") (ql:quickload :aliya) (asdf:test-system :aliya) (uiop:quit 0)"

