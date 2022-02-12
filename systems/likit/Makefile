makefile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
pwd := $(patsubst %/,%,$(dir $(makefile_path)))

.PHONY: test

test:
	ros run -e "(asdf:load-asd #p\"$(pwd)/likit.asd\") (ql:quickload :likit) (asdf:test-system :likit) (uiop:quit 0)"
