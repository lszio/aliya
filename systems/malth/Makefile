.PHONY: test

test:
	ros run -e "(asdf:load-asd (probe-file \"./malth.asd\")) (ql:quickload :malth/tests) (asdf:test-system :malth) (uiop:quit 0)"
