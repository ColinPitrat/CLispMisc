(load 'asdf)
(asdf:load-system :xlunit)

(load "labyrinth-solver.lisp")

(defclass labyrinth-solver-test (xlunit:test-case) ())

;(xlunit:def-test-method factorial-0-is-1 ((xlunit:test factorial-test))
;                        (xlunit:assert-equal (factorial 0) 1))
