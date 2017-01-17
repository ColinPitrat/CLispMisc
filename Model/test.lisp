(load 'asdf)
(asdf:load-system :xlunit)

(load "factorial.lisp")

(defclass factorial-test (xlunit:test-case) ())

(xlunit:def-test-method factorial-0-is-1 ((xlunit:test factorial-test))
                        (xlunit:assert-equal (factorial 0) 1))

(xlunit:def-test-method factorial-1-is-1 ((xlunit:test factorial-test))
                        (xlunit:assert-equal (factorial 1) 1))

(xlunit:def-test-method factorial-2-is-2 ((xlunit:test factorial-test))
                        (xlunit:assert-equal (factorial 2) 2))

(xlunit:def-test-method factorial-3-is-6 ((xlunit:test factorial-test))
                        (xlunit:assert-equal (factorial 3) 6))

(xlunit:def-test-method factorial-10-is-3628800 ((xlunit:test factorial-test))
                        (xlunit:assert-equal (factorial 10) 3628800))

(xlunit:def-test-method factorial-50-is-30414093201713378043612608166064768844377641568960512000000000000 ((xlunit:test factorial-test))
                        (xlunit:assert-equal (factorial 50) 30414093201713378043612608166064768844377641568960512000000000000))

