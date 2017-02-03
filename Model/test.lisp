(load 'asdf)
(asdf:load-system :xlunit)

(load "factorial.lisp")

(defpackage :factorial-test
  (:use :common-lisp :xlunit :factorial))

(in-package :factorial-test)

(defclass factorial-test (test-case) ())

(defmacro test-factorial (name expr)
  `(def-test-method ,name ((test factorial-test)) ,expr))

(test-factorial factorial-0-is-1 (assert-equal (factorial 0) 1))

(test-factorial factorial-1-is-1 (assert-equal (factorial 1) 1))

(test-factorial factorial-2-is-2 (assert-equal (factorial 2) 2))

(test-factorial factorial-3-is-6 (assert-equal (factorial 3) 6))

(test-factorial factorial-10-is-3628800 (assert-equal (factorial 10) 3628800))

(test-factorial
  factorial-50-is-huge
  (assert-equal
    (factorial 50)
    30414093201713378043612608166064768844377641568960512000000000000))


(textui-test-run (get-suite factorial-test))
