(load 'asdf)
(asdf:load-system :xlunit)

(load "pendu.lisp")

(defclass pendu-test (xlunit:test-case) ())

(xlunit:def-test-method known-word-with-empty-letter-list ((xlunit:test pendu-test))
                        (xlunit:assert-equal (known-word "marathon" nil) "________"))

(xlunit:def-test-method known-word-with-some-unique-letters ((xlunit:test pendu-test))
                        (xlunit:assert-equal (known-word "marathon" (list #\m #\t)) "m___t___"))

(xlunit:def-test-method known-word-with-some-repeated-letters ((xlunit:test pendu-test))
                        (xlunit:assert-equal (known-word "marathon" (list #\a #\t)) "_a_at___"))

(xlunit:def-test-method known-word-with-some-absent-letters ((xlunit:test pendu-test))
                        (xlunit:assert-equal (known-word "marathon" (list #\t #\z #\w)) "____t___"))

(xlunit:def-test-method complete-with-completed-word ((xlunit:test pendu-test))
                        (xlunit:assert-equal (complete "marathon" (list #\a #\t #\h #\m #\n #\o #\r)) T))

(xlunit:def-test-method complete-with-missing-letter ((xlunit:test pendu-test))
                        (xlunit:assert-equal (complete "marathon" (list #\a #\w #\h #\m #\n #\o #\r)) nil))

(xlunit:def-test-method complete-with-no-letter ((xlunit:test pendu-test))
                        (xlunit:assert-equal (complete "marathon" nil) nil))

