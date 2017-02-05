(require "asdf")
(asdf:load-system :xlunit)

(load "sudoku.lisp")

(defclass sudoku-test (xlunit:test-case) ())

(xlunit:def-test-method parse-sudoku-empty-line ((xlunit:test sudoku-test))
                        (xlunit:assert-equal '(#\. #\. #\. #\. #\. #\. #\. #\. #\.) (parse-sudoku-line ".........")))

(xlunit:def-test-method parse-sudoku-line ((xlunit:test sudoku-test))
                        (xlunit:assert-equal '(#\. #\2 #\. #\4 #\. #\6 #\. #\8 #\.) (parse-sudoku-line ".2.4.6.8.")))

(xlunit:def-test-method present-twice-no ((xlunit:test sudoku-test))
                        (xlunit:assert-equal nil (present-twice 1 (parse-sudoku-line "122345678"))))

(xlunit:def-test-method present-twice-yes ((xlunit:test sudoku-test))
                        (xlunit:assert-equal nil (present-twice 2 (parse-sudoku-line "122345678"))))

(xlunit:def-test-method has-double-number-no ((xlunit:test sudoku-test))
                        (xlunit:assert-equal nil (has-double-number (parse-sudoku-line "123456789"))))

(xlunit:def-test-method has-double-number-yes ((xlunit:test sudoku-test))
                        (xlunit:assert-equal t (has-double-number (parse-sudoku-line "113456789"))))

(xlunit:def-test-method has-double-number-yes-with-holes ((xlunit:test sudoku-test))
                        (xlunit:assert-equal t (has-double-number (parse-sudoku-line ".1.456.49"))))

(xlunit:def-test-method has-double-number-no-with-holes ((xlunit:test sudoku-test))
                        (xlunit:assert-equal nil (has-double-number (parse-sudoku-line ".1.456.39"))))

(xlunit:def-test-method has-double-number-no-with-holes ((xlunit:test sudoku-test))
                        (xlunit:assert-equal nil (has-double-number (parse-sudoku-line ".1.456.39"))))

(xlunit:def-test-method are-sudoku-lines-ok-empty ((xlunit:test sudoku-test))
                        (xlunit:assert-equal t (are-sudoku-lines-ok (load-sudoku "tests/empty.txt"))))

(xlunit:def-test-method are-sudoku-lines-ok-diag ((xlunit:test sudoku-test))
                        (xlunit:assert-equal t (are-sudoku-lines-ok (load-sudoku "tests/diag.txt"))))

(xlunit:def-test-method are-sudoku-lines-ok-invalid ((xlunit:test sudoku-test))
                        (xlunit:assert-equal nil (are-sudoku-lines-ok (load-sudoku "tests/invalid_line.txt"))))

(xlunit:def-test-method get-column ((xlunit:test sudoku-test))
                        (xlunit:assert-equal '(#\. #\. #\3 #\. #\. #\. #\. #\. #\.) (get-column 2 (load-sudoku "tests/diag.txt"))))

(xlunit:def-test-method are-sudoku-columns-ok-empty ((xlunit:test sudoku-test))
                        (xlunit:assert-equal t (are-sudoku-columns-ok (load-sudoku "tests/empty.txt"))))

(xlunit:def-test-method are-sudoku-columns-ok-diag ((xlunit:test sudoku-test))
                        (xlunit:assert-equal t (are-sudoku-columns-ok (load-sudoku "tests/diag.txt"))))

(xlunit:def-test-method are-sudoku-columns-ok-invalid ((xlunit:test sudoku-test))
                        (xlunit:assert-equal nil (are-sudoku-columns-ok (load-sudoku "tests/invalid_column.txt"))))

(xlunit:def-test-method cartesian-product-nil-nil ((xlunit:test sudoku-test))
                        (xlunit:assert-equal nil (cartesian-product nil nil)))

(xlunit:def-test-method cartesian-product-2-nil ((xlunit:test sudoku-test))
                        (xlunit:assert-equal nil (cartesian-product '(0 1) nil)))

(xlunit:def-test-method cartesian-product-2-2 ((xlunit:test sudoku-test))
                        (xlunit:assert-equal '((0 2) (0 3) (1 2) (1 3)) (cartesian-product '(0 1) '(2 3))))

(xlunit:def-test-method half-indices-area-0 ((xlunit:test sudoku-test))
                        (xlunit:assert-equal '(0 1 2) (half-indices-area 0)))

(xlunit:def-test-method half-indices-area-1 ((xlunit:test sudoku-test))
                        (xlunit:assert-equal '(3 4 5) (half-indices-area 1)))

(xlunit:def-test-method half-indices-area-2 ((xlunit:test sudoku-test))
                        (xlunit:assert-equal '(6 7 8) (half-indices-area 2)))

(xlunit:def-test-method indices-area-0-0 ((xlunit:test sudoku-test))
                        (xlunit:assert-equal '((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)) (indices-area '(0 0))))

(xlunit:def-test-method get-element ((xlunit:test sudoku-test))
                        (xlunit:assert-equal #\3 (get-element '(2 3) (load-sudoku "tests/invalid_column.txt"))))

(xlunit:def-test-method get-element2 ((xlunit:test sudoku-test))
                        (xlunit:assert-equal #\4 (get-element '(4 3) (load-sudoku "tests/invalid_column.txt"))))

(xlunit:def-test-method get-element2 ((xlunit:test sudoku-test))
                        (xlunit:assert-equal #\5 (get-element '(6 3) (load-sudoku "tests/invalid_column.txt"))))

(xlunit:def-test-method set-element ((xlunit:test sudoku-test))
                        (xlunit:assert-equal #\3 (let ((sudoku (load-sudoku "tests/invalid_column.txt")))
                                                   (set-element '(2 3) sudoku #\3)
                                                   (get-element '(2 3) sudoku))))

(xlunit:def-test-method extract-sudoku-area ((xlunit:test sudoku-test))
                        (xlunit:assert-equal '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (extract-sudoku-area '(0 0) (load-sudoku "tests/area_1_full.txt"))))

(xlunit:def-test-method are-sudoku-areas-ok-yes ((xlunit:test sudoku-test))
                        (xlunit:assert-equal t (are-sudoku-areas-ok (load-sudoku "tests/area_1_full.txt"))))

(xlunit:def-test-method are-sudoku-areas-ok-no1 ((xlunit:test sudoku-test))
                        (xlunit:assert-equal nil (are-sudoku-areas-ok (load-sudoku "tests/area_1_invalid.txt"))))

(xlunit:def-test-method are-sudoku-areas-ok-no2 ((xlunit:test sudoku-test))
                        (xlunit:assert-equal nil (are-sudoku-areas-ok (load-sudoku "tests/area_9_invalid.txt"))))

(xlunit:def-test-method is-sudoku-valid-yes1 ((xlunit:test sudoku-test))
                        (xlunit:assert-equal t (is-sudoku-valid (load-sudoku "tests/area_1_full.txt"))))

(xlunit:def-test-method is-sudoku-valid-yes2 ((xlunit:test sudoku-test))
                        (xlunit:assert-equal t (is-sudoku-valid (load-sudoku "tests/diag.txt"))))

(xlunit:def-test-method is-sudoku-valid-yes3 ((xlunit:test sudoku-test))
                        (xlunit:assert-equal t (is-sudoku-valid (load-sudoku "tests/empty.txt"))))

(xlunit:def-test-method is-sudoku-valid-no1 ((xlunit:test sudoku-test))
                        (xlunit:assert-equal nil (is-sudoku-valid (load-sudoku "tests/area_1_invalid.txt"))))

(xlunit:def-test-method is-sudoku-valid-no2 ((xlunit:test sudoku-test))
                        (xlunit:assert-equal nil (is-sudoku-valid (load-sudoku "tests/invalid_column.txt"))))

(xlunit:def-test-method is-sudoku-valid-no3 ((xlunit:test sudoku-test))
                        (xlunit:assert-equal nil (is-sudoku-valid (load-sudoku "tests/invalid_line.txt"))))

(xlunit:textui-test-run (xlunit:get-suite sudoku-test))
