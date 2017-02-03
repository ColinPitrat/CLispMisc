(load 'asdf)
(asdf:load-system :xlunit)

(load "gameoflife.lisp")

(defpackage :gameoflife-test
  (:use :common-lisp :xlunit :gameoflife))

(in-package :gameoflife-test)

(defclass gameoflife-test (test-case) ())

(defmacro test-gameoflife (name expr)
  `(def-test-method ,name ((test gameoflife-test)) ,expr))

(test-gameoflife idx-0-0-is-0 (assert-equal (idx 0 0 (make-instance 'gol)) 0))

(test-gameoflife idx-1-0-is-1 (assert-equal (idx 1 0 (make-instance 'gol)) 1))

(test-gameoflife idx-1-1-is-11 (assert-equal (idx 1 1 (make-instance 'gol)) 11))

(test-gameoflife idx-10-10-is-0 (assert-equal (idx 10 10 (make-instance 'gol)) 0))

(test-gameoflife idx--1--1-is-81 (assert-equal (idx -1 -1 (make-instance 'gol)) 99))

(test-gameoflife get-cell (assert-equal (get-cell 0 0 (make-instance 'gol)) 0))

(test-gameoflife set-alive-get-cell-0-0 (assert-equal
					  (let ((board (make-instance 'gol)))
					    (set-alive 0 0 board)
					    (get-cell 0 0 board))
					  1))

(test-gameoflife set-dead-get-cell-0-0 (assert-equal
					 (let ((board (make-instance 'gol)))
					   (set-alive 0 0 board)
					   (set-dead 0 0 board)
					   (get-cell 0 0 board))
					 0))

(test-gameoflife get-neighbours-indices-3-3 (assert-equal
					      (get-neighbours-indices 3 3)
					      '((2 2) (2 3) (2 4) (3 2) (3 4) (4 2) (4 3) (4 4))))

(test-gameoflife count-neighbours-0 (assert-equal
				      (let ((board (make-instance 'gol)))
					(set-alive 0 0 board)
					(count-neighbours 0 0 board))
				      0))

(test-gameoflife count-neighbours-1 (assert-equal
				      (let ((board (make-instance 'gol)))
					(set-alive 0 0 board)
					(count-neighbours 1 1 board))
				      1))

(test-gameoflife count-neighbours-8 (assert-equal
				      (let ((board (make-instance 'gol)))
					(set-alive 0 0 board)
					(set-alive 0 1 board)
					(set-alive 0 2 board)
					(set-alive 1 0 board)
					(set-alive 1 1 board)
					(set-alive 1 2 board)
					(set-alive 2 0 board)
					(set-alive 2 1 board)
					(set-alive 2 2 board)
					(count-neighbours 1 1 board))
				      8))

(defmacro test-next-state (state neighbours expected)
  `(test-gameoflife (format t nil "next-state-~a-~a" ,state ,neighbours)
		    (assert-equal (next-state ,state ,neighbours) ,expected)))

(defmacro test-next-state-both (neighbours expected)
  `(test-next-state 0 ,neighbours ,expected)
  `(test-next-state 1 ,neighbours ,expected))

; A cell always end-up dead ...
(test-next-state-both 0 0)
(test-next-state-both 1 0)
(test-next-state-both 4 0)
(test-next-state-both 5 0)
(test-next-state-both 6 0)
(test-next-state-both 7 0)
(test-next-state-both 8 0)
; A cell always end-up except if it has 2 or 3 neighbours
(test-next-state 0 2 0)
(test-next-state 0 3 1)
(test-next-state 1 2 1)
(test-next-state 1 3 1)

(test-gameoflife all-indices-3-3 (assert-equal (all-indices (make-instance 'gol :width 3 :height 3))
					       '((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2))))

(test-gameoflife next-generation-1 (assert-equal '(0 0 0 0 0 1 1 0 0 1 1 0 0 0 0 0)
						 (let ((board (make-instance 'gol :width 4 :height 4)))
						   (set-alive 1 1 board)
						   (set-alive 1 2 board)
						   (set-alive 2 1 board)
						   (set-alive 2 2 board)
						   (next-generation board))))

(textui-test-run (get-suite gameoflife-test))
