(load "factorial.lisp")

(loop for x from 1 to 50
      do (format t "~a! = ~a~%" x (factorial:factorial x)))
