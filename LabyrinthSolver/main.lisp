(load "labyrinth-solver.lisp")

;(format t "50! = ~a" (factorial 50))
;(format t "~a~%" (load-labyrinth "test1.txt"))
(print-labyrinth (load-labyrinth "test1.txt"))
(format t "~a~%" (find-entrance (load-labyrinth "test1.txt")))
(format t "~a~%" (find-output (load-labyrinth "test1.txt")))
(let ((labyrinth (load-labyrinth "test1.txt")))
  (format t "~a~%" (item-at (find-entrance labyrinth) labyrinth))
  (format t "~a~%" (item-at (find-output labyrinth) labyrinth)))
(format t "~a~%" (neighbours (list 2 2)))
(labyrinth-solve (load-labyrinth "test1.txt"))
(labyrinth-solve (load-labyrinth "test2.txt"))
