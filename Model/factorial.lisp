(defpackage :factorial
  (:use :common-lisp)
  (:export :factorial))

(in-package :factorial)

(defun factorial (n)
  (if (> n 1)
    (* (factorial (- n 1)) n)
    1))

