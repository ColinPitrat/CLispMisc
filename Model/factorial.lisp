(defun factorial (n)
  (if (> n 1)
    (* (factorial (- n 1)) n)
    1))

