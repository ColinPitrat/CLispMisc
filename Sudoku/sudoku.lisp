(defun parse-sudoku-line (line)
  (nreverse (reduce #'(lambda (x y) (push y x)) line :initial-value nil)))

(defun load-sudoku (file)
  (let ((sudoku nil))
    (with-open-file (stream file :direction :input)
      (do ((line (read-line stream nil) (read-line stream nil)))
        ((null line) (nreverse sudoku))
        (push (parse-sudoku-line line) sudoku)))))

(defun print-sudoku (sudoku)
  (map 'nil #'(lambda (x) (format t "~a~%" x)) sudoku))

(defun numbers ()
  (list #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defun indices ()
  (list 0 1 2 3 4 5 6 7 8))

(defun sub-indices ()
  (list 0 1 2))

(defun present-twice (n line)
  (> (count n line) 1))

(defun has-double-number (line)
  (reduce #'(lambda (x y) (or x (present-twice y line))) (numbers) :initial-value nil))

(defun are-sudoku-lines-ok (sudoku)
  (not (reduce #'(lambda (x y) (or x y)) (map 'list 'has-double-number sudoku))))

(defun get-column (n sudoku)
  (map 'list #'(lambda (x) (nth n x)) sudoku))

(defun are-sudoku-columns-ok (sudoku)
  (are-sudoku-lines-ok (map 'list #'(lambda (n) (get-column n sudoku)) (indices))))

(defun cartesian-product (l1 l2)
  (remove-if
    'null
    (reduce
      'append
      (map 'list
           #'(lambda (x)
               (map 'list #'(lambda (y) (list x y)) l2))
           l1))))

(defun half-indices-area (n)
  (map 'list #'(lambda (x) (+ (* n 3) x)) '(0 1 2)))

(defun indices-area (idx)
  (cartesian-product (half-indices-area (car idx)) (half-indices-area (cadr idx))))

(defun all-indices()
  (cartesian-product (indices) (indices)))

; idx is a list (row col)
(defun get-element (idx sudoku)
  (nth (cadr idx) (nth (car idx) sudoku)))

(defun set-element (idx sudoku n)
  (setf (nth (cadr idx) (nth (car idx) sudoku)) n))

(defun extract-sudoku-area (idx sudoku)
  (map 'list #'(lambda (idx) (get-element idx sudoku)) (indices-area idx)))

(defun all-sudoku-areas (sudoku)
  (map 'list #'(lambda (idx) (extract-sudoku-area idx sudoku)) (cartesian-product (sub-indices) (sub-indices))))

(defun are-sudoku-areas-ok (sudoku)
  (are-sudoku-lines-ok (all-sudoku-areas sudoku)))

(defun is-sudoku-valid (sudoku)
  (and
       ;(let ()
       ;  (format t "Is valid: ~%")
       ;  (print-sudoku sudoku)
       ;  (equal 1 1))
       (are-sudoku-lines-ok sudoku)
       (are-sudoku-columns-ok sudoku)
       (are-sudoku-areas-ok sudoku)
       ))

(defun finished-sudoku (sudoku)
  (and (is-sudoku-valid sudoku)
       (loop for idx in (all-indices)
             never (equalp #\. (get-element idx sudoku)))))

; TODO: solve-sudoku
(defun solve-sudoku (sudoku)
  (if (finished-sudoku sudoku) (return-from solve-sudoku sudoku))
  (loop for idx in (all-indices)
        when (equalp #\. (get-element idx sudoku))
        do (loop for n in (numbers)
                 do (set-element idx sudoku n)
                 (if (is-sudoku-valid sudoku) (if (solve-sudoku sudoku) (return-from solve-sudoku sudoku))))
        (set-element idx sudoku #\.)
        (return-from solve-sudoku nil)))

(defun pretty-solve-sudoku (file)
  (let ((sudoku (load-sudoku file)))
    (format t " === ~a === ~%" file)
    (print-sudoku sudoku)
    (format t "~%" file)
    (print-sudoku (solve-sudoku sudoku))))
