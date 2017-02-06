(defpackage :gameoflife
  (:use :common-lisp)
  (:export :idx :gol :set-alive :set-dead :get-cell :get-neighbours-indices
	   :count-neighbours :next-state :all-indices :next-generation
	   :load-game-of-life :print-board))

(in-package :gameoflife)

(defclass gol ()
  ((width
     :initarg :width
     :initform 10)
   (height
     :initarg :height
     :initform 10)
   (cells
     :initarg :cells)))

(defmethod initialize-instance :after ((board gol) &key)
  (let ((width (slot-value board 'width))
	(height (slot-value board 'height)))
    (setf (slot-value board 'cells)
	  (make-list (* width height) :initial-element 0))))

(defun idx (x y board)
  (let ((width (slot-value board 'width))
	(height (slot-value board 'height)))
    (+ (mod x width) (* (mod y height) width))))

(defmacro get-cell (x y board)
  `(nth (idx ,x ,y ,board) (slot-value ,board 'cells)))

(defun set-cell (x y val board)
  (setf (get-cell x y board) val))

(defun set-alive (x y board)
  (set-cell x y 1 board))

(defun set-dead (x y board)
  (set-cell x y 0 board))

(defun cartesian-product (l1 l2)
  (reduce 'append
	  (map 'list
	       #'(lambda (x)
		   (map 'list
			#'(lambda (y) (list x y))
			l2))
	       l1)))

(defun neighbourhood-1d-indices (z)
  (map 'list #'(lambda (m) (+ z m)) '(-1 0 1)))

(defun get-neighbours-indices (x y)
  (remove (list x y)
	  (cartesian-product (neighbourhood-1d-indices x)
			     (neighbourhood-1d-indices y))
	  :test 'equalp))

(defun next-state (state neighbours)
  (case neighbours
    (2 state)
    (3 1)
    (otherwise 0)))

(defun count-neighbours (x y board)
  (reduce #'(lambda (a b) (+ a (get-cell (car b) (cadr b) board))) (get-neighbours-indices x y) :initial-value 0))

(defun all-indices (board)
  (let ((width (slot-value board 'width))
	(height (slot-value board 'height)))
    (cartesian-product
      (loop for x from 0 below width by 1 collect x)
      (loop for y from 0 below height by 1 collect y))))

(defun next-generation (board)
  (let ((copy-board (make-instance 'gol :width (slot-value board 'width) :height (slot-value board 'height))))
    (loop for idx in (all-indices board)
	  do
	  (let ((x (car idx))
		(y (cadr idx)))
	    (set-cell x y (next-state (get-cell x y board) (count-neighbours x y board)) copy-board)))
    (return-from next-generation copy-board)))

(defun read-integer (stream)
  (parse-integer (read-line stream)))

(defun load-game-of-life (file)
  (with-open-file (stream file :direction :input)
    (let* ((width (read-integer stream))
	   (height (read-integer stream))
	   (board (make-instance 'gol :width width :height height)))
      (do ((l (read-line stream) (read-line stream nil 'eof))
	   (y 0 (+ 1 y))
	   (x 0 0))
	((eq l 'eof) board)
	(loop for v across l
	      do
	      (set-cell x y (digit-char-p v) board)
	      (setf x (+ 1 x)))))))

(defun print-board (board)
  (loop for y from 0 below (slot-value board 'height)
	do
	(loop for x from 0 below (slot-value board 'width)
	      do
	      (format t (if (equalp 0 (get-cell x y board)) " " "X")))
	(format t "~%"))
  (format t "~%"))

(defun show-evolution (file generations)
  (do ((board (load-game-of-life file) (next-generation board))
       (gen 0 (+ 1 gen)))
    ((equalp generations gen) board)
    (print-board board)
    (sleep 0.1)))
