(defpackage :gameoflife
  (:use :common-lisp)
  (:export :idx :gol :set-alive :set-dead :get-cell :get-neighbours-indices
	   :count-neighbours :next-state :all-indices :next-generation))

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
			l1))
	       l2)))

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
      (loop for y from 0 below height by 1 collect y)
      (loop for x from 0 below width by 1 collect x))))

(defun next-generation (board)
  (map 'list
       #'(lambda (idx)
	   (let ((x (car idx))
		 (y (cadr idx)))
	     (next-state (get-cell x y board) (count-neighbours x y board))))
       (all-indices board)))
