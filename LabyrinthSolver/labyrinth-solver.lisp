(defun load-labyrinth (filename)
  (with-open-file (stream filename :direction :input)
    (do ((line (read-line stream nil) (read-line stream nil))
	 (labyrinth nil))
      ((null line) (nreverse labyrinth))
      (push (reduce #'(lambda (x y) (push x y)) line :from-end t :initial-value nil) labyrinth))))

(defun print-labyrinth (labyrinth)
  (map 'nil #'(lambda (x)
		(map 'nil #'(lambda (y)
			      (format t "~a" y))
		     x)
		(format t "~%"))
       labyrinth))

(defun line-contains (item line)
  (find item line))

(defun find-item (item labyrinth)
  (flet ((line-contains-item (line) (line-contains item line)))
    (list
      (position-if #'line-contains-item labyrinth)
      (position item (find-if #'line-contains-item labyrinth)))))

(defun find-entrance (labyrinth)
  (find-item #\A labyrinth))

(defun find-output (labyrinth)
  (find-item #\B labyrinth))

(defun item-at (pos labyrinth)
  (nth (nth 1 pos) (nth (nth 0 pos) labyrinth)))

(defun set-item-at (pos labyrinth item)
  (setf (nth (nth 1 pos) (nth (nth 0 pos) labyrinth)) item))

(defun shift-pos (pos delta)
  (list (+ (nth 0 pos) (nth 0 delta)) (+ (nth 1 pos) (nth 1 delta))))


(defun neighbours (pos)
  (map 'list
       (lambda (x) (shift-pos pos x))
       (list '(-1 0) '(0 1) '(1 0) '(0 -1))))

(defun find-path (start end labyrinth)
  (loop for pos in (neighbours start)
    do (if
	 (and (> (nth 0 pos) 0) (> (nth 1 pos) 0))
	 (progn
	   (if
	     (eq #\B (item-at pos labyrinth))
	     (print-labyrinth labyrinth))
	   (if
	     (eq #\  (item-at pos labyrinth)) 
	     (progn
	       (set-item-at pos labyrinth #\.)
	       (find-path pos end labyrinth)
	       (set-item-at pos labyrinth #\ )
	       )
	     )
	   )
	 )
    )
  )

(defun labyrinth-solve (labyrinth)
  (find-path (find-entrance labyrinth) (find-output labyrinth) labyrinth)
)
