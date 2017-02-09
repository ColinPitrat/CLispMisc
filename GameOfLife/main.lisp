(load "gameoflife.lisp")

(in-package :gameoflife)

;(load-and-show-evolution "board/pentadecathlon.txt" 16)
(show-evolution (board-randomize (make-instance 'gol :width 127 :height 20)) 1000)
