(defun prompt-number (string)
  (format t "~a: " string) (read nil 'eof nil))

(defun prompt-string (string)
  (format t "~a: " string) (read-line))

(defun game-internal-loop (random-state)
  (let ((answer (random 1000 random-state))
        (max_essais 12))
    ; Uncomment to cheat
    ;(format t "~a~%" answer)
    (do ((essais 1 (+ 1 essais))
         (essai 0 0) 
         (gagne nil)
         (fini nil))
      ((or fini gagne) gagne)
      (let ()
        (setq essai (prompt-number "Votre propositon: "))
        (cond
          ((> essai answer) (format t "Trop grand~%"))
          ((< essai answer) (format t "Trop petit~%"))
          ((= essai answer) (setq gagne T)))
        (if (>= essais max_essais) (setq fini T))))))

(defun game ()
  (do ((state (make-random-state T))
       (exit nil))
    (exit nil)
    (let ()
      (format t "J'ai choisi un nombre entre 0 et 1000. Trouvez le en 12 essais !~%")
      (format t (if (game-internal-loop state) "Bravo, vous avez gagné !~%" "Dommage, vous avez perdu !~%"))
      (if (equalp "y" (prompt-string "Quitter ? [y/n]")) (setq exit T)))))

(game)
