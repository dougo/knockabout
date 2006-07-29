(module move "swindle.ss"
  (require "seat.ss")
  (require "position.ss")
  (require "direction.ss")
  (require "dice.ss")
  (require "board.ss")
  (provide (all-defined))

  (defclass <move> ()
    (position :reader position)
    (direction :reader direction)
    :automaker #t)

  (defmethod (print-object (move <move>) esc? port)
    (echo :> port (position move) (direction move)))

  (defmethod (check-move (board <board>) (seat <seat>) (move <move>))
    (let* ((pos (position move))
	   (die (die-at board pos)))
      (unless die
	(raise-user-error "There's no die at that position."))
      (unless (equals? seat (owner die))
	(raise-user-error "That's not your die."))
      (check-direction (location board pos) (direction move))))

  (defmethod (legal-moves (die <die>))
    (let ((pos (position die)))
      (map (lambda (dir) (make-move pos dir))
	   (legal-directions (location (board die) pos)))))

  (defmethod (move! (board <board>) (move <move>))
    (let* ((die (die-at board (position move)))
	   (dir (direction move)))
      (let loop ((die die) (moves (face die)) (knocked? #f))
	(let ((pos (next-position die dir)))
	  (if (or (zero? moves) (off-board? (location board pos)))
	      ;; The last die to get knocked gets re-rolled.
	      (if knocked? (roll! die) die)
	      (let ((die2 (die-at board pos)))
		(if die2
		    ;; Transfer moves to next die.
		    (loop die2 moves #t)
		    ;; Move the die and decrement moves.
		    (begin (set! (die-at board pos) die)
			   (loop die (1- moves) knocked?)))))))))
)
