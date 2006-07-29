;;; Knockabout, a game designed by Greg Lam.
;;; http://www.pair-of-dice.com/games/knockabout.html

(module knockabout "swindle.ss"
  (require* "seat.ss")
  (require* "position.ss")
  (require* "direction.ss")
  (require* "dice.ss")
  (require* "board.ss")
  (require* "move.ss")
  (provide (all-defined))

  (defclass <game> ()
    (board :reader board :type <board>
	   :initializer (lambda initargs (make <board> . initargs)))
    (whose-turn :accessor whose-turn :type <seat>
		:initarg :whose-turn :initvalue 0))

  (defmethod (initialize (this <game>) initargs)
    (call-next-method)
    (set! (game (board this)) this))

  (defmethod (legal-moves (game <game>))
    (mappend legal-moves (legal-dice game)))

  ;; A list of dice that are legal to be moved.
  (defmethod (legal-dice (game <game>))
    (filter (lambda (die) (equals? (owner die) (whose-turn game)))
	    (dice-list (board game))))

  (defmethod (check-move (game <game>) (seat <seat>) (move <move>))
    (unless (equals? seat (whose-turn game))
      (raise-user-error "It's not your turn."))
    (check-move (board game) seat move))

  ;; Move the die at a position on the board of a game a direction.
  (defmethod (move! (game <game>) (move <move>))
    (set! (whose-turn game) (next-seat (whose-turn game)))
    (move! game move))
)
