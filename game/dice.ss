;;; Dice on the board.

(module dice "swindle.ss"
  (require "seat.ss")
  (require "position.ss")
  (provide (all-defined))

  (defclass <die> ()
    ;; Each die is owned by a seat.
    (owner :reader owner :initarg :owner :type <seat>)
    ;; The face showing, which determines its movement.
    (face :accessor face :initarg :face :type <exact-integer>)
    ;; The board the die is on.
    (board :accessor board :initvalue #f)
    ;; The position of the die on the board.
    (position :accessor position :initvalue #f))

  (defmethod (row (die <die>)) (row (position die)))
  (defmethod (col (die <die>)) (col (position die)))

  (defmethod (roll! (die <die>))
    (set! (face die) (1+ (random (size die))))
    die)

  (defmethod (config (die <die>))
    (list (row die) (col die) (size die) (face die)))

  (defmethod (print-object (die <die>) esc? port)
    (print (config die) port))

  (defclass <d4> (<die>))
  (defmethod (size (die <d4>)) 4)
  (defmethod (die-class (size = 4)) <d4>)

  (defclass <d6> (<die>))
  (defmethod (size (die <d6>)) 6)
  (defmethod (die-class (size = 6)) <d6>)

  (defclass <d8> (<die>))
  (defmethod (size (die <d8>)) 8)
  (defmethod (die-class (size = 8)) <d8>)
)
