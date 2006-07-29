;; Board positions and locations.

(module position "swindle.ss"
  (provide (all-defined))

  ;; Row 0 is the northwest gutter, column 0 is the west gutter.
  (define <position> <pair>)
  (defmethod (pos (row <exact-integer>) (col <exact-integer>)) (cons row col))
  (defmethod (row (pos <position>)) (car pos))
  (defmethod (col (pos <position>)) (cdr pos))
  
  ;; A location is a classification of a position with respect to a
  ;; board.  It is either off the board, on the map, in a side gutter,
  ;; or in a gutter corner.
  (defclass <location> ())

  (defclass <off-board> (<location>))
  (define *off-board* (make <off-board>))
  (defmethod (off-board) *off-board*)
  (defmethod (off-board? (loc <location>)) #f)
  (defmethod (off-board? (loc <off-board>)) #t)

  (defclass <on-board> (<location>))

  (defclass <on-map> (<on-board>))
  (define *on-map* (make <on-map>))
  (defmethod (on-map) *on-map*)
  (defmethod (on-map? (loc <location>)) #f)
  (defmethod (on-map? (loc <on-map>)) #t)

  (defclass <in-gutter> (<on-board>)
    (index :reader index :initarg :i))

  ;; Side gutters are numbered clockwise from 0 = northwest.
  (defclass <side-gutter> (<in-gutter>) :automaker #t)

  ;; Gutter corners are numbered clockwise from 0 = north.
  (defclass <gutter-corner> (<in-gutter>) :automaker #t)
)
