;; Movement directions.

(module direction "swindle.ss"
  (require "position.ss")
  (require (only (lib "1.ss" "srfi") find))
  (provide (all-defined))

  (defclass <direction> ()
    (name :reader name :initarg :name))

  (defmethod (print-object (dir <direction>) esc? port)
    (display (name dir) port))

  ;; Board directions are numbered clockwise from 0 = north.
  (defclass <board-direction> (<direction>)
    (row :reader row)
    (col :reader col)
    :automaker #t)

  (define *board-directions*
    (list
     (make-board-direction 'n  -1  0)
     (make-board-direction 'ne  0 +1)
     (make-board-direction 'se +1 +1)
     (make-board-direction 's  +1  0)
     (make-board-direction 'sw  0 -1)
     (make-board-direction 'nw -1 -1)))
     
  (defmethod (dir (i <exact-integer>)) (ref *board-directions* (modulo i 6)))
  (defmethod (dir (s <symbol>))
    (find (lambda (dir) (equals? (name dir) s)) *board-directions*))

  (defclass <gutter-direction> (<direction>))

  (defclass <clockwise> (<gutter-direction>))
  (define *clockwise* (make <clockwise> :name 'cw))
  (defmethod (dir (s = 'cw)) *clockwise*)

  (defclass <counter-clockwise> (<gutter-direction>))
  (define *counter-clockwise* (make <counter-clockwise> :name 'ccw))
  (defmethod (dir (s = 'ccw)) *counter-clockwise*)

  ;; Translate from gutter location/direction to board direction.
  (defmethod (board-direction (loc <side-gutter>) (cw <clockwise>))
    (dir (+ (index loc) 1)))
  (defmethod (board-direction (loc <gutter-corner>) (cw <clockwise>))
    (dir (+ (index loc) 2)))
  (defmethod (board-direction (loc <in-gutter>) (ccw <counter-clockwise>))
    (dir (- (index loc) 2)))

  ;; The next position when moving in a board direction.
  (defmethod (next-position (p <position>) (d <board-direction>))
    (pos (+ (row p) (row d)) (+ (col p) (col d))))

  ;; The directions that a die can move from a location.
  (defmethod (legal-directions (loc <on-map>))
    *board-directions*)
  (defmethod (legal-directions (loc <in-gutter>))
    (list *clockwise* *counter-clockwise*))

  (defmethod (check-direction (loc <location>) (dir <direction>))
    (raise-user-error "That die can't move in that direction."))
  (defmethod (check-direction (loc <on-map>) (dir <board-direction>))
    (void))
  (defmethod (check-direction (loc <in-gutter>) (dir <gutter-direction>))
    (void))
)
