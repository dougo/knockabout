;;; Game seats.

#lang swindle

(provide (all-defined))

(define <seat> <exact-integer>)
(define *seats* (list 0 1))

;; The next seat in turn order.
(defmethod (next-seat (seat <seat>)) (modulo (1+ seat) 2))

