#lang racket

(require "racketcon-common.rkt"
         (for-space minidusa "racketcon-common.rkt"))

;; much easier to implement in Racket
(define (adjacent? x1 y1 x2 y2)
  (or (and (= (abs (- x1 x2)) 1) (= y1 y2))
      (and (= (abs (- y1 y2)) 1) (= x1 x2))))

(define world-using-adjacent
  (logic #:import ([s add1] [adjacent adjacent?])
    (xc 0) (xc 1) (xc 2) (xc 3) (xc 4)
    (yc 0) (yc 1) (yc 2) (yc 3) (yc 4) (yc 5)
    ((coordinates X Y) :- (xc X) (yc Y))
    
    (((grid N M) is {'city 'forest 'mountain 'ocean 'plain}) :- (coordinates N M))

    ;; rewritten to use new adjacent function
    (forbid ((grid X1 Y1) is 'city)
            ((grid X2 Y2) is 'city)
            ((adjacent X1 Y1 X2 Y2) is #t))
    (demand ((grid X Y) is 'city))

    (((can-reach-water X Y) is {#t})  :- ((grid X Y) is 'ocean))
    (((can-reach-water X Y) is? {#t}) :- ((grid X Y) is _)
                                         ((can-reach-water N M) is #t)
                                         ((adjacent X Y N M) is #t))
    (((can-reach-water X Y) is {#f})  :- ((grid X Y) is 'mountain))

    (forbid ((grid X Y) is 'city) ((can-reach-water X Y) is #f))))
