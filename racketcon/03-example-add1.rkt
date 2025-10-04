#lang racket

(require "racketcon-common.rkt"
         (for-space minidusa "racketcon-common.rkt"))

(define world-importing
  (logic #:import ([s add1])
    (xc 0) (xc 1) (xc 2) (xc 3) (xc 4)
    (yc 0) (yc 1) (yc 2) (yc 3) (yc 4) (yc 5)
    ((coordinates X Y) :- (xc X) (yc Y))
    
    ;; rewritten to use add1 instead of (adj n n+1) facts
    ((adjacent X1 Y1 X1 Y2) :- (coordinates X1 Y1) (coordinates X1 Y2) ((s Y1) is Y2))
    ((adjacent X1 Y1 X2 Y1) :- (coordinates X1 Y1) (coordinates X2 Y1) ((s X1) is X2))
    ((adjacent X1 Y1 X2 Y2) :- (adjacent X2 Y2 X1 Y1))

    (((grid N M) is {'city 'forest 'mountain 'ocean 'plain}) :- (coordinates N M))
    (forbid ((grid X1 Y1) is 'city)
            ((grid X2 Y2) is 'city)
            (adjacent X1 Y1 X2 Y2))
    (demand ((grid X Y) is 'city))

    (((can-reach-water X Y) is {#t})  :- ((grid X Y) is 'ocean))
    (((can-reach-water X Y) is? {#t}) :- ((grid X Y) is _)
                                         ((can-reach-water N M) is #t)
                                         (adjacent X Y N M))
    (((can-reach-water X Y) is {#f})  :- ((grid X Y) is 'mountain))

    (forbid ((grid X Y) is 'city) ((can-reach-water X Y) is #f))))
