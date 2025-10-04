#lang racket

(require "racketcon-common.rkt"
         (for-space minidusa "racketcon-common.rkt"))

(define world-raw
  (logic
    ;; rows/columns defining a 5 x 6 grid
    (xc 0) (xc 1) (xc 2) (xc 3) (xc 4)
    (yc 0) (yc 1) (yc 2) (yc 3) (yc 4) (yc 5)
    ((coordinates X Y) :- (xc X) (yc Y))
    
    ;; adjacent rows/columns
    (adj 0 1) (adj 1 2) (adj 2 3) (adj 3 4) (adj 4 5)
    ((adj A B) :- (adj B A))
    ;; defining adjacency on the grid    
    ((adjacent X1 Y1 X1 Y2) :- (coordinates X1 Y1) (adj Y1 Y2))
    ((adjacent X1 Y1 X2 Y1) :- (coordinates X1 Y1) (adj X1 X2))

    ;; assign a type of terrain to each coordinate on the grid
    (((grid N M) is {'city 'forest 'mountain 'ocean 'plain}) :- (coordinates N M))

    ;; two cities cannot be adjacent
    (forbid ((grid X1 Y1) is 'city)
            ((grid X2 Y2) is 'city)
            (adjacent X1 Y1 X2 Y2))
    ;; there must be at least one city
    (demand ((grid X Y) is 'city))

    ;; tracks whether the ocean is reachable without crossing mountains
    (((can-reach-water X Y) is {#t})  :- ((grid X Y) is 'ocean))
    (((can-reach-water X Y) is? {#t}) :- ((grid X Y) is _)
                                         (adjacent X Y N M)
                                         ((can-reach-water N M) is #t))
    (((can-reach-water X Y) is {#f})  :- ((grid X Y) is 'mountain))

    ;; all cities must be able to reach water
    (forbid ((grid X Y) is 'city) ((can-reach-water X Y) is #f))))
