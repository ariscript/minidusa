#lang racket

(require "../testing.rkt" syntax-spec-v3
         (for-syntax syntax/parse racket/list))

(define-dsl-syntax forbid logic-macro
  (lambda (stx)
    (syntax-parse stx
      [(_ p ...+)
       #'(decls ((x) is {#t})
           (((x) is {#f}) :- p ...))])))
(define reachability
  (logic
    (edge 'a 'b) (edge 'b 'c) (edge 'd 'e)
    ((edge X Y) :- (edge Y X))
    ((node X) :- (edge X _))

    ((reachable X Y) :- (edge X Y))
    ((reachable X Y) :- (edge Z Y) (reachable X Z))

    (((color X) is {'red 'green 'blue}) :- (node X))
    (forbid (edge X Y) ((color X) is C)
                       ((color Y) is C))))

#;(check-all-solutions
   reachability
   (list
    (set
     (fact 'reachable '(d d) NONE)
     (fact 'edge '(c b) NONE)
     (fact 'reachable '(c b) NONE)
     (fact 'edge '(c a) NONE)
     (fact 'reachable '(c a) NONE)
     (fact 'edge '(a c) NONE)
     (fact 'reachable '(a c) NONE)
     (fact 'reachable '(c c) NONE)
     (fact 'edge '(d e) NONE)
     (fact 'reachable '(d e) NONE)
     (fact 'reachable '(b b) NONE)
     (fact 'reachable '(e e) NONE)
     (fact 'reachable '(a a) NONE)
     (fact 'edge '(b a) NONE)
     (fact 'reachable '(b a) NONE)
     (fact 'edge '(e d) NONE)
     (fact 'reachable '(e d) NONE)
     (fact 'edge '(a b) NONE)
     (fact 'reachable '(a b) NONE)
     (fact 'edge '(b c) NONE)
     (fact 'reachable '(b c) NONE))))

(define-dsl-syntax undirected-graph logic-macro
  (lambda (stx)
    (syntax-parse stx
      [(_ edge-rel node-rel (node [neighbor ...]) ...)
       (define nodes (syntax-e #'(node ...)))
       (define neighbors (syntax-e #'((neighbor ...) ...)))
       (define/syntax-parse ((stxes ...) ...)
         (for/list ([node nodes]
                    [edges neighbors])
           (for/list ([edge (syntax-e edges)])
             #`(edge-rel #,node #,edge))))
       #'(decls stxes ... ...
           ((edge-rel X Y) :- (edge-rel Y X))
           ((node-rel X) :- (edge-rel X _)))])))

(define coloring
  (logic
    (undirected-graph
     edge node
     ['a ('b 'c)]
     ['b ('c)]
     ['d ('e)])
    (((color X) is {'red 'green 'blue}) :- (node X))
    (forbid ((color X) is C)
            ((color Y) is C)
            (edge X Y))))

(define-dsl-syntax demand logic-macro
  (lambda (stx)
    (syntax-parse stx
      [(_ p ...+)
       #'(decls ((x) is? {#f})
           (((x) is {#t}) :- p ...)
           (forbid ((x) is #f)))])))

(define stop-and-go
  (logic #:import ([s add1])
    ((run 0) is {'stop 'go})
    (((run M) is {'stop 'go})
     :- ((run N) is 'go) ((s N) is M))
    (forbid ((run 10) is 'go))))

(define world-raw
  (logic
    (coord 0) (coord 1) (coord 2) (coord 3) (coord 4)
    (adj 0 1) (adj 1 2) (adj 2 3) (adj 3 4)
    ((adj A B) :- (adj B A))
    
    ((coordinates X Y) :- (coord X) (coord Y))
    ((adjacent X1 Y1 X1 Y2) :- (coord X1) (adj Y1 Y2))
    ((adjacent X1 Y1 X2 Y1) :- (adj X1 X2) (coord Y1))

    (((grid N M) is {'city 'forest 'mountain 'ocean 'plain}) :- (coordinates N M))
    (forbid ((grid X1 Y1) is 'city)
            ((grid X2 Y2) is 'city)
            (adjacent X1 Y1 X2 Y2))
    (demand ((grid X Y) is 'city))

    (((can-reach-water X Y) is {#t}) :- ((grid X Y) is 'ocean))
    (((can-reach-water X Y) is? {#t}) :-
     ((grid X Y) is _)
     (adjacent X Y N M)
     ((can-reach-water N M) is #t))
    (((can-reach-water X Y) is {#f}) :- ((grid X Y) is 'mountain))

    (forbid ((grid X Y) is 'city) ((can-reach-water X Y) is #f))))

(define world-importing
  (logic #:import ([s add1])
    (coord 0) (coord 1) (coord 2) (coord 3) (coord 4)
    ((coordinates X Y) :- (coord X) (coord Y))
    ((adjacent X1 Y1 X1 Y2) :- (coordinates X1 Y1) (coordinates X1 Y2) ((s Y1) is Y2))
    ((adjacent X1 Y1 X2 Y1) :- (coordinates X1 Y1) (coordinates X2 Y1) ((s X1) is X2))
    ((adjacent X1 Y1 X2 Y2) :- (adjacent X2 Y2 X1 Y1))

    (((grid N M) is {'city 'forest 'mountain 'ocean 'plain}) :- (coordinates N M))
    (forbid ((grid X1 Y1) is 'city)
            ((grid X2 Y2) is 'city)
            (adjacent X1 Y1 X2 Y2))
    (demand ((grid X Y) is 'city))

    (((can-reach-water X Y) is {#t}) :- ((grid X Y) is 'ocean))
    (((can-reach-water X Y) is? {#t}) :-
     ((grid X Y) is _)
     ((can-reach-water N M) is #t)
     (adjacent X Y N M))
    (((can-reach-water X Y) is {#f}) :- ((grid X Y) is 'mountain))

    (forbid ((grid X Y) is 'city) ((can-reach-water X Y) is #f))))

(define-dsl-syntax square-list logic-macro
  (lambda (stx)
    (syntax-parse stx
      [(_ n coordinates)
       (define size (syntax->datum #'n))
       (define/syntax-parse ((coords ...) ...)
         (for/list ([x (range size)])
           (for/list ([y (range size)])
             #`(coordinates #,x #,y))))
       #'(decls coords ... ...)])))

(define world-list
  (logic #:import ([s add1])
    (square-list 5 coordinates)
    ((adjacent X1 Y1 X1 Y2) :- (coordinates X1 Y1) (coordinates X1 Y2) ((s Y1) is Y2))
    ((adjacent X1 Y1 X2 Y1) :- (coordinates X1 Y1) (coordinates X2 Y1) ((s X1) is X2))
    ((adjacent X1 Y1 X2 Y2) :- (adjacent X2 Y2 X1 Y1))

    (((grid N M) is {'city 'forest 'mountain 'ocean 'plain}) :- (coordinates N M))
    (forbid ((grid X1 Y1) is 'city)
            ((grid X2 Y2) is 'city)
            (adjacent X1 Y1 X2 Y2))
    (demand ((grid X Y) is 'city))

    (((can-reach-water X Y) is {#t}) :- ((grid X Y) is 'ocean))
    (((can-reach-water X Y) is? {#t}) :-
     ((grid X Y) is _)
     ((can-reach-water N M) is #t)
     (adjacent X Y N M))
    (((can-reach-water X Y) is {#f}) :- ((grid X Y) is 'mountain))

    (forbid ((grid X Y) is 'city) ((can-reach-water X Y) is #f))))

(define-dsl-syntax square-gen logic-macro
  (lambda (stx)
    (syntax-parse stx
      [(_ n coordinates)
       (define size (syntax->datum #'n))
       #`(decls #:import ([s add1] [less? <])
           (coordinates 0 0)
           ((coordinates N SM) :-
            (coordinates N M)
            ((less? N #,size) is #t)
            ((less? M #,(sub1 size)) is #t)
            ((s M) is SM))
           ((coordinates SN M) :-
            (coordinates N M)
            ((less? N #,(sub1 size)) is #t)
            ((less? M #,size) is #t)
            ((s N) is SN)))])))

(define world-gen
  (logic #:import ([s add1])
    (square-gen 5 coordinates)
    ((adjacent X1 Y1 X1 Y2) :- (coordinates X1 Y1) (coordinates X1 Y2) ((s Y1) is Y2))
    ((adjacent X1 Y1 X2 Y1) :- (coordinates X1 Y1) (coordinates X2 Y1) ((s X1) is X2))
    ((adjacent X1 Y1 X2 Y2) :- (adjacent X2 Y2 X1 Y1))

    (((grid N M) is {'city 'forest 'mountain 'ocean 'plain}) :- (coordinates N M))
    (forbid ((grid X1 Y1) is 'city)
            ((grid X2 Y2) is 'city)
            (adjacent X1 Y1 X2 Y2))
    (demand ((grid X Y) is 'city))

    (((can-reach-water X Y) is {#t}) :- ((grid X Y) is 'ocean))
    (((can-reach-water X Y) is? {#t}) :-
     ((grid X Y) is _)
     ((can-reach-water N M) is #t)
     (adjacent X Y N M))
    (((can-reach-water X Y) is {#f}) :- ((grid X Y) is 'mountain))

    (forbid ((grid X Y) is 'city) ((can-reach-water X Y) is #f))))

(define (adjacent? x1 y1 x2 y2)
  (or (and (= (abs (- x1 x2)) 1) (= y1 y2))
      (and (= (abs (- y1 y2)) 1) (= x1 x2))))

(define world-using-adjacent
  (logic #:import ([s add1] [adjacent adjacent?])
    (square-gen 5 coordinates)

    (((grid N M) is {'city 'forest 'mountain 'ocean 'plain}) :- (coordinates N M))
    (forbid ((grid X1 Y1) is 'city)
            ((grid X2 Y2) is 'city)
            ((adjacent X1 Y1 X2 Y2) is #t))
    (demand ((grid X Y) is 'city))

    (((can-reach-water X Y) is {#t}) :- ((grid X Y) is 'ocean))
    (((can-reach-water X Y) is? {#t}) :-
     ((grid X Y) is _)
     ((can-reach-water N M) is #t)
     ((adjacent X Y N M) is #t))
    (((can-reach-water X Y) is {#f}) :- ((grid X Y) is 'mountain))

    (forbid ((grid X Y) is 'city) ((can-reach-water X Y) is #f))))
