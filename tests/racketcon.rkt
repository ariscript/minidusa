#lang racket

(require "../main.rkt"
         syntax-spec-v3
         gregor
         (for-syntax syntax/parse racket/list))

(define-dsl-syntax forbid logic-macro
  (syntax-parser
    [(_ p ...+)
     #'(decls
         ((ok) is {#t})
         (((ok) is {#f}) :- p ...))]))

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

(define-dsl-syntax undirected-graph logic-macro
  (syntax-parser
    [(_ edge-rel (node [neighbor ...]) ...)
     (define nodes (syntax-e #'(node ...)))
     (define neighbors (syntax-e #'((neighbor ...) ...)))
     (define/syntax-parse ((stxes ...) ...)
       (for/list ([node nodes]
                  [edges neighbors])
         (for/list ([edge (syntax-e edges)])
           #`(edge-rel #,node #,edge))))
     #'(decls
         stxes ... ...
         ((edge-rel X Y) :- (edge-rel Y X)))]))

(define coloring
  (logic
    (undirected-graph edge
      ['a ('b 'c)]
      ['b ('c)]
      ['d ('e)])
    (((color X) is {'red 'green 'blue}) :- (edge X _))
    (forbid ((color X) is C)
            ((color Y) is C)
            (edge X Y))))
#;(undirected-graph edge
  ['a ('b 'c 'd)]
  ['c ('b 'd 'e)]
  ['e ('b 'f)])


#;(decls
 (edge 'a 'b) (edge 'a 'c) (edge 'a 'd)
 (edge 'c 'b) (edge 'c 'd) (edge 'c 'e)
 (edge 'e 'b) (edge 'b 'f)
 (edge X Y) :- (edge Y X))

(define-dsl-syntax demand logic-macro
  (syntax-parser
    [(_ p ...+)
     #'(decls
         ((ok) is? {#f})
         (((ok) is {#t}) :- p ...)
         (forbid ((ok) is #f)))]))

#;(define stop-and-go
  (logic #:import ([s add1])
    ((run 0) is {'stop 'go})
    (((run M) is {'stop 'go})
     :- ((run N) is 'go) ((s N) is M))
    (forbid ((run 10) is 'go))))

(define world-raw
  (logic
    (xc 0) (xc 1) (xc 2) (xc 3) (xc 4)
    (yc 0) (yc 1) (yc 2) (yc 3) (yc 4) (yc 5)
    (adj 0 1) (adj 1 2) (adj 2 3) (adj 3 4) (adj 4 5)
    ((adj A B) :- (adj B A))
    
    ((coordinates X Y) :- (xc X) (yc Y))
    ((adjacent X1 Y1 X1 Y2) :- (coordinates X1 Y1) (adj Y1 Y2))
    ((adjacent X1 Y1 X2 Y1) :- (coordinates X1 Y1) (adj X1 X2))

    (((grid N M) is {'city 'forest 'mountain 'ocean 'plain}) :- (coordinates N M))
    (forbid ((grid X1 Y1) is 'city)
            ((grid X2 Y2) is 'city)
            (adjacent X1 Y1 X2 Y2))
    (demand ((grid X Y) is 'city))

    (((can-reach-water X Y) is {#t})  :- ((grid X Y) is 'ocean))
    (((can-reach-water X Y) is? {#t}) :- ((grid X Y) is _)
                                         (adjacent X Y N M)
                                         ((can-reach-water N M) is #t))
    (((can-reach-water X Y) is {#f})  :- ((grid X Y) is 'mountain))

    (forbid ((grid X Y) is 'city) ((can-reach-water X Y) is #f))))

(define world-importing
  (logic #:import ([s add1])
    (xc 0) (xc 1) (xc 2) (xc 3) (xc 4)
    (yc 0) (yc 1) (yc 2) (yc 3) (yc 4) (yc 5)
    
    ((coordinates X Y) :- (xc X) (yc Y))
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

(define (adjacent? x1 y1 x2 y2)
  (or (and (= (abs (- x1 x2)) 1) (= y1 y2))
      (and (= (abs (- y1 y2)) 1) (= x1 x2))))

(define world-using-adjacent
  (logic #:import ([s add1] [adjacent adjacent?])
    (xc 0) (xc 1) (xc 2) (xc 3) (xc 4)
    (yc 0) (yc 1) (yc 2) (yc 3) (yc 4) (yc 5)
    ((coordinates X Y) :- (xc X) (yc Y))
    
    (((grid N M) is {'city 'forest 'mountain 'ocean 'plain}) :- (coordinates N M))
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

(define-dsl-syntax grid-list logic-macro
  (syntax-parser
    [(_ n m coordinates)
     (define/syntax-parse (xcs ...)
       (for/list ([x (range (syntax->datum #'n))])
         #`(xc #,x)))
     (define/syntax-parse (ycs ...)
       (for/list ([x (range (syntax->datum #'n))])
         #`(yc #,x)))
     #'(decls
         xcs ...
         ycs ...
         ((coordinates X Y) :- (xc X) (yc Y)))]))

(define world-list
  (logic #:import ([s add1] [adjacent adjacent?])
    (grid-list 5 6 coordinates)

    (((grid N M) is {'city 'forest 'mountain 'ocean 'plain}) :- (coordinates N M))
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

(define-dsl-syntax grid-s logic-macro
  (syntax-parser
    [(_ n m coordinates)
     (define width (syntax->datum #'n))
     (define length (syntax->datum #'m))
     #`(decls #:import ([s add1] [less? <])
         (coordinates 0 0)
         ((coordinates N SM) :- (coordinates N M)
                                ((less? N #,width) is #t)
                                ((less? M #,(sub1 length)) is #t)
                                ((s M) is SM))
         ((coordinates SN M) :- (coordinates N M)
                                ((less? N #,(sub1 width)) is #t)
                                ((less? M #,length) is #t)
                                ((s N) is SN)))]))

(define world-gen
  (logic #:import ([s add1] [adjacent adjacent?])
    (grid-s 5 6 coordinates)
    
    (((grid N M) is {'city 'forest 'mountain 'ocean 'plain}) :- (coordinates N M))
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

(struct participant [name DOB])

;; fetch-participants : -> [SetOf Participant]
(define (fetch-participants)
  (list (participant "Dr. Racket" "01/28/1995")
        (participant "Zack" "07/26/2004")
        (participant "Ryan" "06/20/2004")
        (participant "Ari" "01/16/2005")))

;; participants->factset : [SetOf Participant] Symbol -> [SetOf Fact]
(define (participants->factset ps name)
  (for/set ([p ps])
    (fact name (list (participant-name p)) (participant-DOB p))))

;; same-year? : DateString DateString -> Bool
(define (same-year? dob1 dob2)
  (define (extract-year dob)
    (->year (parse-date dob "M/d/yyyy")))
  (equal? (extract-year dob1) (extract-year dob2)))

#;(soln->factset
 (stream-first
  (solve #:facts (participants->factset (fetch-participants) 'person)
    (logic #:import ([year=? same-year?]) #:extern (person)
      ((edge X Y) :- ((person X) is DOB1)
                     ((person Y) is DOB2)
                     ((year=? DOB1 DOB2) is #t))
      
      (((room X) is {'room1 'room2 'room3}) :- (edge X _))
      (forbid ((room X) is R)
              ((room Y) is R)
              (edge X Y))))))

#;(solve #:facts (participants->factset (fetch-participants) 'person)
  (logic #:import ([year=? same-year?]) #:extern (person)
    ((edge X Y) :- ((person X) is DOB1)
                   ((person Y) is DOB2)
                   ((year=? DOB1 DOB2) is #t))
    #| more computations |#))