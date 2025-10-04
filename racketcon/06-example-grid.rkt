#lang racket

(require "racketcon-common.rkt"
         (for-space minidusa "racketcon-common.rkt"))

(define-dsl-syntax grid-facts logic-macro
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

#;(define-dsl-syntax grid-facts logic-macro
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

(define world
  (logic #:import ([s add1] [adjacent adjacent?])
    (grid-facts 5 6 coordinates)

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

(define sol-stream (solve world))

#;(soln->factset (stream-first sol-stream))