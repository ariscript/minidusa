#lang racket

(module+ test
  (require rackunit
           syntax-spec-v3
           (for-syntax (only-in syntax/parse syntax-parse define/syntax-parse))
           "../main.rkt")

  (check-equal?
   (length (stream->list (all (logic
                                (edge 'a 'b)
                                (edge 'b 'c)
                                (edge 'a 'c)
                                (edge 'c 'd)
                                (edge 'a 'e)
                                ((edge X Y) :- (edge Y X))
                                ((node X) :- (edge X _))
                                (((color X) is {1 2 3}) :- (node X))

                                ((ok) is {#t})
                                (((ok) is {#f}) :- (edge X Y)
                                                ((color X) is C)
                                                ((color Y) is C))))))
   24)

  (define-dsl-syntax graph logic-macro
    (lambda (stx)
      (syntax-parse stx
        [(_ name (node [neighbor ...]) ...)
         (define nodes (syntax-e #'(node ...)))
         (define neighbors (syntax-e #'((neighbor ...) ...)))
         (define/syntax-parse ((stxes ...) ...)
           (for/list ([node nodes]
                      [edges neighbors])
             (for/list ([edge (syntax-e edges)])
               #`(name #,node #,edge))))
         #'(decls stxes ... ...
                  ((name X Y) :- (name Y X)))])))

  ;; EXAMPLE EXPANSION:
  #;(graph edge
           ('a ['b 'c 'e])
           ('c ['b 'd]))
  ;; EXPANDS TO
  #;(decls (edge 'a 'b)
           (edge 'a 'c)
           (edge 'a 'e)
           (edge 'c 'b)
           (edge 'c 'd)
           ((edge X Y) :- (edge Y X)))

  (check-equal?
   (length (stream->list (all (logic
                                (graph edge
                                       ('a ['b 'c 'e])
                                       ('c ['b 'd]))
                                ((node X) :- (edge X _))
                                (((color X) is {1 2 3}) :- (node X))

                                ((ok) is {#t})
                                (((ok) is {#f}) :- (edge X Y)
                                                ((color X) is C)
                                                ((color Y) is C))))))
   24)

  (define-dsl-syntax forbid logic-macro
    (lambda (stx)
      (syntax-parse stx
        [(_ name p ...+)
         #'(decls ((name) is {#t})
                  (((name) is {#f}) :- p ...))])))

  (check-equal?
   (length (stream->list (all (logic
                                (graph edge
                                       ('a ['b 'c 'e])
                                       ('c ['b 'd]))
                                ((node X) :- (edge X _))
                                (((color X) is {1 2 3}) :- (node X))

                                (forbid ok
                                        (edge X Y)
                                        ((color X) is C)
                                        ((color Y) is C))))))
   24))
