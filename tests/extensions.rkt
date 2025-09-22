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
   24)

  (check-equal?
   (stream->list
    (all (logic
           ((a) is? {1})
           ((b) is {1})
           ((ok) is {#t})
           (((ok) is {#f}) :- ((a) is 1)))))
   '())

  (define-dsl-syntax demand logic-macro
    (lambda (stx)
      (syntax-parse stx
        [(_ d f p ...+)
         #'(decls ((d) is? {#f})
                  (((d) is {#t}) :- p ...)
                  (forbid f ((d) is #f)))])))

  (check-equal?
   (stream->list
    (all (logic
           ((species) is {'bear 'bird})
           (((name) is {'yogi}) :- ((species) is 'bear))
           (((name) is {'tweety}) :- ((species) is 'bird))
           ; this is the expansion of demand
           #;(decls ((d) is? {#f})
                  (((d) is {#t}) :- ((name) is 'yogi))
                  (decls ((ok) is {#t})
                         (((ok) is {#f}) :- ((d) is #f))))
           (demand d ok ((name) is 'yogi)))))
   (list (solution (db-of
                    (fact 'species '() 'bear)
                    (fact 'name '() 'yogi)
                    (fact 'ok '() #t)
                    (fact 'd '() #t)))))

  (define-dsl-syntax mydecl logic-macro
    (lambda (stx)
      (syntax-parse stx
        [(_)
         #'(foo 1)])))

  (check-equal?
   (stream->list (all (logic
                        (mydecl)
                        ((foo 2) :- (foo 1)))))
   '()
   #;(list (solution (db-of (fact 'foo '(2)) (fact 'foo '(1)))))))
