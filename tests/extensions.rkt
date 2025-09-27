#lang racket

(module+ test
  (require "../testing.rkt"
           (for-syntax (only-in syntax/parse syntax-parse define/syntax-parse))
           syntax-spec-v3)

  (check-equal?
   (length (stream->list (solve (logic
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
   (length (stream->list (solve (logic
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
   (length (stream->list (solve (logic
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

  (check-all-solutions
   (logic
     ((a) is? {1})
     ((b) is {1})
     ((ok) is {#t})
     (((ok) is {#f}) :- ((a) is 1)))
   '())

  (define-dsl-syntax demand logic-macro
    (lambda (stx)
      (syntax-parse stx
        [(_ d f p ...+)
         #'(decls ((d) is? {#f})
                  (((d) is {#t}) :- p ...)
                  (forbid f ((d) is #f)))])))

  (check-all-solutions
   (logic
     ((species) is {'bear 'bird})
     (((name) is {'yogi}) :- ((species) is 'bear))
     (((name) is {'tweety}) :- ((species) is 'bird))
     ; this is the expansion of demand
     #;(decls ((d) is? {#f})
              (((d) is {#t}) :- ((name) is 'yogi))
              (decls ((ok) is {#t})
                     (((ok) is {#f}) :- ((d) is #f))))
     (demand d ok ((name) is 'yogi)))
   (list (set
          (fact 'species '() 'bear)
          (fact 'name '() 'yogi)
          (fact 'ok '() #t)
          (fact 'd '() #t))))

  (define-dsl-syntax deduce-name logic-macro
    (lambda (stx)
      (syntax-parse stx
        [(_ name)
         #'(decls (foo)
                  ((name) :- (foo)))])))

  (define-dsl-syntax has-foo-1 logic-macro
    (lambda (stx)
      (syntax-parse stx
        [(_)
         #'(foo 1)])))

  (check-all-solutions
   (logic
     ;; tests that the arities do not conflict, that the fresh symbol foo does
     ;; not appear in the solution, but bar (deduced using foo) DOES appear
     (deduce-name bar)
     ;; this will again test arity, and that foo 2 is not deduced
     (has-foo-1)
     ((foo 2) :- (foo 1)))
   (list (set (fact 'bar '())))))
