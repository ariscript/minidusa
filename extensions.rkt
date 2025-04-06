#lang racket

(require rackunit
         syntax-spec-v3
         (for-syntax (only-in syntax/parse syntax-parse define/syntax-parse))
         (prefix-in rt: "runtime.rkt")
         "spec.rkt")

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
 (length (stream->list (rt:all (logic
                                (graph edge
                                       ('a ['b 'c 'e])
                                       ('c ['b 'd]))
                                ((node X) :- (edge X _))
                                ((is (color X) (choice 1 2 3)) :- (node X))

                                (is (ok) (choice #t))
                                ((is (ok) (choice #f)) :- (edge X Y)
                                                       (is (color X) C)
                                                       (is (color Y) C))))))
 24)
