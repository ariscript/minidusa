#lang racket

(require "racketcon-common.rkt"
         (for-space minidusa "racketcon-common.rkt"))

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