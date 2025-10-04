#lang racket

(require "../main.rkt"
         syntax-spec-v3
         (for-syntax syntax/parse racket/list))

(provide (all-from-out syntax-spec-v3
                       "../main.rkt")
         adjacent?
         (for-syntax (all-from-out syntax/parse racket/list))
         (for-space minidusa
                    forbid
                    demand))

(define-dsl-syntax forbid logic-macro
  (syntax-parser
    [(_ p ...+)
     #'(decls
         ((ok) is {#t})
         (((ok) is {#f}) :- p ...))]))

(define-dsl-syntax demand logic-macro
  (syntax-parser
    [(_ p ...+)
     #'(decls
         ((ok) is? {#f})
         (((ok) is {#t}) :- p ...)
         (forbid ((ok) is #f)))]))

(define (adjacent? x1 y1 x2 y2)
  (or (and (= (abs (- x1 x2)) 1) (= y1 y2))
      (and (= (abs (- y1 y2)) 1) (= x1 x2))))
