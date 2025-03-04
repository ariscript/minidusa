#lang racket

(provide filteri)

;; filteri: {X} [X Nat -> Any] [ListOf X] -> [ListOf X]
;; Like `filter`, but also provides the index to the filter predicate.
(define (filteri pred? l)
  (for/fold ([idx 0]
             [res '()]
             #:result (reverse res))
            ([elem l])
    (values (add1 idx) (if (pred? elem idx) (cons elem res) res)))
  ;; easier-to-understand but less performant version:
  #;(map car
         (filter (lambda (p) (pred? (car p) (cdr p)))
                 (enumerate l))))

(module+ test
  (require rackunit)

  (check-equal?
   (filteri (lambda (_ i) (odd? i)) '(a b c d e f))
   '(b d f)))
