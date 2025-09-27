#lang racket/base

(module+ test
  (require "../testing.rkt")

  (define simple-program
    (program
     (list (rule (rule-frag 'foo '(1) '(a) #f) '()))
     '()))

  (check-equal?
   (has (stream-first (all simple-program)) 'foo 1)
   #t)
  
  (check-equal?
   (has (stream-first (all simple-program)) 'foo 2)
   #f)

  (check-equal?
   (get (stream-first (all simple-program)) 'foo 1)
   'a)

  (check-exn
   ;; TODO: make this error message better
   #rx""
   (lambda () (get (stream-first (all simple-program)) 'foo 2)))
  
  ;; we can run imported relations
  (check-equal?
   (stream->list (all (program
                       (list (rule (rule-frag 'foo '() '() #f)
                                   (list (fact add1 '(0) 1))))
                       '())))
   (list (solution (db-of (make-fact 'foo '())))))

  (check-equal?
   (stream->list (all (program
                       (list (rule (rule-frag 'foo '() '() #f)
                                   (list (fact * '(0 1 2) 1))))
                       '())))
   (list (solution (db-of))))

  ;; example where we bind a variable based on an import
  (check-equal?
   (stream->list (all (program
                       (list (rule (rule-frag 'foo '() (list (variable 'X)) #f)
                                   (list (fact + '(1 2 3 4) (variable 'X)))))
                       '())))
   (list (solution (db-of (make-fact 'foo '() 10))))))