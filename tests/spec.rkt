#lang racket/base

(module+ test
  (require "../testing.rkt")

  ;; since the expanded program has syntax objects in it, expansion is hard
  ;; to test. perhaps we don't actually want to test this (just extensionally,
  ;; or test the compilation helpers directly), but we already wrote these.
  ;; so, this tests by smushing the syntax objects in the ast to just the
  ;; underlying symbols, then comparing for structural equality.
  ;; hygiene is tested separately through extensional integration tests.
  (define (check-equal/unhygenic? ast ast-with-symbols)
    (check-equal? (smush-syntax/program ast) ast-with-symbols))
  
  (check-equal/unhygenic?
   (logic
     (foo 1))
   (program (list (rule (rule-frag 'foo '(1) '() #f)
                              '()))
               '()))
  
  (check-equal/unhygenic?
   (logic
     ((foo 2) :- (foo 1))
     (foo 1))
   (program (list (rule (rule-frag 'foo '(2) '() #f)
                              (list (fact 'foo '(1))))
                     (rule (rule-frag 'foo '(1) '() #f)
                              '()))
               '()))

  (check-equal/unhygenic?
   (logic
     (foo "abc")
     ((bar #t 'a) is {1 2 #\c}))
   (program (list (rule (rule-frag 'foo '("abc") '() #f)
                              '()))
               (list (rule (rule-frag 'bar '(#t a) '(1 2 #\c) #f)
                              '()))))

  ;; we disallow binding relation variables on RHS of :-
  (check-exn
   #rx"not bound as rel-var"
   (lambda ()
     (convert-compile-time-error
      (logic ((foo X) :- ((bar) is X) (baz))))))
  
  (check-equal/unhygenic?
   (logic
     (bar)
     ((foo X) :- ((bar) is X) (baz))
     (baz))
   (program
    (list (rule (rule-frag 'bar '() '() #f) '())
          (rule (rule-frag 'foo (list (variable 'X)) '() #f)
                   (list (fact 'bar '() (variable 'X))
                         (fact 'baz '())))
          (rule (rule-frag 'baz '() '() #f) '()))
    '()))

  ;; some error cases

  (check-exn
   #rx"cannot bind variables in conclusions"
   (lambda ()
     (convert-compile-time-error
      (logic (foo a)))))

  (check-exn
   #rx"1 argument\\(s\\) but got 2"
   (lambda ()
     (convert-compile-time-error
      (logic
        (foo 1)
        (foo 1 2)))))

  (check-exn
   ; this error message isn't good, but it's from syntax-spec
   #rx""
   (lambda ()
     (convert-compile-time-error
      (logic (is (bar 10))))))

  (check-exn
   #rx"use of reserved name"
   (lambda ()
     (convert-compile-time-error
      (logic (is 10)))))

  ;; larger examples

  (check-equal/unhygenic?
   (logic
     (decls (parent 'alice 'bob)
            (decls (parent 'bob 'carol))
            (decls))

     (decls ((ancestor X Y) :- (parent X Y)))
     ((ancestor X Y) :- (parent X Z) (ancestor Z Y)))
   (program
    (list (rule (rule-frag 'parent '(alice bob) '() #f) '())
          (rule (rule-frag 'parent '(bob carol) '() #f) '())
          (rule (rule-frag 'ancestor
                                 (list (variable 'X) (variable 'Y))
                                 '()
                                 #f)
                   (list
                    (fact 'parent
                             (list (variable 'X) (variable 'Y)) )))
          (rule (rule-frag 'ancestor
                                 (list (variable 'X) (variable 'Y)) '()
                                 #f)
                   (list
                    (fact 'parent
                             (list (variable 'X) (variable 'Z)))
                    (fact 'ancestor
                             (list (variable 'Z) (variable 'Y))))))
    '()))

  (check-equal/unhygenic?
   (logic
     ;; these are unbound relation names, because this is a placeholder example
     ;; this is a way to declare that
     ((region R) :- (region R))
     ((adjacent R S) :- (adjacent R S))
     
     (((terrain R) is {'mountain 'forest 'ocean}) :- (region R))
     (((terrain R) is {'forest 'ocean})
      :-
      (adjacent R S) ((terrain S) is 'ocean)))
   (program
    (list
     (rule
      (rule-frag 'region (list (variable 'R)) '() #f)
      (list (fact 'region (list (variable 'R)))))
     (rule
      (rule-frag 'adjacent (list (variable 'R) (variable 'S)) '() #f)
      (list (fact 'adjacent (list (variable 'R) (variable 'S))))))
    (list
     (rule
      (rule-frag 'terrain
                    (list (variable 'R))
                    '(mountain forest ocean) #f)
      (list (fact 'region (list (variable 'R)))))
     (rule
      (rule-frag 'terrain (list (variable 'R)) '(forest ocean) #f)
      (list
       (fact 'adjacent (list (variable 'R) (variable 'S)))
       (fact 'terrain (list (variable 'S)) 'ocean))))))

  ;; importing tests

  (check-equal/unhygenic?
   (logic #:import ([a add1])
     ((foo) :- ((a 0) is 1)))
   (program
    (list (rule (rule-frag 'foo '() '() #f)
                   (list (fact add1 '(0) 1))))
    '()))

  (check-equal/unhygenic?
   (logic #:import ([p +])
     ((foo) :- ((p 1 2) is 3))
     ((bar X) :- ((p 1 2 3) is X)))
   (program
    (list (rule (rule-frag 'foo '() '() #f)
                   (list (fact + '(1 2) 3)))
          (rule (rule-frag 'bar (list (variable 'X)) '() #f)
                   (list (fact + '(1 2 3) (variable 'X)))))
    '()))
  
  (check-equal/unhygenic?
   (logic #:import [add1]
     ((foo) :- ((add1 0) is 1)))
   (program
    (list (rule (rule-frag 'foo '() '() #f)
                   (list (fact add1 '(0) 1))))
    '()))

  (check-equal/unhygenic?
   (logic #:import ([add1 add1])
     (foo 1)
     ((bar) :- (foo X) ((add1 X) is 2)))
   (program
    (list (rule (rule-frag 'foo '(1) '() #f) '())
          (rule (rule-frag 'bar '() '() #f)
                   (list (fact 'foo (list (variable 'X)))
                         (fact add1 (list (variable 'X)) 2))))
    '()))

  ;; errors with imports
  (check-exn
   #rx"imported relations cannot appear in conclusions"
   (lambda ()
     (convert-compile-time-error (logic #:import [add1]
                                   (add1 0)))))

  (check-exn
   #rx"imported relations cannot appear in conclusions"
   (lambda ()
     (convert-compile-time-error (logic #:import [add1]
                                   ((add1 0) is {1})))))

  (check-exn
   #rx"imported relations must be used with 'is'"
   (lambda ()
     (convert-compile-time-error (logic #:import [add1]
                                   ((foo) :- (add1 0))))))

  (check-exn
   #rx"identifier already defined"
   (lambda ()
     (convert-compile-time-error (logic #:import ([x add1] [x sub1])))))
  
  (check-exn
   #rx"cannot run imported relations backwards"
   (lambda ()
     (convert-compile-time-error
      (logic #:import [add1]
        ((foo X) :- ((add1 X) is 2))))))

  ;; is? rules
  (check-equal/unhygenic?
   (logic
     (((a) is? {1}) :- ((a) is 1))
     ((a) is {1})
     ((a) is? {2 3}))
   (program
    (list (rule (rule-frag 'a '() '(1) #f) '()))
    (list
     (rule (rule-frag 'a '() '(1) #t) (list (fact 'a '() 1)))
     (rule (rule-frag 'a '() '(2 3) #t) '()))))

  ;; is? errors
  (check-exn
   #rx"expected attr" ;; FIXME: this is bad
   (lambda ()
     (convert-compile-time-error
      (logic
        ((foo) :- ((foo) is? 1))))))

  (check-equal/unhygenic?
   (logic
     (decls ((a) is? {#t})))
   (program
    '()
    (list (rule (rule-frag 'a '() '(#t) #t) '()))))
  )