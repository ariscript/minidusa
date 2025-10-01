#lang racket

(module+ test
  (require "../testing.rkt"
           syntax-spec-v3
           (for-syntax syntax/parse))

  (check-all-solutions
   (logic (foo 1))
   (list (set (fact 'foo '(1)))))

  (check-all-solutions
   (logic
     ((foo 2) :- (foo 1))
     (foo 1))
   ;; TODO: use `solution` and override equal? to make this insensitive to
   ;; the order in which facts are deduced
   (list (set (fact 'foo '(2)) (fact 'foo '(1)))))

  ;; order of the rules in the program does not matter
  (check-all-solutions
   (logic
     (foo 1)
     ((foo 2) :- (foo 1)))
   (list (set (fact 'foo '(2))
              (fact 'foo '(1)))))

  (check-all-solutions
   (logic
     (foo 1)
     ((bar X) :- (foo X))
     ((foo 2) :- (foo 1)))
   (list (set (fact 'bar '(2))
              (fact 'foo '(2))
              (fact 'bar '(1))
              (fact 'foo '(1)))))

  (check-all-solutions
   (logic
     ((foo 1) is {'a}))
   (list (set (fact 'foo '(1) 'a))))

  (check-all-solutions
   (logic
     ((foo 1) is {'a})
     (((foo 2) is {'b}) :- ((foo 1) is 'a)))
   (list (set (fact 'foo '(2) 'b)
              (fact 'foo '(1) 'a))))

  (check-all-solutions
   (logic
     ((foo 1) is {'a})
     (((bar 2) is {X}) :- ((foo 1) is X)))
   (list (set (fact 'bar '(2) 'a)
              (fact 'foo '(1) 'a))))

  (check-all-solutions
   (logic
     (foo 1)
     (foo 2)
     (((bar) is {X}) :- (foo X)))
   '())

  (check-all-solutions
   (logic
     ((foo) is {'a})
     ((foo) is {'b}))
   '())

  (check-all-solutions
   (logic ((foo) is {'a 'b}))
   (list (set (fact 'foo '() 'a))
         (set (fact 'foo '() 'b))))

  (check-all-solutions
   (logic
     ((foo) is {'a 'b})
     ((foo) is {'b 'c}))
   (list (set (fact 'foo '() 'b))))

  ;; regression test: order of choices should not matter
  (check-all-solutions
   (logic
     ((foo) is {'a 'b})
     ((foo) is {'c 'b}))
   (list (set (fact 'foo '() 'b))))

  (check-equal?
   (length (stream->list
            (solve (logic
                     (edge 'a 'b)
                     (edge 'b 'c)
                     ((edge X Y) :- (edge Y X))
                     ((node X) :- (edge X _))
                     (((color X) is {1 2 3}) :- (node X))))))
   27)

  (check-equal?
   (length (stream->list (solve (logic
                                  (edge 'a 'b)
                                  (edge 'b 'c)
                                  ((edge X Y) :- (edge Y X))
                                  ((node X) :- (edge X _))
                                  (((color X) is {1 2 3}) :- (node X))

                                  ((ok) is {#t})
                                  (((ok) is {#f}) :- (edge X Y)
                                                  ((color X) is C)
                                                  ((color Y) is C))))))
   12)

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

  ;; - Alice
  ;;   - Bob
  ;;     - Carol
  ;;     - Dianne
  ;;   - Ethan

  (define ancestor-prog
    (logic
      (parent 'alice 'bob)
      (parent 'bob 'carol)
      (parent 'bob 'dianne)
      (parent 'alice 'ethan)

      ((ancestor X Y) :- (parent X Y))
      ((ancestor X Y) :- (parent X Z) (ancestor Z Y))))

  (check-all-solutions
   ancestor-prog
   (list
    (set
     (fact 'ancestor '(alice dianne))
     (fact 'ancestor '(alice carol))
     (fact 'ancestor '(alice bob))
     (fact 'ancestor '(bob carol))
     (fact 'ancestor '(bob dianne))
     (fact 'ancestor '(alice ethan))
     (fact 'parent '(alice ethan))
     (fact 'parent '(bob dianne))
     (fact 'parent '(bob carol))
     (fact 'parent '(alice bob)))))

  ;; running built-ins

  (check-all-solutions
   (logic #:import ([p +])
     ((foo) :- ((p 1 2) is 3))
     ((bar X) :- ((p 1 2 3) is X)))
   (list (set (fact 'foo '())
              (fact 'bar '(6)))))

  (check-equal?
   (length (stream->list (solve
                          (logic #:import ([s add1])
                            ((run 0) is {'stop 'go})
                            (((run M) is {'stop 'go})
                             :- ((run N) is 'go) ((s N) is M))

                            ; forbid desugaring
                            ((ok) is {#t})
                            (((ok) is {#f}) :- ((run 10) is 'go))))))
   ; 11 solutions because 0-10 inclusive
   11)

  (check-all-solutions
   (logic #:import ([add1 add1])
     (foo 1)
     ((bar) :- (foo X) ((add1 X) is 2)))
   (list (set (fact 'foo '(1))
              (fact 'bar '()))))

  (check-all-solutions
   (logic #:import ([+ string-append]
                    [= equal?]
                    [string-length string-length])
     (pre-string "a")
     (pre-string "cd")
     (string "123")
     ((string X) :- (pre-string A)
                 (pre-string B)
                 ((= A B) is #f)
                 ((+ A B) is X))
     ((same-len X Y) :- (string X)
                     (string Y)
                     ((string-length X) is M)
                     ((string-length Y) is N)
                     ((= M N) is #t)))
   (list (set (fact 'pre-string '("cd"))
              (fact 'pre-string '("a"))
              (fact 'string '("cda"))
              (fact 'string '("acd"))
              (fact 'string '("123"))
              (fact 'same-len '("123" "123"))
              (fact 'same-len '("123" "acd"))
              (fact 'same-len '("123" "cda"))
              (fact 'same-len '("acd" "123"))
              (fact 'same-len '("acd" "acd"))
              (fact 'same-len '("acd" "cda"))
              (fact 'same-len '("cda" "123"))
              (fact 'same-len '("cda" "acd"))
              (fact 'same-len '("cda" "cda")))))

  (check-equal?
   (length (stream->list
            (solve (logic
                     ((a 1 2 3) is {#t #f})
                     ((b) is {#t #f})
                     ((b) is {#t #f})
                     ((c) is {#t #f})
                     (bar 1) ; maybe this should be disallowed...
                     ((bar 2) is {#t #t})))))
   16)

  (check-all-solutions
   (logic
     ((a) is? {1}))
   (list (set (fact 'a '() 1))))

  (check-all-solutions
   (logic
     ((a) is? {1 2})
     ((b) :- ((a) is _))
     (((a) is? {3}) :- (b)))
   (list (set (fact 'a '() 1)
              (fact 'b '()))
         (set (fact 'a '() 2)
              (fact 'b '()))))

  ;; TODO: make sure that this plays well with hygiene
  (define extern-prog
    (logic #:extern (foo bar)
      (foo 1)
      ((bar 2) :- (foo 2))))
  
  (check-equal?
   (soln->factset (stream-first (solve extern-prog)))
   (set (fact 'foo '(1))))

  (check-equal?
   (soln->factset
    (stream-first (solve #:facts (set (fact 'foo '(2))) extern-prog)))
   (set (fact 'foo '(1))
        (fact 'foo '(2))
        (fact 'bar '(2))))

  (define-dsl-syntax forbid logic-macro
    (lambda (stx)
      (syntax-parse stx
        [(_  p ...+)
         #'(decls ((name) is {#t})
                  (((name) is {#f}) :- p ...))])))

  (define-dsl-syntax demand logic-macro
    (lambda (stx)
      (syntax-parse stx
        [(_ p ...+)
         #'(decls ((d) is? {#f})
                  (((d) is {#t}) :- p ...)
                  (forbid ((d) is #f)))])))

  (define (adjacent x1 y1 x2 y2)
    (or (and (= (abs (- x1 x2)) 1) (= y1 y2))
             (and (= (abs (- y1 y2)) 1) (= x1 x2))))

  #;(stream-first
   (solve
    (logic #:import (add1 < adjacent cons) #:extern (images)
      ((grid 0 0) is {'city 'plain 'mountain 'forest 'ocean})
      (((grid n sm) is {'city 'plain 'mountain 'forest 'ocean})
       :-
       ((grid n m) is _)
       ((add1 m) is sm)
       ((< n 5) is #t)
       ((< m 4) is #t))
      (((grid sn m) is {'city 'plain 'mountain 'forest 'ocean})
       :-
       ((grid n m) is _)
       ((add1 n) is sn)
       ((< n 4) is #t)
       ((< n 5) is #t))
    
      ; no two cities should be directly adjacent
      (forbid ((grid x1 y1) is 'city)
              ((grid x2 y2) is 'city)
              ((adjacent x1 x2 y1 y2) is #t))
      (demand ((grid x y) is 'city)) ; require at least one city

      ; a tile is connected if there's a path to the ocean
      ; all cities should be connected to the ocean
      (((connected n m) is {#t}) :- ((grid n m) is 'ocean))
      (((connected n m) is? {#t}) :-
                                  ((grid x y) is _a)
                                  ((grid n m) is _b)
                                  ((adjacent n m x y) is #t)
                                  ((connected x y) is #t))
      (((connected n m) is {#f}) :- ((grid n m) is 'mountain))
      (demand ((grid x y) is 'city) ((connected x y) is #t))

      (((spawn) is? {x}) :- ((grid n m) is 'city) ((cons n m) is x))

      ; set up map from grid coordinates to image
      (((image n m) is {img}) :-
                              ((grid n m) is biome)
                              ((images biome) is img)))))
  )
