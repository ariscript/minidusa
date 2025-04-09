#lang racket

(module+ test
  (require rackunit
           "../main.rkt")

  (check-equal?
   (stream->list (all (logic (foo 1))))
   ;; one solution consisting of one fact
   (list (solution (db-of (fact 'foo '(1))))))

  (check-equal?
   (stream->list (all (logic
                        ((foo 2) :- (foo 1))
                        (foo 1))))
   ;; TODO: use `solution` and override equal? to make this insensitive to
   ;; the order in which facts are deduced
   (list (solution (db-of (fact 'foo '(2)) (fact 'foo '(1))))))

  ;; order of the rules in the program does not matter
  (check-equal?
   (stream->list (all (logic
                        (foo 1)
                        ((foo 2) :- (foo 1)))))
   (list (solution (db-of (fact 'foo '(2)) (fact 'foo '(1))))))

  ;; TODO: make this test less brittle
  (check-equal?
   (stream->list (all (logic
                        (foo 1)
                        ((bar X) :- (foo X))
                        ((foo 2) :- (foo 1)))))
   (list (solution (db-of (fact 'bar '(2))
                          (fact 'foo '(2))
                          (fact 'bar '(1))
                          (fact 'foo '(1))))))

  (check-equal?
   (stream->list (all (logic
                        ((foo 1) is {'a}))))
   (list (solution (db-of (fact 'foo '(1) 'a)))))

  (check-equal?
   (stream->list (all (logic
                        ((foo 1) is {'a})
                        (((foo 2) is {'b}) :- ((foo 1) is 'a)))))
   (list (solution (db-of (fact 'foo '(2) 'b)
                          (fact 'foo '(1) 'a)))))

  (check-equal?
   (stream->list (all (logic
                        ((foo 1) is {'a})
                        (((bar 2) is {X}) :- ((foo 1) is X)))))
   (list (solution (db-of (fact 'bar '(2) 'a)
                          (fact 'foo '(1) 'a)))))

  (check-equal?
   (stream->list (all (logic
                        (foo 1)
                        (foo 2)
                        (((bar) is {X}) :- (foo X)))))
   '())

  (check-equal?
   (stream->list (all (logic
                        ((foo) is {'a})
                        ((foo) is {'b}))))
   '())

  (check-equal?
   (stream->list (all (logic
                        ((foo) is {'a 'b}))))
   (list (solution (db-of (fact 'foo '() 'a)))
         (solution (db-of (fact 'foo '() 'b)))))

  (check-equal?
   (stream->list (all (logic
                        ((foo) is {'a 'b})
                        ((foo) is {'b 'c}))))
   (list (solution (db-of (fact 'foo '() 'b)))))

  (check-equal?
   (length (stream->list
            (all (logic
                   (edge 'a 'b)
                   (edge 'b 'c)
                   ((edge X Y) :- (edge Y X))
                   ((node X) :- (edge X _))
                   (((color X) is {1 2 3}) :- (node X))))))
   27)

  (check-equal?
   (length (stream->list (all (logic
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

  (check-equal?
   (stream->list (all ancestor-prog))
   (list
    (solution (db-of
               (fact 'ancestor '(alice dianne))
               (fact 'ancestor '(alice carol))
               (fact 'ancestor '(alice bob))
               (fact 'ancestor '(bob carol))
               (fact 'ancestor '(bob dianne))
               (fact 'ancestor '(alice ethan))
               (fact 'parent '(alice ethan))
               (fact 'parent '(bob dianne))
               (fact 'parent '(bob carol))
               (fact 'parent '(alice bob))))))

  ;; running built-ins

  (check-equal?
   (stream->list
    (all
     (logic #:import ([p +])
                      ((foo) :- ((p 1 2) is 3))
                      ((bar X) :- ((p 1 2 3) is X)))))
   (list (solution (db-of
                    (fact 'foo '())
                    (fact 'bar '(6))))))

  (check-equal?
   (length (stream->list (all
                          (logic #:import
                           ([s add1])
                           ((run 0) is {'stop 'go})
                           (((run M) is {'stop 'go})
                            :- ((run N) is 'go) ((s N) is M))

                           ; forbid desugaring
                           ((ok) is {#t})
                           (((ok) is {#f}) :- ((run 10) is 'go))))))
   ; 11 solutions because 0-10 inclusive
   11)

  (check-equal?
   (stream->list (all (logic #:import [add1]
                                       (foo 1)
                                       ((bar) :- (foo X) ((add1 X) is 2)))))
   (list (solution (db-of (fact 'foo '(1))
                          (fact 'bar '()))))))
