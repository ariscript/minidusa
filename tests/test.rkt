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
                       (is (foo 1) (choice 'a)))))
   (list (solution (db-of (fact 'foo '(1) 'a)))))

  (check-equal?
   (stream->list (all (logic
                       (is (foo 1) (choice 'a))
                       ((is (foo 2) (choice 'b)) :- (is (foo 1) 'a)))))
   (list (solution (db-of (fact 'foo '(2) 'b)
                          (fact 'foo '(1) 'a)))))

  (check-equal?
   (stream->list (all (logic
                       (is (foo 1) (choice 'a))
                       ((is (bar 2) (choice X)) :- (is (foo 1) X)))))
   (list (solution (db-of (fact 'bar '(2) 'a)
                          (fact 'foo '(1) 'a)))))

  (check-equal?
   (stream->list (all (logic
                       (foo 1)
                       (foo 2)
                       ((is (bar) (choice X)) :- (foo X)))))
   '())

  (check-equal?
   (stream->list (all (logic
                       (is (foo) (choice 'a))
                       (is (foo) (choice 'b)))))
   '())

  (check-equal?
   (stream->list (all (logic
                       (is (foo) (choice 'a 'b)))))
   (list (solution (db-of (fact 'foo '() 'a)))
         (solution (db-of (fact 'foo '() 'b)))))

  (check-equal?
   (stream->list (all (logic
                       (is (foo) (choice 'a 'b))
                       (is (foo) (choice 'b 'c)))))
   (list (solution (db-of (fact 'foo '() 'b)))))

  (check-equal?
   (length (stream->list
            (all (logic
                  (edge 'a 'b)
                  (edge 'b 'c)
                  ((edge X Y) :- (edge Y X))
                  ((node X) :- (edge X _))
                  ((is (color X) (choice 1 2 3)) :- (node X))))))
   27)

  (check-equal?
   (length (stream->list (all (logic
                               (edge 'a 'b)
                               (edge 'b 'c)
                               ((edge X Y) :- (edge Y X))
                               ((node X) :- (edge X _))
                               ((is (color X) (choice 1 2 3)) :- (node X))

                               (is (ok) (choice #t))
                               ((is (ok) (choice #f)) :- (edge X Y)
                                                      (is (color X) C)
                                                      (is (color Y) C))))))
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
                               ((is (color X) (choice 1 2 3)) :- (node X))

                               (is (ok) (choice #t))
                               ((is (ok) (choice #f)) :- (edge X Y)
                                                      (is (color X) C)
                                                      (is (color Y) C))))))
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
     (logic/importing ([p +])
                      ((foo) :- (is (p 1 2) 3))
                      ((bar X) :- (is (p 1 2 3) X)))))
   (list (solution (db-of
                    (fact 'foo '())
                    (fact 'bar '(6))))))

  (check-equal?
   (length (stream->list (all
                          (logic/importing
                           ([s add1])
                           (is (run 0) (choice 'stop 'go))
                           ((is (run M) (choice 'stop 'go))
                            :- (is (run N) 'go) (is (s N) M))

                           ; forbid desugaring
                           (is (ok) (choice #t))
                           ((is (ok) (choice #f)) :- (is (run 10) 'go))))))
   ; 11 solutions because 0-10 inclusive
   11)

  (check-equal?
   (stream->list (all (logic/importing [add1]
                                       (foo 1)
                                       ((bar) :- (foo X) (is (add1 X) 2)))))
   (list (solution (db-of (fact 'foo '(1))
                          (fact 'bar '()))))))
