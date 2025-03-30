#lang racket

(require rackunit
         racket/lazy-require
         (prefix-in rt: "runtime.rkt")
         "spec.rkt")

(check-equal?
 (stream->list (rt:all (logic (foo 1))))
 ;; one solution consisting of one fact
 (list (rt:solution (set (rt:fact 'foo '(1))))))

(check-equal?
 (stream->list (rt:all (logic
                        ((foo 2) :- (foo 1))
                        (foo 1))))
 ;; TODO: use `solution` and override equal? to make this insensitive to
 ;; the order in which facts are deduced
 (list (rt:solution (set (rt:fact 'foo '(2)) (rt:fact 'foo '(1))))))

;; order of the rules in the program does not matter
(check-equal?
 (stream->list (rt:all (logic
                        (foo 1)
                        ((foo 2) :- (foo 1)))))
 (list (rt:solution (set (rt:fact 'foo '(2)) (rt:fact 'foo '(1))))))

;; TODO: make this test less brittle
(check-equal?
 (stream->list (rt:all (logic
                        (foo 1)
                        ((bar X) :- (foo X))
                        ((foo 2) :- (foo 1)))))
 (list (rt:solution (set (rt:fact 'bar '(2))
                      (rt:fact 'foo '(2))
                      (rt:fact 'bar '(1))
                      (rt:fact 'foo '(1))))))

(check-equal?
 (stream->list (rt:all (logic
                        (is (foo 1) (choice 'a)))))
 (list (rt:solution (set (rt:fact 'foo '(1) 'a)))))

(check-equal?
 (stream->list (rt:all (logic
                        (is (foo 1) (choice 'a))
                        ((is (foo 2) (choice 'b)) :- (is (foo 1) 'a)))))
 (list (rt:solution (set (rt:fact 'foo '(2) 'b)
                      (rt:fact 'foo '(1) 'a)))))

(check-equal?
 (stream->list (rt:all (logic
                        (is (foo 1) (choice 'a))
                        ((is (bar 2) (choice X)) :- (is (foo 1) X)))))
 (list (rt:solution (set (rt:fact 'bar '(2) 'a)
                      (rt:fact 'foo '(1) 'a)))))

(check-equal?
 (stream->list (rt:all (logic
                        (foo 1)
                        (foo 2)
                        ((is (bar) (choice X)) :- (foo X)))))
 '())

(check-equal?
 (stream->list (rt:all (logic
                        (is (foo) (choice 'a))
                        (is (foo) (choice 'b)))))
 '())

(check-equal?
 (stream->list (rt:all (logic
                        (is (foo) (choice 'a 'b)))))
 (list (rt:solution (set (rt:fact 'foo '() 'a)))
       (rt:solution (set (rt:fact 'foo '() 'b)))))

(check-equal?
   (stream->list (rt:all (logic
                          (is (foo) (choice 'a 'b))
                          (is (foo) (choice 'b 'c)))))
   (list (rt:solution (set (rt:fact 'foo '() 'b)))))

(check-equal?
 (length (stream->list (rt:all (logic
                                (edge 'a 'b)
                                (edge 'b 'c)
                                ((edge X Y) :- (edge Y X))
                                ((node X) :- (edge X _))
                                ((is (color X) (choice 1 2 3)) :- (node X))))))
 27)

(check-equal?
 (length (stream->list (rt:all (logic
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
 (length (stream->list (rt:all (logic
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
 (stream->list (rt:all ancestor-prog))
 (list
  (rt:solution (set
                (rt:fact 'ancestor '(alice dianne))
                (rt:fact 'ancestor '(alice carol))
                (rt:fact 'ancestor '(alice bob))
                (rt:fact 'ancestor '(bob carol))
                (rt:fact 'ancestor '(bob dianne))
                (rt:fact 'ancestor '(alice ethan))
                (rt:fact 'parent '(alice ethan))
                (rt:fact 'parent '(bob dianne))
                (rt:fact 'parent '(bob carol))
                (rt:fact 'parent '(alice bob))))))
