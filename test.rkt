#lang racket

(require rackunit
         racket/lazy-require
         (prefix-in rt: "runtime.rkt")
         "spec.rkt")

(check-equal?
 (stream->list (rt:all (logic (foo 1))))
 ;; one solution consisting of one fact
 (list (list (rt:fact 'foo '(1)))))

(check-equal?
 (stream->list (rt:all (logic
                        ((foo 2) :- (foo 1))
                        (foo 1))))
 ;; TODO: use `solution` and override equal? to make this insensitive to
 ;; the order in which facts are deduced
 (list (list (rt:fact 'foo '(2)) (rt:fact 'foo '(1)))))

;; order of the rules in the program does not matter
(check-equal?
 (stream->list (rt:all (logic
                        (foo 1)
                        ((foo 2) :- (foo 1)))))
 (list (list (rt:fact 'foo '(2)) (rt:fact 'foo '(1)))))

;; TODO: make this test less brittle
(check-equal?
 (stream->list (rt:all (logic
                        (foo 1)
                        ((bar X) :- (foo X))
                        ((foo 2) :- (foo 1)))))
 (list (list (rt:fact 'bar '(2))
             (rt:fact 'foo '(2))
             (rt:fact 'bar '(1))
             (rt:fact 'foo '(1)))))

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
 (list
  (rt:fact 'ancestor '(alice dianne))
  (rt:fact 'ancestor '(alice carol))
  (rt:fact 'ancestor '(alice bob))
  (rt:fact 'ancestor '(bob carol))
  (rt:fact 'ancestor '(bob dianne))
  (rt:fact 'ancestor '(alice ethan))
  (rt:fact 'parent '(alice ethan))
  (rt:fact 'parent '(bob dianne))
  (rt:fact 'parent '(bob carol))
  (rt:fact 'parent '(alice bob)))))
