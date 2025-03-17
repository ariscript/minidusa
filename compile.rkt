#lang racket

(provide compile-logic)

;; there are a few ways to set this up; this is from PEG class example

(require (for-template
          racket/base
          (prefix-in rt: "runtime.rkt"))
         syntax/parse)

;; EXAMPLE EXPANSIONS!
#;(logic
   (foo 1))
;; =>
#;(rt:logic (list (rule (rule-frag 'foo '(1) '())
                        '()))
            '())


#;(logic
   ((foo 2) :- (foo 1))
   (foo 1))
;; =>
#;(rt:logic (list (rule (rule-frag 'foo '(2) '())
                        (list (fact 'foo '(1) (none))))
                  (rule (rule-frag 'foo '(1) '())
                        '()))
            '())

#;(logic
   (foo 1)
   (is (bar 10 11) (choice 1 2 3)))
;; =>
#;(rt:logic (list (rule (rule-frag 'foo '(1) '())
                        '()))
            (list (rule (rule-frag 'bar '(10 11) '(1 2 3))
                        '())))

;; this is the old compile-time function
;; LogicSyntax -> RacketSyntax
(define (compile-logic logic-stx)
  (syntax-parse logic-stx
    [(logic d ...) #''(d ...)]))