#lang racket

(provide compile-logic)

;; there are a few ways to set this up; this is from PEG class example
(require (for-syntax syntax/parse)
         #;(prefix-in rt: "runtime.rkt"))

;; this is the version that uses a compile-time function rather than a macro
#;(require (for-template
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

(define-syntax compile-logic
  (syntax-parser
    ;; we actually have to create a logic that splits out into two lists
    ;; of rules, which may be better fit for a compile-time function...
    ;; though checking which is which is still a little awkward,
    ;; unless fewer helpers are used
    ;; or, compile-time is-choose function, then partition with it
    [(_ d ...) #''(d ...)]))

;; this is the old compile-time function
;; LogicSyntax -> RacketSyntax
#;(define (compile-logic logic-stx)
  (syntax-parse logic-stx
    [(logic d ...) #''(d ...)]))