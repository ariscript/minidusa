#lang racket/base

(require "main.rkt"
         syntax/macro-testing
         rackunit
         racket/stream
         racket/set)

(provide (all-from-out "main.rkt"
                       rackunit
                       syntax/macro-testing
                       racket/stream
                       racket/set)
         smush-syntax/program
         check-all-solutions)

;; replaces all of the relation syntax objects in the program with symbols
;; by walking the program AST and syntax->datum on relation names
(define (smush-syntax/program prog)
  (program (map smush-syntax/rule (program-deduce-rules prog))
           (map smush-syntax/rule (program-choose-rules prog))))

(define (smush-syntax/rule r)
  (rule (smush-syntax/rule-frag (rule-conclusion r))
        (map smush-syntax/fact (rule-premises r))))

(define (smush-syntax/rule-frag rf)
  (struct-copy rule-frag rf [name (syntax->datum (rule-frag-name rf))]))

;; check-all-solutions: Program [ListOf [SetOf Fact]] -> Void
(define-syntax-rule (check-all-solutions prog expected)
  (check-equal? (map soln->factset (stream->list (solve prog)))
                expected))