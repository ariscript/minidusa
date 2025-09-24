#lang racket/base

(require "main.rkt"
         syntax/macro-testing
         rackunit)

(provide (all-from-out "main.rkt"
                       rackunit
                       syntax/macro-testing)
         smush-syntax/program)

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

(define (smush-syntax/fact f)
  (define rel (fact-rel f))
  ;; can't struct-copy fact because it uses a smart constructor
  ;; and doesn't export the actual name itself
  (make-fact (if (procedure? rel) rel (syntax->datum rel))
             (fact-terms f)
             (fact-value f)))