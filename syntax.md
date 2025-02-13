```bnf
;; a top-level FCLP consists of facts and rules to make deductions
(define-logic <declaration> ...+)

;; declarations are facts (conclusions) or rules
<declaration> := (<conclusion>)
               | (:- <conclusion> <premise> ...+)

;; either a relational propositon (attribute)
;; or a (closed or open) functional proposition
<conclusion> := <attribute>
              | (is <attribute> <choices>)
              | (is? <attribute> <choices>)

;; select one or more from a finite collection of choices
<choices> := (choice <term>...+)

;; a premise to a rule, which will imply the conclusion if
;; satisfied, and may be either relational or functional
<premise> := <attribute>
           | (is <attribute> <term>)

;; a proposition on zero or more terms, which should
;; always be used with a consistent amount of terms
<attribute> := <symbol>
             | (<symbol> <atomic-term>...+)

;; basic data in the language, either primitive data or
;; a constructor/proposition applied to other data
<term> := <atomic-term>
        | (<symbol> <atomic-term>...+)

<atomic-term> := <identifier>
               | <symbol>
               | <int>
               | <string>
               | (<term>)
```

NOTES:

- parens are messed up a bit
- not sure what constructors are
- maybe rename some things?
- atomic data: can we expand?
- Interaction like query vs solve vs etc
  - This should be present in the report, can mention in presentation
- To fit in, probably cut out term stuff
