```bnf
;; a top-level finite-choice logic program consists of
;; facts and rules to make deductions
(logic <decl> ...+)

;; declarations are facts (conclusions) or rules
<decl> := (<conclusion>)
        | (<conclusion> :- <premise> ...+)

;; either a relational proposition (attribute) or a functional proposition
;; while conclusions can have variables, they must be bound by premises
<conclusion> := <attribute>
              | (is <attribute> (choice <logic-term>...+))

;; a premise to a rule, which will imply the conclusion if
;; satisfied, and may be either relational or functional
<premise> := <attribute>
           | (is <attribute> <logic-term>)

;; a proposition on zero or more terms, which should
;; always be used with a consistent amount of terms
<attribute> := (<symbol> <logic-term> ...)

;; basic data in the language, either primitive data or
;; a constructor/proposition applied to other data
<logic-term> := <ID>
              | <DATUM>
```