```bnf
;; a top-level finite-choice logic program consists of
;; facts and rules to make deductions
<logic> ::= (logic <decl> ...)
          | (logic #:import [<imp> ...] <decl> ...)

;; declarations are facts (conclusions) or rules
<decl> ::= <conclusion>                       ; fact
         | (<conclusion> :- <premise> ...+)   ; rule
         | (decls <decl> ...)                 ; nested (for macros)

;; either a relational proposition (attribute) or a functional proposition
;; while conclusions can have variables, they must be bound by premises
<conclusion> ::= <attr>
               | (<attr> is {<logic-term> ...+})

;; a premise to a rule, which will imply the conclusion if
;; satisfied, and may be either relational or functional
<premise> ::= <attr>
            | (<attr> is <logic-term>)

;; a proposition on zero or more terms, which should
;; always be used with a consistent amount of terms
<attr> ::= (<ID> <logic-term> ...)

;; basic data in the language, either primitive data or
;; a constructor/proposition applied to other data
<logic-term> ::= <ID>
               | <DATUM>
```
