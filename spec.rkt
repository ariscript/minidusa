#lang racket

(require syntax-spec-v3
         (for-syntax syntax/parse))

;; see README for the grammar we are incrementally working towards

(syntax-spec
 (binding-class logic-nt)

 ;; (logic <decl> ...+)
 (host-interface/expression
   (logic d:decl ...+)
   ;; TODO: actually compile!
   #''(d ...))

 ;; <decl> ::= <conclusion>                       ; fact
 ;;          | (<conclusion> :- <premise> ...+)   ; rule
 (nonterminal decl
   (c:conclusion (~datum :-) p:premise ...+)
   #:binding (nest p ... c)
   
   c:conclusion)

 ;; <conclusion> ::= <attribute>
 ;;                | (is <attribute> (choice <logic-term> ...+))
 (nonterminal conclusion
   ;; it's important that this comes first, otherwise some things
   ;; are attempted to be parsed as logic-terms and explode
   ((~datum is) a:attribute ((~datum choice) t:logic-term ...+))
   #:binding (scope (import a) (import t) ...)
   
   a:attribute
   #:binding (scope (import a)))

 ;; <premise> ::= <attribute>
 ;;             | (is <attribute> <logic-term>)
 (nonterminal/nesting premise (nested)
   ((~datum is) a:attribute t:logic-term)
   #:binding (scope (import a) (import t) nested)
   
   a:attribute
   #:binding (scope (import a) nested))

 ;; <attribute> ::= (<ID>)
 ;;               | (<ID> <logic-term> ...+)
 (nonterminal/exporting attribute
   (name:id)
   (name:id t:logic-term ...+)
   #:binding [(re-export t) ...])

 ;; <logic-term> ::= (bind <ID>)
 ;;                | (ref <ID>)
 ;;                | <DATUM>
 (nonterminal/exporting logic-term
   (bind v:logic-nt)
   #:binding (export v)
   (ref v:logic-nt)
   ;; TODO: if this is something powerful (like expr), then syntax-spec
   ;; starts thinking that too many things are logic-terms
   ;; is there `racket-datum` or similar? what about symbols?
   ;; it would be nice to allow some expressions, though...
   ;; another option to avoid confusion would be to require symbols throughout
   n:number
   b:boolean
   ;; s:string ;; if this gets uncommented, then (bar 10) parses as logic-term
   c:char)

 )

;; some examples: this (surprisingly) actually works!
(logic
 (foo 1)
 ;; this is actually parsing as a logic-term (foo o4)
 ((foo 0) :- (foo 1))
 ((foo 2) :- (foo 0) (foo 1))

 ;; correctly fails to parse, even if message isn't great:
 ;; (is (bar 10))

 ;; not sure if this is actually good syntax...
 (is (bar 10) (choice 1 2 3))
 ((is (bar 20) (choice 4 5 6)) :- (foo 0) (is (foo 1) 45))

 ;; (baz (bind x))
 ;; this parses but shouldn't scope check.
 ;;
 ;; conclusions should not be able to bind, but must be able to ref
 ;; OPTION 1 = whatever expands to `bind` only does so in some places
 ;; OPTION 2 = more nonterminals to enforce this syntactically
 ;;   (I don't think that Michael's hack would work for either of these)
 ;; OPTION 3 = enforce this later as a static check when compiling
 ;; OPTION 1.5 = make atomic-term just use logic-nt, which we understand
 ;; to always be a ref. this actually DOES work with the hack above
 
 ((baz (ref x)) :- (baz (bind x)) (qux (ref x)))  ; IDE support :)
 )

;; EXAMPLE EXPANSION!

