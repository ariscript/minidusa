#lang racket

(require racket/set)

(provide (except-out (all-defined-out)
                     (struct-out database)))

(require "data.rkt")

;; A Database is a (database [SetOf Fact] [SetOf Constraint])
(struct database [facts constraints] #:transparent)

;; to support changing Database to something faster (like a Trie), we
;; use the following abstractions abstract:

;; db-of : Fact ... -> Database
(define (db-of . facts)
  (database (apply set facts) (set)))

;; db-empty? : Database -> Boolean
;; Returns #f if the database contains facts.
(define (db-empty? db)
  (set-empty? (database-facts db)))

;; db-first : Database -> Fact
;; A Database is unordered, and so this function will return some arbitrary,
;; but consistent, fact: not necessarily the "first" one.
(define (db-first db)
  (set-first (database-facts db)))

;; db-rest : Database -> Database
;; Return the given database with `(db-first db)` removed.
(define (db-rest db)
  (database (set-rest (database-facts db)) (database-constraints db)))

;; db-cons : Fact Database -> Database
;; The given fact _must_ be consistent with the database when called.
(define (db-cons f db)
  (database (set-add (database-facts db) f)
            ; once we have an actual value, we no longer need the constraints
            ; this is important for later positivity checks
            (for/set ([c (database-constraints db)]
                      #:when
                      (not (and (equal? (fact-rel f) (constraint-rel c))
                                (equal? (fact-terms f) (constraint-terms c)))))
              c)))

;; db-add-constraint : Constraint Database -> Database
;; Adds a constraint to the database, if it does not already contain that attr.
(define (db-add-constraint c db)
  (database (database-facts db)
            (if (db-has-attr? (constraint-rel c) (constraint-terms c) db)
                (database-constraints db)
                (set-add (database-constraints db) c))))

;; db-positive? : Database -> Boolean
;; Determine if the database is _positive_: it has no constraints.
;; See Definition 4.7 in the Dusa paper.
(define (db-positive? db)
  ; we will never add `none-of: '()`.
  (set-empty? (database-constraints db)))

;; db-has? : Fact Database -> Boolean
;; Determine if the database contains the given fact.
(define (db-has? f db) (set-member? (database-facts db) f))

;; db-has-attr? : Symbol [ListOf Datum] Database -> Boolean
;; Determine if the database contains a fact that has the same
;; attribute as rel and terms.
(define (db-has-attr? rel terms db)
  ;; attr-like? : Fact -> Boolean
  ;; Determine if the given fact has the same attribute as
  ;; rel and terms.
  (define (attr-like? f)
    (and (equal? (fact-rel f) rel)
              (equal? (fact-terms f) terms)))
  (not (db-empty? (db-filter attr-like? db))))

;; db-filter : [Fact -> Boolean] Database -> Database
(define (db-filter pred? db)
  ; why does racket not have this?
  (database
   (for/set ([fact (database-facts db)]
             #:when (pred? fact))
     fact)
   (database-constraints db)))

;; db-andmap : [Fact -> Boolean] Database -> Boolean
(define (db-andmap pred? db)
  ; see comment above
  (for/and ([fact (database-facts db)]) (pred? fact)))

;; consistent? : Database [OneOf Fact Constraint] -> Boolean
;; Determine if the given fact is consistent with the database of already
;; known facts and constraints.
(define (consistent? db to-add)
  ;; fact-consistent? : Fact -> Boolean
  ;; Determine if the known fact is consistent with the closed over fact f.
  ;; Two facts are consistent if they do not map the same attribute to
  ;; different values.
  ;; This should only be called when to-add is a Fact.
  (define (fact-consistent? known)
    (not (and (equal? (fact-rel to-add) (fact-rel known))
              (equal? (fact-terms to-add) (fact-terms known))
              (not (equal? (fact-value to-add) (fact-value known))))))

  ;; constraint-valid? : Constraint Fact -> Boolean
  ;; Determine if the fact is consistent given the constraint c.
  (define (constraint-valid? c f)
    (not (and (equal? (fact-rel f) (constraint-rel c))
              (equal? (fact-terms f) (constraint-terms c))
              (set-member? (constraint-none-of c) (fact-value f)))))

  (if (constraint? to-add)
      (for/and ([fact (database-facts db)])
        (constraint-valid? to-add fact))
      (and (for/and ([fact (database-facts db)])
             (fact-consistent? fact))
           (for/and ([c (database-constraints db)])
             (constraint-valid? c to-add)))))
