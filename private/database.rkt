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
(define (db-cons f db)
  (database (set-add (database-facts db) f)
            (database-constraints db)))

;; db-has? : Fact Database -> Boolean
;; Determine if the database contains the given fact.
(define (db-has? f db) (set-member? (database-facts db) f))

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

;; consistent? : Database Fact -> Boolean
;; Determine if the given fact is consistent with the database of already
;; known facts.
(define (consistent? db f)
  ;; fact-consistent? : Fact -> Boolean
  ;; Determine if the known fact is consistent with the closed over fact f.
  ;; Two facts are consistent if they do not map the same attribute to
  ;; different values.
  (define (fact-consistent? known)
    (not (and (equal? (fact-rel f) (fact-rel known))
              (equal? (fact-terms f) (fact-terms known))
              (not (equal? (fact-value f) (fact-value known))))))

  (db-andmap fact-consistent? db))
