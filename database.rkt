#lang racket

(require racket/set)

(provide (rename-out [set-empty? db-empty?]
                     [set-first db-first]
                     [set-rest db-rest]
                     [set-empty? db-empty?]
                     [set db-of])
         (all-defined-out))

;; A Database is a [SetOf Fact]

;; to support changing Database to something faster (like a Trie), we
;; use the following abstractions abstract:

;; db-cons : Fact Database -> Database
(define (db-cons f db) (set-add db f))

;; db-has? : Fact Database -> Boolean
;; Determine if the database contains the given fact.
(define (db-has? f db) (set-member? db f))

;; db-filter : [Fact -> Boolean] Database -> Database
(define (db-filter pred? db)
  ; why does racket not have this?
  (for/set ([fact db]
            #:when (pred? fact))
    fact))

;; db-andmap : [Fact -> Boolean] Database -> Boolean
(define (db-andmap pred? db)
  ; see comment above
  (for/and ([fact db]) (pred? fact)))