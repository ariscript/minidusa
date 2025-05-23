#lang info

(define collection "minidusa")
(define deps '("base" "syntax-spec-v3"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/minidusa.scrbl"
                       ()
                       (experimental)
                       "minidusa")))
(define pkg-desc "Finite-choice logic DSL in Racket, inspired by Dusa")
(define version "0.0")
(define pkg-authors '(ari))
(define license '(AGPL-3.0-only))
(define test-include-paths '("private/" "tests/"))
(define test-omit-paths '(#rx"compiled/" #rx"\\.rkt~" #rx"README" #rx".DS_Store"))
