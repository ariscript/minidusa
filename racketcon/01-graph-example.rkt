#lang racket

(require "racketcon-common.rkt"
         (for-space minidusa "racketcon-common.rkt"))

(define graph-example
  (logic
    (edge 'a 'b) (edge 'b 'c) (edge 'd 'e)
    ((edge X Y) :- (edge Y X))
    ((node X) :- (edge X _))

    ((reachable X Y) :- (edge X Y))
    ((reachable X Y) :- (edge Z Y) (reachable X Z))

    (((color X) is {'red 'green 'blue}) :- (node X))
    (forbid (edge X Y)
            ((color X) is C)
            ((color Y) is C))))

(define solution-stream (solve graph-example))

(length (stream->list solution-stream))

(all-facts-for (stream-first solution-stream) 'color)