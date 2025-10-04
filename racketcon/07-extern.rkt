#lang racket

(require "racketcon-common.rkt"
         (for-space minidusa "racketcon-common.rkt"))

(struct participant [name DOB])

;; fetch-participants : -> [SetOf Participant]
(define (fetch-participants)
  (list (participant "Joe" "01/28/1995")
        (participant "Zack" "07/26/2004")
        (participant "Ryan" "06/20/2004")
        (participant "Ari" "01/01/1995")))

;; participants->factset : [SetOf Participant] Symbol -> [SetOf Fact]
(define (participants->factset ps name)
  (for/set ([p ps])
    (fact name (list (participant-name p)) (participant-DOB p))))

;; same-year? : DateString DateString -> Bool
(define (same-year? dob1 dob2)
  (define (extract-year dob)
    (->year (parse-date dob "M/d/yyyy")))
  (equal? (extract-year dob1) (extract-year dob2)))

(soln->factset
 (stream-first
  (solve #:facts (participants->factset (fetch-participants) 'person)
    (logic #:import ([year=? same-year?]) #:extern (person)
      ((edge X Y) :- ((person X) is DOB1)
                     ((person Y) is DOB2)
                     ((year=? DOB1 DOB2) is #t))
      
      (((room X) is {'room1 'room2 'room3}) :- (edge X _))
      (forbid ((room X) is R)
              ((room Y) is R)
              (edge X Y))))))
