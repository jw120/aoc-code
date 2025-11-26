#lang racket

;; Day 1 - Advent of Code 2018

; Part a: Sum of the frequency changes
(define (part-a xs)
  (apply + xs))

(module+ test
  (require rackunit)
  (check-equal? (part-a '(1 1 1)) 3)
  (check-equal? (part-a '(1 1 -2)) 0)
  (check-equal? (part-a '(-1 -2 -3)) -6))

; Part b: First repeat of sums of the series (repeated)
; Iterates over the cyclically repeating frequencies while accumulating the running sum
; and a set of frequencies already seen. Breaks loop when the sum repeats a previous value.
; let-values is used to capture the multiple return values from the for/fold
(define (part-b xs)
  (let-values ([(s-final _seen-final)
                (for/fold ([s 0]
                          [seen (set)])
                         ([i (in-cycle xs)]
                          #:break (set-member? seen s))
                 (values (+ s i) (set-add seen s)))])
    s-final))

(module+ test
  (check-equal? (part-b '(1 -1 1)) 0)
  (check-equal? (part-b '(3 3 4 -2 -4)) 10)
  (check-equal? (part-b '(-6 3 8 5 -6)) 5)
  (check-equal? (part-b '(7 7 -2 -7 -4)) 14))

(module+ main
  (define inputs (for/list ([line (in-lines)]) (string->number line)))
  (displayln (part-a inputs))
  (displayln (part-b inputs)))


