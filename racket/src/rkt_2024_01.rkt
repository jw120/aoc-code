#lang racket

;; Day 1 - Advent of Code 2018

(define (read-input)
  (let ([lines (for/list ([line (in-lines)])
                 (string-split line))])
    (values (map (λ (line) (string->number (first line))) lines)
            (map (λ (line) (string->number (second line))) lines))))

; Part a: Sum of absolute differences of sorted lists
(define (part-a x y)
  (for/sum ([i (sort x <)] [j (sort y <)]) (abs (- i j))))

(module+ test
  (require rackunit)
  (check-equal? (part-a '(3 2 1) '(2 3 4)) 3))

; Part b: Product of first list and number of occurrences in second list
(define (part-b x y)
  (for/sum ([i x]) (* i (count (λ (x) (equal? i x)) y))))

(module+ test
  (check-equal? (part-b '(3 2 1 4) '(2 3 4 4)) (+ 3 2 8)))

(module+ main
  (define-values (x y) (read-input))
  (displayln (part-a x y))
  (displayln (part-b x y)))
