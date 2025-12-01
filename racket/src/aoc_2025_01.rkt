#lang racket

;; Day 1 - Advent of Code 2025

(define (parse-move line)
  (define distance (string->number (substring line 1)))
  (match (substring line 0 1)
    ["L" (- distance)]
    ["R" distance]))

(module+ test
  (require rackunit)
  (check-equal? (parse-move "R23") 23)
  (check-equal? (parse-move "L42") -42))

(define (run _moves) (values 0 0))

; fn run(moves: &[i32]) -> (i32, i32) {
;     let mut position: i32 = START_POSITION;
;     let mut zero_finishes: i32 = 0;
;     let mut zero_passes: i32 = 0;
;     for mv in moves {
;         zero_passes += (mv.abs()) / DIAL_SIZE;
;         let m: i32 = (mv.abs() % DIAL_SIZE) * mv.signum();
;         position += m;
;         if position % DIAL_SIZE == 0 {
;             zero_finishes += 1;
;             position = 0;
;         } else if position > DIAL_SIZE {
;             zero_passes += 1;
;             position -= DIAL_SIZE;
;         } else if position < 0 {
;             zero_passes += i32::from(position != m);
;             position += DIAL_SIZE;
;         }
;     }
;     (zero_finishes, zero_passes + zero_finishes)
; }



(module+ main
  (define moves (for/list ([line (in-lines)])
                  (parse-move line)))
  (define-values (a b) (run moves))
  (displayln a)
  (displayln b))
