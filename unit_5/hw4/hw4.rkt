#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence spacing low high)
  (if (> low high)
      null
      (cons low (sequence spacing (+ low spacing) high))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? list) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; (define (stream-for-k-steps s k)
;   (if (< k 1)
;       null
;       (cons (car s) (stream-for-k-steps (cdr s) (- k 1)))))
(define (stream-for-k-steps s k)
  (if (= k 0)
      null
      (let ([pr (s)])
        (cons (car pr)
              (stream-for-k-steps (cdr pr) (- k 1))))))


(define funny-number-stream
  (letrec ([f (lambda (x) (cons
                           (if (= (remainder x 6) 0)
                               (* x -1)
                               x)
                           (lambda () (f (+ x 1)))))]
           )
    (lambda () (f 1))))