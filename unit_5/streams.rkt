#lang racket

(provide (all-defined-out))

(define ones (lambda() (cons 1 ones)))

(define (f x) (cons x (lambda () (f (+ x 1)))))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda() (f (+ x 1)))))])
    (lambda() (f 1))))

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

(define (number-until stream tester)
  (letrec ([f (lambda (stream acc)
                (let ([pr (stream)])
                  (if (tester (car pr))
                      acc
                      (f (cdr pr) (+ acc 1)))))])
    (f stream 1)))