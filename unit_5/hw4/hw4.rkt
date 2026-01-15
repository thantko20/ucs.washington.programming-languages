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

(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    dan))

(define (stream-add-one s)
  (lambda()
    (let ([pr (s)])
      (cons (cons 1 (car pr))
            (stream-add-one (cdr pr))))
    )
  )

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (lambda()
                  (let [(nx (list-nth-mod xs n))
                        (ny (list-nth-mod ys n))]
                    (cons (cons nx ny)
                          (f (+ n 1))))))])
    (f 0)))

(define (vector-assoc v vec)
  (letrec ([helper
            (lambda(pos)
              (cond [(= pos (vector-length vec)) #f]
                    [(not (pair? (vector-ref vec pos))) #f]
                    [(equal? v (car (vector-ref vec pos))) (vector-ref vec pos)]
                    [#t (helper (+ pos 1))]))])
    (helper 0)))

(define (caching-assoc xs n)
  (letrec ([vec (make-vector n #f)]
           [pos 0])
    (lambda(v)
      (let ([ans (vector-assoc v vec)])
        (if ans ans
            (let ([assoc-ans (assoc v xs)])
              (if assoc-ans
                  (
                   begin
                    (vector-set! vec pos assoc-ans)
                    (set! pos (if (= pos (- n 1))
                                  0
                                  (+ pos 1)))
                    assoc-ans
                    )
                  #f)))))))

(define-syntax while-greater
  (syntax-rules (do)
    [(while-greater e1 do e2)
     (letrec (
              [n1 e1]
              [f (
                  lambda()
                   (if (> e2 n1)
                       (f)
                       #t))])
       (f))]))
