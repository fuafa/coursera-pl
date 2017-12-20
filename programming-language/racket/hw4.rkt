
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; 2
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

;; 3
(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (= (length xs) 0)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

;; 4
(define (stream-for-n-steps s n)
  (if (< n 1)
      null
      (let ([res (s)])
        (cons (car res) (stream-for-n-steps (cdr res) (- n 1))))))

;; 5
(define (funny-number-stream)
  (letrec ([f (lambda (x) (if (= (remainder x 5) 0)
                             (cons (- x) (lambda () (f (+ x 1))))
                             (cons x (lambda () (f (+ x 1))))))])
    (f 1)))

;; 6
(define dan-then-dog
  (lambda () (letrec ([f (lambda (s) (if (string=? s "dan.jpg")
                                         (cons "dog.jpg" (lambda () (f "dog.jpg")))
                                         (cons "dan.jpg" (lambda () (f "dan.jpg")))))])
               (f "dog.jpg"))))

;; 7
(define (stream-add-zero s)
  (lambda () (let ([res-s (s)])
    (cons (cons 0 (car res-s)) (stream-add-zero (cdr res-s))))))

;; 8
(define (cycle-lists xs ys)
  (lambda () (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
               (f 0))))

;; 9
(define (vector-assoc v vec)
  (letrec ([vec-len (vector-length vec)]
           [f (lambda (pos) (if (>= pos vec-len)
                                #f
                                (let ([vec-element (vector-ref vec pos)])
                                  (if (pair? vec-element)
                                      (if (equal? (car vec-element) v)
                                          vec-element
                                          (f (+ pos 1)))
                                      (f (+ pos 1))))))])
    (f 0)))

;; 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [pos 0]
           [f (lambda (v) (let ([cache-ans (vector-assoc v cache)])
                            (if cache-ans
                                cache-ans
                                (let ([ans (assoc v xs)])
                                  (if ans
                                      (begin (vector-set! cache pos ans)
                                             (set! pos (remainder (+ pos 1) n))
                                             ans)
                                      ans)))))])
    f))

;; 11
(define-syntax while-less
  (syntax-rules (do)
                [(while-less e1 do e2)
                (letrec ([e1-res e1]
                         [f (lambda () (if (> e1-res e2)
                                        (f)
                                        #t))])
                  (f))]))
                                        
                

  
  
  
