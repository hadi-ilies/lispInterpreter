(define > (
    lambda (x y)
        (cond ((eq? x y) #f) ((< x y) #f) (#t #t))
))

(define >= (
    lambda (x y)
        (cond ((< x y) #f) (#t #t))
))

(define <= (
    lambda (x y)
        (cond ((> x y) #f) (#t #t))
))

(define not (
    lambda (x)
        (cond ((eq? x #t) #f) (#t #t))
))

(define or (
    lambda (x y)
        (cond ((eq? x #t) #t) ((eq? y #t) #t) (#t #f))
))

(define and (
    lambda (x y)
        (cond ((eq? x #t) (cond ((eq? y #t) #t) (#t #f))) (#t #f))
))

(define cadr (
    lambda (x)
        (car (cdr x))
))

(define caddr (
    lambda (x)
        (car (cdr (cdr x)))
))

(define caadr (
    lambda (x)
        (car (car (cdr x)))
))

(define caaddr (
    lambda (x)
        (car (car (cdr (cdr x))))
))

(define null? (
    lambda (x)
        (cond ((eq? x '()) #t) (#t #f))
))

(define length (
    lambda (x)
        (cond ((null? x) 0) (#t (+ 1 (length (cdr x)))))
))

(define reverse (
    lambda (x)
        (cond ((<= (length x) 1) x) (#t (cons (reverse (cdr x)) (car x))))
))