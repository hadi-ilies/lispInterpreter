(define > (
    lambda (x y)
        (if (eq? x y) #f (if (< x y) #f #t))
))

(define >= (
    lambda (x y)
        (if (< x y) #f #t)
))

(define <= (
    lambda (x y)
        (if (> x y) #f #t)
))

(define not (
    lambda (x)
        (if (eq? x #t) #f #t)
))

(define or (
    lambda (x y)
        (if (eq? x #t) #t (if (eq? y #t) #t #f))
))

(define and (
    lambda (x y)
        (if (eq? x #t) (if (eq? y #t) #t #f) #f)
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
        (if (eq? x '()) #t #f)
))

(define length (
    lambda (x)
        (if (null? x) 0 (+ 1 (length (cdr x))))
))

(define reverse (
    lambda (x)
        (if (<= (length x) 1) x (cons (reverse (cdr x)) (car x)))
))