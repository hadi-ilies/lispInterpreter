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