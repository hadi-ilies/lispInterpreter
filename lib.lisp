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