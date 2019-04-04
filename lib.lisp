(define >
    (lambda (x y)
        (cond
            ((eq? x y) #f)
            ((< x y) #f)
            (#t #t)
        )
    )
)

(define <=
    (lambda (x y)
        (cond
            ((> x y) #f)
            (#t #t)
        )
    )
)

(define >=
    (lambda (x y)
        (cond
            ((< x y) #f)
            (#t #t)
        )
    )
)

(define abs
    (lambda (x)
        (cond
            ((< x 0) (* x (- 1)))
            (#t x)
        )
    )
)

(define not
    (lambda (x)
        (cond
            ((eq? x #t) #f)
            (#t #t)
        )
    )
)

(define or
    (lambda (x y)
        (cond
            ((eq? x #t) #t)
            ((eq? y #t) #t)
            (#t #f)
        )
    )
)

(define and
    (lambda (x y)
        (cond
            ((eq? x #t)
                (cond
                    ((eq? y #t) #t)
                    (#t #f)
                )
            )
            (#t #f)
        )
    )
)

(define cadr
    (lambda (x)
        (car (cdr x))
    )
)

(define caddr
    (lambda (x)
        (car (cdr (cdr x)))
    )
)

(define caadr
    (lambda (x)
        (car (car (cdr x)))
    )
)

(define caaddr
    (lambda (x)
        (car (car (cdr (cdr x))))
    )
)

(define null?
    (lambda (x)
        (cond
            ((eq? x '()) #t)
            (#t #f)
        )
    )
)

(define length
    (lambda (x)
        (cond
            ((null? x) 0)
            (#t (+ 1 (length (cdr x))))
        )
    )
)

(define append
    (lambda (x y)
        (cond
            ((not (null? x)) (cons (car x) (append (cdr x) y)))
            ((not (null? y)) (cons (car y) (append x (cdr y))))
            (#t '())
        )
    )
)

(define appends
    (lambda (x)
        (cond
            ((<= (length x) 1) (cdr x))
            ((<= (length x) 2) (append (car x) (cadr x)))
            (#t (append (car x) (appends (cdr x))))
        )
    )
)

;; Marche pas
(define reverse
    (lambda (x)
        (cond
            ((<= (length x) 1) x)
            (#t (cons (reverse (cdr x)) (car x)))
        )
    )
)