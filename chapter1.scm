#|
1.Building Abstractions with Procedures

1.1 The Elements of Programming
|#

#| Exercise 1.3: Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers. |#

; ver. 1
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (min-of-2 a b)
  (if (< a b)
      a
      b))
(define (min-of-3 a b c)
  (if (< (min-of-2 a b) c)
      (min-of-2 a b)
      c))
(define (solution a b c)
  (cond ((= (min-of-3 a b c) a) (sum-of-squares b c))
        ((= (min-of-3 a b c) b) (sum-of-squares a c))
        (else (sum-of-squares a b))))

; ver. 2
(define (sq x) (* x x))
(define (min a b)
  (if (< a b)
      a
      b))
(define (solution a b c)
  (+ (sq a)
     (sq b)
     (sq c)
     (-(sq (min (min a b) c)))))

; ver. 3
(define (sq x) (* x x))
(define (sum-sq x y) (+ (sq x) (sq y)))
(define (min a b)
  (if (< a b)
      a
      b))
(define (max a b)
  (if (> a b)
      a
      b))
(define (solution a b c)
  (sum-sq (max a b)
          (max (min a b) c)))
