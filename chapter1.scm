#|
1.Building Abstractions with Procedures

1.1 The Elements of Programming
|#


#| Exercise 1.3: Define a procedure that takes three numbers as arguments
and returns the sum of the squares of the two larger numbers. |#

; Solution: ver. 1
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


#| Exercise 1.4: Observe that our model of evaluation allows
for combinations whose operators are compound expressions.
Use this observation to describe the behavior of the following procedure: |#
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

#| Solution: The procedure `(a-plus-abs-b a b)` returns the result
of the operation `a + abs(b)`, depending on the sign of `b`.|#


#| Exercise 1.5: Ben Bitdiddle has invented a test to determine
whether the interpreter he is faced with is using applicative-order
evaluation or normal-order evaluation. He defines the following two procedures: |#
(define (p) (p))
(define (test x y)
(if (= x 0) 0 y))
;Then he evaluates the expression
(test 0 (p))
#| What behavior will Ben observe with an interpreter that uses applicative-order
evaluation? What behavior will he observe with an interpreter that uses
normal-order evaluation? Explain your answer. |#

#| Solution: Applicative order of evaluation will result in infinite recursion for 
(test 0 (p)), whereas in normal order evaluation,
(test 0 (p)) will return 0 without evaluating (p) |#


#|Exercise 1.6: Alyssa P. Hacker doesn’t see why if needs to be provided as
a special form. “Why can’t I just define it as an ordinary procedure in
terms of cond?” she asks. Alyssa’s friend Eva Lu Ator claims this can indeed
be done, and she defines a new version of if:|#

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;Eva demonstrates the program for Alyssa:

(new-if (= 2 3) 0 5) > 5

(new-if (= 1 1) 0 5) > 0

;Delighted, Alyssa uses new-if to rewrite the square-root program:

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

;What happens when Alyssa aempts to use this to compute square roots? Explain.

#| Solution: Eva's new-if always evaluates both possible answers
(then-clause and else-clause) regardless of the condition, leading to
aunnecessary computations and errors in recursive use.|#
