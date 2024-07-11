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

;What happens when Alyssa atempts to use this to compute square roots? Explain.

#| Solution: Eva's new-if always evaluates both possible answers
(then-clause and else-clause) regardless of the condition, leading to
aunnecessary computations and errors in recursive use.|#


#|Exercise 1.7: The good-enough? test used in computing square roots will
not be very effective for finding the square roots of very small numbers.
Also, in real computers, arithmetic operations are almost always
performed with limited precision. This makes our test inadequate for very large
numbers. Explain these statements, with examples showing how the test fails
for small and large numbers. An alternative strategy for implementing
good-enough? is to watch how guess changes from one iteration to the next and to
stop when the change is a very small fraction of the guess. Design
a square-root procedure that uses this kind of end test.
Does this work better for small and large numbers?|#

; Solution:
(if (good-enough? guess
                    (improve guess x))
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- guess x)) 0.00001))

(define (average x y)
  (/ (+ x y) 2))

(define (square-root x)
  (sqrt-iter 1.0 x))


#|Exercise 1.8: Newton’s method for cube roots is based on the fact that if y
is an approximation to the cube root of x, then a better approximation
is given by the value (x / (y^2) + 2y) / 3.
Use this formula to implement a cube-root procedure analogous
to the square-root procedure. (In Section 1.3.4 we will see how to implement
Newton’s method in general as an abstraction of these square-root
and cube-root procedures.) |#

; Solution:
(define (cube-root-iter previous-guess guess x)
  (if (good-enough? previous-guess guess)
      guess
      (cube-root-iter guess
                      (improve guess x)
                      x)))

(define (improve guess x)
  (/ (+ (* 2 guess)
        (/ x
           (* guess guess)))
     3))

(define (good-enough? previous-guess guess)
  (< (abs (- previous-guess
             guess))
     0.001))

(define (cube-root x)
  (cube-root-iter 0.0 1.0 x))


#|Exercise 1.9: Each of the following two procedures defines a method
for adding two positive integers in terms of the procedures inc,
which increments its argument by 1, and dec, which decrements its argument by 1. |#
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

#|Using the substitution model, illustrate the process generated by each procedure
in evaluating (+ 4 5). Are these processes iterative or recursive? |#

; Solution:

#| 1. Recursive process:

(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

This process is recursive because it computes through
recursive calls until it reaches the base case (when `a` becomes 0).

2. Iterative process:

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

This process is iterative because it computes in a loop, using
intermediate values (`a` decreases from 4 to 0, while `b` increases from 5 to 9).

Thus, the first procedure uses recursion to compute the sum,
while the second procedure uses iteration. |#
