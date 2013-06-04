;;; Exercise 1.2: Translate the following expression into prefix form.
;;; (5 + 4 + (2 - (3 - (6 + 4/5)))) / (3 * (6 - 2) * (2 - 7))
(define ans (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7))))

;;; test
(= (/ -37 150) ans)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.3: Define a procedure that takes three numbers as arguments and
;;; returns the sum of the squares of the two larger numbers.
(define (square x)
  (* x x)) 

(define (sum a b c)
  (if (>= a b)
      (if (>= b c)
          (+ (square a) (square b))
          (+ (square a) (square c)))
      (if (>= a c)
          (+ (square b) (square a))
          (+ (square b) (square c)))))

;;; tests
(= 0 (sum 0 0 0))
(= 13 (sum 1 2 3))
(= 41 (sum 3 4 5))
(= 1 (sum -1 0 1))
(= 5 (sum -1 -2 -3))
(= 25 (sum -3 -4 -5))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.4: Observe that our model of evaluation allows for combinations
;;; whose operators are compound expressions. Use this observation to describe
;;; the behavior of the following procedure,
;;;   ==> (define (a-plus-abs-b a b)
;;;   ...   ((if (> b 0) + -) a b))

;;; - To get a plus |b|, do a+b if b>0, else a-b if b<0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.5: Ben Bitdiddle has invented a test to determine whether the
;;; interpreter he is faced with is using applicative-order evaluation or
;;; normal-order evaluation. He defines the following two procedures:
;;;   ==> (define (p) (p))
;;;   ... (define (test x y)
;;;   ...   (if (= x 0) 0 y))
;;;
;;; Then he evaluates the expression
;;;   ==> (test 0 (p))
;;;
;;; What behavior will Ben observe with an interpreter that uses
;;; applicative-order evaluation? What behavior will he observe with an
;;; interpreter that uses normal-order evaluation? Explain your answer. (Assume
;;; that the evaluation rule for the special form if is the same whether the
;;; interpreter is using normal or applicative order: The predicate expression
;;; is evaluated first, and the result determines whether to evaluate the
;;; consequent or the alternative expression.)

;;; - Applicative-order evaluation => eager evaluation, viz. arguments are
;;;   evaluated prior to being passed into a function, whereas normal-order
;;;   evaluation => lazy evaluation, viz. an argument is not evaluated until
;;;   the interpreter knows it will need the value of that argument
;;; - For a lazy interpreter, the expression will terminate with result 0. For
;;;   an eager interpeter, the expression will never terminate because the
;;;   'p' function is a recursive call to itself with no base case.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.6: Alyssa doesn't see why 'if' needs to be provided as a special
;;; form, "Why can't it just be defined as an ordinary procedure in terms of
;;; 'cond'?" Eva claims this can indeed be done and defines it as
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;;; Eva demonstrates this program,
(= 5 (new-if (= 2 3) 0 5))
(= 0 (new-if (= 1 1) 0 5))

;;; Alyssa uses new-if to rewrite the square root program,
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

;;;   ==> (sqrt-iter 1 2)
;;;
;;; - An infinite loop occurs in sqrt-iter with eager-racket because 'new-if'
;;;   is a procedure where the type of evaluation is left up to the
;;;   interpreter, in this case eager. Special forms seem to choose lazy/eager
;;;   evaluation on a case-by-case basis.
;;; - N.B. Switching to lazy racket will evaluate this expression just fine.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.7: The 'good-enough?' test used in computing square roots will
;;; not be very effective for finding the square roots of very small numbers.
;;; Also, in real computers, arithmetic operations are almost always performed
;;; with limited precision. This makes our test inadequate for very large
;;; numbers. Explain these statements, with examples showing how the test fails
;;; for small and large numbers. An alternative strategy for implementing
;;; 'good-enough?' is to watch how 'guess' changes from one iteration to the
;;; next and to stop when the change is a very small fraction of the guess.
;;; Design a square-root procedure that uses this kind of end test. Does this
;;; work better for small and large numbers?
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (sqrt x) (sqrt-iter 1.0 x))

(define (new-good-enough? last-guess guess)
  (< (abs (- last-guess guess)) (* 0.001 guess)))

(define (new-sqrt-iter last-guess guess x)
  (if (new-good-enough? last-guess guess)
      guess
      (new-sqrt-iter guess (improve guess x) x)))

(define (new-sqrt x) (new-sqrt-iter 0.0 1.0 x))

;;; The sqrt function returns inaccurate values on small numbers because of
;;; its relatively high tolerance.
(sqrt 0.01)        ; Accurate
(new-sqrt 0.01)    ; Accurate
(sqrt 0.05)        ; Close
(new-sqrt 0.05)    ; Accurate
(sqrt 0.001)       ; Inaccurate
(new-sqrt 0.001)   ; Accurate

;;; The sqrt function is accurate for large numbers until it hits a point
;;; where what seems like an overflow occurs...the machine can't accurately
;;; represent the floating point number. Though I'm not clear why I can
;;; type a large number but not get the square of its square-root accurately.
(sqrt 1e30)        ; Accurate
(new-sqrt 1e30)    ; Accurate
(sqrt 16e64)       ; Infinite Loop!
(square 4.0e32)    ; Accurate
(new-sqrt 16e64)   ; Accurate


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.8: Newton's method for cube roots is based on the fact that if
;;; 'y' is an approximation to the cube root of 'x', then a better
;;; approximation is given by the value (x/y^2 + 2y) / 3. Use this formula
;;; to implement a cube-root procedure analogous to the square-root procedure.
(define (cube x) (* x x x))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x) x)))

(define (cbrt x)
  (cbrt-iter 1.0 x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.9: Each of the following two procedures defines a method for
;;; adding two positive integers in terms of the procedures inc, which
;;; increments its argument by 1, and dec, which decrements its argument by 1.
;;;   ==> (define (+ a b)
;;;   ...   (if (= a 0) b (inc (+ (dec a) b))))
;;;
;;;   ==> (define (+ a b)
;;;   ...   (if (= a 0) b (+ (dec a) (inc b))))
;;;
;;; Using the substitution model, illustrate the process generated by each
;;; procedure in evaluating (+ 4 5). Are these processes iterative
;;; or recursive?

;;; - (+ 4 5) = (inc (+ 3 5)) = (inc (inc (+ 2 5))) = (inc (inc (inc (+ 1 5))))
;;;           = (inc (inc (inc (inc (+ 0 5))))) = (inc (inc (inc (inc 5)))) = 9
;;;
;;; - (+ 4 5) = (+ 3 6) = (+ 2 7) = (+ 1 8) = (+ 0 9) = 9


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.10: The following procedure computes a mathematical function
;;; called Ackermann's function.
;;;   (define (A x y)
;;;     (cond ((= y 0) 0)
;;;           ((= x 0) (* 2 y))
;;;           ((= y 1) 2)
;;;           (else (A (- x 1)
;;;                    (A x (- y 1))))))
;;;
;;; What are the values of the following expressions?
;;;   (A 1 10)
;;;   (A 2 4)
;;;   (A 3 3)
;;;
;;; Consider the following procedures, where A is the procedure defined above,
;;;   ==> (define (f n) (A 0 n))
;;;   ==> (define (g n) (A 1 n))
;;;   ==> (define (h n) (A 2 n))
;;;   ==> (define (k n) (* 5 n n))
;;;
;;; Give concise mathematical definitions for the functions computed by the
;;; procedures f, g, and h for the positive integer values of n. For example,
;;; (k, n) computes 5n^2.

