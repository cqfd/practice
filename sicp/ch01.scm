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
      (define (A x y)
	(cond ((= y 0) 0)
	      ((= x 0) (* 2 y))
	      ((= y 1) 2)
	      (else (A (- x 1)
		       (A x (- y 1))))))
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

;;; (A 1 10) = (A 0 (A 1 9))
;;;          = (A 0 (A 0 (A 1 8)))
;;;          = (A 0 (A 0 (A 0 (A 1 7))))
;;;          = (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;;;          = (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;;;          = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;;;          = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;;;          = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;;;          = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;;;          = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;;;          = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;;;          = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
;;;          = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
;;;          = (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
;;;          = (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
;;;          = (A 0 (A 0 (A 0 (A 0 64))))
;;;          = (A 0 (A 0 (A 0 128)))
;;;          = (A 0 (A 0 256))
;;;          = (A 0 512)
;;;          = 1024 => 2^10

;;; (A 2 4) = (A 1 (A 2 3))
;;;         = (A 1 (A 1 (A 2 2)))
;;;         = (A 1 (A 1 (A 1 (A 2 1))))
;;;         = (A 1 (A 1 (A 1 2))
;;;         = (A 1 (A 1 (A 0 (A 1 1))))
;;;         = (A 1 (A 1 (A 0 2)))
;;;         = (A 1 (A 1 4))
;;;         = (A 1 (A 0 (A 1 3)))
;;;         = (A 1 (A 0 (A 0 (A 1 2))))
;;;         = (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;;;         = (A 1 (A 0 (A 0 (A 0 2))))
;;;         = (A 1 (A 0 (A 0 4)))
;;;         = (A 1 (A 0 8))
;;;         = (A 1 16)
;;;         = (A 1 16) => 2^16

;;; (A 3 3) = (A 2 (A 3 2))
;;;         = (A 2 (A 2 (A 3 1)))
;;;         = (A 2 (A 2 2))
;;;         = (A 2 (A 1 (A 2 1)))
;;;         = (A 2 (A 1 2))
;;;         = (A 2 4)
;;;         = (A 1 16) => 2^16

;;; (f n) = (A 0 n) => 2n

;;; (g n) = (A 1 n) => 2^n, n > 0

;;; (h n) = (A 2 n) => 2^(2^2...[n times]), n > 0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.2.2 TREE RECURSION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; - fib1 is easier to reason about (its essentially a translation of the
;;;   definition), but its inefficient (can't tail call optimize?)
;;;   - The number of steps in tree-recursive process is proportional to the
;;;     number of nodes in the tree, 2^n in this case.
;;;   - The space required is proportional to the maximum depth of the tree.
;;; - fib2 is a linear iteration which is more efficient, but slightly less
;;;   clear than fib1.
(define (fib1 n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

(define (fib2 n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.11: A function f is defined by the rule that f(n) = n if
;;; n < 3 and f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n>=3. Write a procedure
;;; that computes f by means of a recursive process. Write a procedure that
;;; computes f by means of an iterative process.
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f-iter a b c count)
  (if (= count 0)
      a
      (f-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
(define (f n)
  (f-iter 0 1 2 n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.12: Write a procedure that computes elements of Pascal's
;;; triangle by means of a recursive procedure.
(define (pascal n m)
  (cond ((or (< n 0) (< m 0) (> m n)) 0)
	((= 0 n m) 1)
	(else (+ (pascal (- n 1) m) (pascal (- n 1) (- m 1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.14: Draw the tree illustrating the process generated by
;; the count-change procedure in making change for 11 cents. What are
;; the orders of growth of the space and number of steps used by this
;; process as the amount to be changed increases?
(define (count-change amount)
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
	  ((or (< amount 0) (= kinds-of-coins 0)) 0)
	  (else (+ (cc amount
		       (- kinds-of-coins 1))
		   (cc (- amount
			  (first-denomination kinds-of-coins))
		       kinds-of-coins)))))
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
	  ((= kinds-of-coins 2) 5)
	  ((= kinds-of-coins 3) 10)
	  ((= kinds-of-coins 4) 25)
	  ((= kinds-of-coins 5) 50)))
  (cc amount 5))

;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.15: The sine of an angle (specified in radians) can be
;;; computed by making use of the approximation sin x ~ x if x is
;;; sufficiently small (<= 0.1 radians), and the trigonometric identity
;;; sin x = 3 sin (x/3) - 4 sin^3 (x/3) to reduce the size of the argument
;;; of sin. These ideas are incorporated in the following procedures,
(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;;; a. 'p' is applied five times
;;; (sine 12.15) = (p (sine 4.05))
;;;              = (p (p (sine 1.35)))
;;;              = (p (p (p (sine 0.45))))
;;;              = (p (p (p (p (sine 0.15)))))
;;;              = (p (p (p (p (p (sine 0.05))))))
;;;              = (p (p (p (p (p 0.05)))))

;;; b. The order of growth in space and number of steps is O(n)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.16: Design a procedure that evolves an iterative
;;; exponentiation process that uses successive squaring and uses a
;;; logarithmic number of steps, as does 'fast-expt'.
(define (expt b n)
  (define (even? n) (= (remainder n 2) 0))
  (define (iter accum count)
    (cond ((= count 0) accum)
	  ((even? count) (iter (square accum) (/ count 2)))
	  (else (iter (* b accum) (- count 1)))))
  (iter 1 n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.17 The exponentiation algorithms in this section are based
;; on performing exponentiation by means of repeating multiplication. In
;; a similar way, one can perform integer multiplication by means of
;; repeated addition. The following multiplication procedure is analogous
;; to the 'expt' procedure:
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
;; The algorithm takes a number of steps that is linear in b. Now suppose
;; we include, together with addition, operations 'double', which doubles
;; an integer, and 'halve', which divides an (even) integer by 2. Using
;; these, design a multiplication procedure analogous to 'fast-expt' that
;; uses a logarithmic number of steps.
(define (fast-mult a b)
  ;; N.B. This only works on small negative numbers because of the bit ops
  (define (even? n) (= (bitwise-and n 1) 0))
  (define (double x) (arithmetic-shift x 1))
  (define (halve x) (arithmetic-shift x -1))
  (cond ((= 0 b) 0)
	((even? b) (fast-mult (double a) (halve b)))
	(else (+ a (fast-mult a (- b 1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.18 Use the results of Exercise 1.16, 1.17 to devise a
;; procedure that generates an iterative process for multiplying two
;; integers in terms of adding, doubling, and halving and uses a
;; logarithmic number of steps.
(define (fast-mult a b)
  ;; N.B. This only works on small negative numbers because of the bit ops
  (define (even? n) (= (bitwise-and n 1) 0))
  (define (double x) (arithmetic-shift x 1))
  (define (halve x) (arithmetic-shift x -1))
  (define (iter aa bb s)
    (cond ((= 0 bb) s)
	  ((even? bb) (iter (double aa) (halve bb) s))
	  (else (iter aa (- bb 1) (+ aa s)))))
  (iter a b 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1.20 The process that a procedure generates is of course
;; dependent on the rules used by the interpreter. As an example, consider
;; the iterative 'gcd' procedure given above. Suppose we were to interpret
;; this procedure using normal-order evaluation. Using the substitution
;; method '(gcd 206 40)' and indicate the 'remainder' operations that
;; are actually performed. How many 'remainder' operations are actually
;; performed in '(gcd 206 40)'? In the applicative-order evaluation?

;; Normal-Order Eval (Lazy) [remainder is evaluated when 'if (= b 0)' is hit]
;; (gcd 206 40)
;; =>if (= 40 0)
;;      206
;;      (gcd 40 (remainder 206 40))
;; (gcd 40 (remainder 206 40))
;; =>if (= (remainder 206 40) 0)
;;      40
;;      (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;; =>if (= 6 0)
;;      40
;;      (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;; =>if (= (remainder 40 (remainder 206 40)) 0)
;;      (remainder 206 40)
;;      (gcd (remainder 40 (remainder 206 40))
;;           (remainder (remainder 206 40)
;;                      (remainder 40 (remainder 206 40))))
;; =>if (= 4 0)
;;      (remainder 206 40)
;;      (gcd (remainder 40 (remainder 206 40))
;;           (remainder (remainder 206 40)
;;                      (remainder 40 (remainder 206 40))))
;; (gcd (remainder 40 (remainder 206 40))
;;      (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;; =>if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
;;      (remainder 40 (remainder 206 40))
;;      (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;           (remainder (remainder 40 (remainder 206 40))
;;                      (remainder (remainder 206 40)
;;                                 (remainder 40 (remainder 206 40)))))
;; =>if (= 2 0)
;;      (remainder 40 (remainder 206 40))
;;      (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;           (remainder (remainder 40 (remainder 206 40))
;;                      (remainder (remainder 206 40)
;;                                 (remainder 40 (remainder 206 40)))))
;; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;      (remainder (remainder 40 (remainder 206 40))
;;                 (remainder (remainder 206 40)
;;                            (remainder 40 (remainder 206 40)))))
;; =>if (= (remainder (remainder 40 (remainder 206 40))
;;                    (remainder (remainder 206 40)
;;                               (remainder 40 (remainder 206 40)))))
;;         0)
;;      (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;      (remainder (remainder (remainder 206 40) (remainder
;;                                                 40
;;                                                 (remainder 206 40)))
;;                 (remainder (remainder 40 (remainder 206 40))
;;                            (remainder (remainder 206 40)
;;                                       (remainder 40 (remainder 206 40))))))
;; =>if (= 0 0)
;;      (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;      (remainder (remainder (remainder 206 40) (remainder
;;                                                 40
;;                                                 (remainder 206 40)))
;;                 (remainder (remainder 40 (remainder 206 40))
;;                            (remainder (remainder 206 40)
;;                                       (remainder 40 (remainder 206 40))))))
;; =>(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;; =>2 [requires 17 calls to remainder]

;; Applicative-Order (Eager) [remainder is evaluated prior to 'gcd' call]
;; (gcd 206 40) = (gcd 40 (remainder 206 40)) = (gcd 40 6)
;;              = (gcd 6 (remainder 40 6)) = (gcd 6 4)
;;              = (gcd 4 (remainder 6 4)) = (gcd 4 2)
;;              = (gcd 2 (remainder 4 2)) = (gcd 2 0)
;;              = 2 [requires 4 calls to remainder]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.21: Use the smallest-divisor procedure to find the smallest
;;; divisor of each of the following numbers, 199, 1999, 19999.
(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find test)
    (cond ((> (square test) n) n)
	  ((divides? test n) test)
	  (else (find (+ test 1)))))
  (find 2))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.30: The sum procedure above generates a linear recursion.
;;; The procedure can be rewritten so that the sum is performed iteratively.
;;; Show how to do this by filling in the missing expressions in the
;;; following definition,
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.42: Let f and g be two one-argument functions. The
;;; composition f after g is defined to be the function x -> f(g(x)).
;;; Define a procedure 'compose' that implements composition.
(define (compose f g)
  (lambda (x) (f (g x))))
