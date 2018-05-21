(ns ch1)

;; the substitution model is a model about the method of interpreter
;; evaluation where a compound procedure is replaced with its meaning
;; i.e it's constituent elements till all operators/procedures are
;; primitive.

;; the applicative order is an order of evaluation whereby a compound
;; or primitive procedure is evaluated, then its arguments are
;; evaluated before the compound procedure is then applied to

;; the normal order is an order of evaluation whereby a compound
;; or primitive procedure is evaluated, but its arguments are not
;; evaluated but rather replace their formal parameters in the
;; procedure.


;; Ex 1.2
(/ (+ 5 4
      (- 2 (- 3 (+ 6 (/ 1 3)))))
   (* 3
      (- 6 2)
      (- 2 7)))


;; Ex 1.3
(defn square [x]
  (* x x))

(defn sum-of-squares [x y]
  (+ (square x) (square y)))

(defn sos-max2 [a b c]
  (cond (and (>= (- a c) 0)
             (>= (- b c) 0)) (sum-of-squares a b)
        (and (>= (- a b) 0)
             (>= (- c b) 0)) (sum-of-squares a c)
        (and (>= (- b a) 0)
             (>= (- c a) 0)) (sum-of-squares b c)))


;; Ex 1.5
;; First the compound procedure is evaluated for application. Then
;; 0 is evaluated to a number. (p) is evaluated to the compound expression
;; (p). This means the (test 0 (p)) will result in an infinite evaluation
;; of p as a function in the applicative order.

;; For the normal order, test is evaluated to the compound. But rather than
;; evaluate its arguments, it replaces the formal parameters in the test
;; procedure with the arguments. And because x is indeed 0 and if special
;; form doesn't evaluate till its predicate is passed, (p) is never
;; evaluated. Hence the result is 0.


;; Newton Approximation Method

(defn abs [x]
  (if (< x 0) (- x) x))

(defn good-enough? [radicand guess]
  (< (abs (- (square guess) radicand)) 0.001))

(defn average [x y] (-> x (+ y) (/ 2)))

(defn improve [guess radicand]
  (average guess (/ radicand guess)))

(defn root
  ([radicand]
   (root radicand 1))

  ([radicand guess]
   (if (good-enough? radicand guess)
     (float guess)
     (recur radicand (improve guess radicand)))))


;; Ex 1.6
;; They must be joking. The reason if is a special form is because if
;; is not supposed to evaluate any of the branches till it knows the
;; answer to the predicate. This means their new-if will evaluate the
;; predicate first, the initial guess next and forever recurse over the
;; root function. This is a case of inifinite recursion.


;; Ex 1.7
;; For the issue with small numbers, the error range is way too high to
;; properly see if the guess is good enough. For extremely low numbers
;; like 0.00000016, it is impossible to get the correct answer because
;; with a threshold of 0.001 in diff where,
;; guess^2 - 0.00000016 < 0.001
;; guess < root(0.00100016)
;; This means the range of possible guesses is too large leading to an
;; incorrect answer(I got 0.031251706).

;; TODO: Get a better understanding of the computers and precision
;; For the issue with large numbers, the error range is simple too small.

;; Because the new good-enough? procedure doesn't rely on the square of
;; of the guess, error in guessing doesn't lead to a distant answer. Instead
;; it relies on the rate of change of the guess. Hence when the delta between
;; guesses is relatively small the guess must be good enough.
(defn good-enough2? [old-guess new-guess]
  (<= (/ (abs (- old-guess new-guess)) old-guess) 0.001))

(defn optimized-root
  ([radicand]
   (optimized-root radicand 1))

  ([radicand guess]
   (optimized-root radicand guess (improve guess radicand)))

  ([radicand old-guess new-guess]
   (if (good-enough2? old-guess new-guess)
     (float new-guess)
     (recur radicand new-guess (improve new-guess radicand)))))

;; Ex 1.8
(defn improve-cube [guess radicand]
  (/ (+
      (/ radicand (square guess))
      (* 2 guess))
     3))

(defn cube-root
  ([radicand]
   (cube-root radicand 1))

  ([radicand guess]
   (cube-root radicand guess (improve-cube guess radicand)))

  ([radicand old-guess new-guess]
   (if (good-enough2? old-guess new-guess)
     (float new-guess)
     (recur radicand new-guess (improve-cube new-guess radicand)))))
