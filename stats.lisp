(defpackage stats)

(defmacro def-set-operator (name arg-list &body body)
  "Define a function that will take successive arguments or a list of arguments."
  (let ((tail (car (last arg-list)))
        (docstring (if (stringp (first body))
                       (first body)
                       "This set-operator is undocumented."))
        (definition (if (stringp (first body))
                        (rest body)
                        body)))
    `(defun ,name ,arg-list
       ,docstring
       (if (consp (first ,tail))
           (apply #',name ,@(append
                             (butlast arg-list 2)
                             (list (list 'first tail))))
           ,@definition))))

(defun fact (n &optional (x 1))
  (if (< n 2)
      x
      (fact (- n 1) (* x n))))

(defun choose (n k)
  (let ((num (round n))
        (kay (round k)))
    (/ (fact num)
       (* (fact (- num kay))
          (fact kay)))))

(defun range-to (bound &key (start 0) (step 1))
  (loop for x from start below bound by step
     collect x))

(defun divisor-checker (d)
  (lambda (x) (= 0 (mod x d))))

(defun list-divisibles (range &rest divisors)
  (let ((checkers (mapcar #'divisor-checker divisors)))
    (remove-if-not
     (lambda (i)
       (loop for d in checkers
          when (funcall d i)
          return t
          finally (return nil)))
     range)))

(defun count-divisibles (range &rest divisors)
  (let ((divisibles (apply #'list-divisibles range divisors)))
    (values (length divisibles) divisibles)))

(defun expected-value (values probabilities)
  "Expected Value ('ev(event)'):
let weighted-values := map * (values probabilities)
  return (sum weighted-values) divided by (sum probabilities)"
  (and (= (length values) (length probabilities))
       (let ((space (apply #'+ probabilities)))
         (float
          (/ (apply #'+ (mapcar #'* values probabilities))
             space)))))

(defun expected-value-2 (&rest pairs)
  (let ((values (mapcar #'first pairs))
        (probabilities (mapcar #'second pairs)))
    (expected-value values probabilities)))

(defun binary-ev (win-value lose-value win-probability)
  (expected-value (list win-value lose-value) (list win-probability (- 1 win-probability))))

(def-set-operator sum (&rest nums)
  (apply #'+ nums))

(def-set-operator mean (&rest nums)
  (float
   (/ (sum nums)
      (length nums))))

(def-set-operator median (&rest nums)
  (let ((l (1- (truncate (/ (length nums) 2))))
        (sorted (sort nums #'>)))
    (print sorted)
    (if (evenp l)
        (mean (elt sorted l) (elt sorted (1+ l)))
        (elt sorted l))))

(defun compress (nums &optional (acc nil))
  (let ((n (first nums)))
    (if (endp nums)
        acc
        (let ((c
               (length
                (remove n nums :test-not #'=))))
          (compress (remove n nums)
                    (acons n c acc))))))

(def-set-operator mode (&rest nums)
  (let ((counts (compress nums)))
    (first (first (sort counts #'> :key #'cdr)))))

(def-set-operator variance (&rest nums)
  "Variance (lower-case sigma squared):
let distances := map (x - mean) nums
  return mean distance"
  (let ((m (mean nums)))
    (values (mean(mapcar
                  (lambda (x) (expt (- x m) 2))
                  nums))
            m)))

(def-set-operator sample-variance (&rest nums)
  "Sample variance (S squared) is like variance except that
instead of taking the actual mean of the squared set,
we sum it and divide the result by (length - 1)"
  (let* ((m (mean nums))
         (variances (mapcar (lambda (x) (expt (- x m) 2)) nums)))
    (values
     (/ (sum variances) (1- (length variances)))
     m)))

(def-set-operator std-dev (&rest nums)
  "Standard Deviation (lower-case sigma):
square root of variance"
  (multiple-value-bind (v m)
      (variance nums)
    (values (sqrt v) v m)))

(def-set-operator sample-std-dev (&rest nums)
  "Sample std dev (S) is to std-dev as sample-variance is to variance"
  (multiple-value-bind (v m)
      (sample-variance nums)
    (values (sqrt v) v m)))

(defun z-score (value mean std-dev)
  "Distance between value and mean, in increments of std-dev
or
   Number of standard deviations away from the mean."
  (float (/ (- value mean) std-dev)))

(defun bayes-theorem (pA pB pA-B)
  (/ (* pA-B pA)
     pB))


;;; all work below here references this:
;;; https://www.countbayesie.com/blog/2016/5/1/a-guide-to-bayesian-statistics
;;;;; Bayes Theorem:

;; p(a | b) = p(b | a) * p(a) / p(b)

;; probability a given b is equal to
;; (probability b given a times probability a) divided by probability b



;; given 3 colors:
;; blue (40/60)
;; red (20/60)
;; yellow (6/60) 

;; [overlap]
;; p (y | b) = 2/40
;; p (y | r) = 4/20

;; numYellow = p(y) * totalPegs[60]
;; numYellow = 1/10 * 60
;; numYellow = 6

;; numRed = p(r) * totalPegs
;; numRed = 20

;; numRed/Yellow = p(y | r) * numRed
;; numRed/Yellow =      1/5 * 20
;; numRed/Yellow = 4

;; p (r | y) = numRed/Yellow / numYellow
;; p (r | y) =             4 / 6
;; p (r | y) = 2/3

;; p (r | y) = p(y | r) * p(y) / p(r)



;;; Priors and Posteriors

;; Posterior = Likelihood * Prior
;; (usually normalized)

;; Beta(a(post), b(post)) = Beta(a(like) + a(prior), b(like) + b(prior))




;;;;; Example with Food Allergies
;;; Question: How skeptical should I be if a friend claims a food allergy?

;;; Data: http://www.ncbi.nlm.nih.gov/pmc/articles/PMC154188/

;;; How many allergic?
;;; How many claim allergies?

;;; Has Allergy : A
;; P(A_child) = .06
;; P(A_adult) = .015

;;; Claims Allergy : C
;; P(C [adult + child]) = 0.3

;; P(A | C)?
;; P(A | C) = P(C | A) * P(A) / P(C)

;; We assume P(C | A) = 1
;; i.e. that someone who has an allergy is
;; 100% likely to claim an allergy.

;; P(A_child | C) =
;; 1 * 0.06 / 0.3
;; 0.06 / 0.3
;;            == 0.2
;; 20%

;; P(A_adult | C) =
;; 0.015 / 0.3
;;            == 0.05
;; 5%




;;;;; Sidequest!
;;;; Measuring Difference between Percentage ratings

;; Change in Percent Rating (%D for %Delta) =
;; %D = (P_new - P_old) / P_old


(defun delta-percent (p-old p-new)
  (/ (- p-new p-old)
     p-old))

(declaim (inline is-integer))
(defun is-integer (float)
  (= float (round float)))

(declaim (inline move-decimal))
(defun move-decimal (num places)
  (* num (expt 10 places)))

(defun left-padding (num)
  (loop for i from 0
     do (let ((n (move-decimal num i)))
          (when (is-integer n)
            (return (values i (round n)))))))

(defun right-padding (num)
  (loop for i downfrom 0
     do (let ((n (move-decimal num (1- i))))
          (when (not (is-integer n))
            (return (values i (round (* 10 n))))))))

(defun padding-zeros (num)
  (if (> num 1)
      (right-padding num)
      (if (< num 1)
          (left-padding num)
          (values 0 1))))

(defun order-of-magnitude (num)
  (loop for i from 0
     do (let ((m (expt 10 i)))
          (when (> m num)
            (return (values i m))))))

(defun percent-certainty (percent)
  "assumes decimal notation:
for 99% pass 0.99

returns the expected proportion of failures to attempts"
  (multiple-value-bind (i p)
      (padding-zeros percent)
    (declare (ignore i))
    (multiple-value-bind (e m)
        (order-of-magnitude p)
      (declare (ignore e))
      (let ((not-p (- m p)))
        (coerce (/ not-p m) 'ratio)))))

(defun format-percent (percent)
  (let ((p (* 100 percent))
        (control-string (typecase percent
                          (integer "~d%")
                          (real    "~f%"))))
    (format nil control-string p)))

;;;;; Bayes Factor and the Null Hypothesis
;;; H      : Null Hypothesis (that everything is random and correlations don't exist)
;;; h      : Tested Hypotheses
;;; D      : The Data, a set of positive or negative responses with respect to h.
;;; p(D|h) : probability we see The Data given h
;;; p(D|H) : probability we see The Data given H


;;;; Example with the perfect psychic who is always right
;;; H : Null Hypothesis (not psychic)
;;; h : Tested Hypothesis (totally psychic, brah)
;;; D : in this case D is an ever-growing string of right answers

;;; p(D|h) : in this case, always 1 (perfect psychic can predict the future, ergo is always right)

;;; p(D|H) : probability that we see the experimental Data given the Null Hypothesis H
;;;          assuming 50% chance of being right, this is 0.5^n
;;;          where n is the number of answers so far

;;; Bayes Factor (Measurement of competition between hypotheses):
;;; often known as k
;;;  p(D|h1) / p(D|h2)
;;; or (with Null Hypothesis H)
;;;  p(D|h) / p(D|H)


;; k = p(D|h) / p(D|H)
;; k = 1 / 0.5^n

;; after 3 answers:
;; k = 1 / 0.5^3
;; k = 8
;;         Apparently this is low / not convincing


;;;         A CHART!!!!?!?!

;;     k        |  Strength of evidence

;;   1 to 3     |    don't even bother
;;   3 to 20    |    vaguely positive
;;  20 to 150   |    pretty strong
;;    >150      |    very strong

;; so if k = 8, we should be curious but definitely unconvinced.


;;;; Priors!

;;; Prior is how likely we thought h (the tested hypothesis)
;;; would be going into the experiment.

;;; Prior odds = O(h) = p(h)/p(H)

;;; Refined Bayes Factor takes account of Priors (i.e. what we thought coming in):
;;; k = O(h | D)
;;; k = O(h) * (p(D|h) / p(D|H))

;; let's assume O(h)(for the skeptic) = 1/1000000
;; i.e. unlikely to ever accept the hypothesis h

;; so after 5 correct answers:
;; k = O(h) * (p(D|h) / p(D|H))
;; k = 1/1000000 * (1 / 0.5^5)
;; k = 0.000032
;;               ...SO UNCONVINCED.



;;;; ALL THAT IN REVERSE!
;;; (inferring a prior based on evidence and k value)

;; after 7 correct answers:
;; assume k(for the believer) = 150 (very strong belief in evidence)

;;    k = O(h) * (p(D|h) / p(D|H))
;;  150 = O(h) * (p(D_7|h) / p(D_7|H))
;;  150 = O(h) * (1 / 0.5^7)
;;  150 = O(h) * 128
;; O(h) = 150/128
;; O(h) = 1.17 (ish)
;; i.e. slightly more likely than not to accept the hypothesis h


;;;;; More with Bayes Factor
;;;; The Boight-Kampff Test (from Blade Runner)

;;; Hypothesis A : ha
;;; subject is a replicant

;;; Hypothesis B : hb
;;; subject is a human


;;; some Prior Information : X

;;; Humans show unintentional response to target questions with 0.8 probability
;;; Replicants only 0.01

;;; so Xa = 0.01
;;; and Xb = 0.8


;; so given a positive response to stimulus
;; D_0 = positive
;; p(D | ha Xa) = 0.01
;; p(D | hb Xb) = 0.8

;; Bayes Factor : k
;; k = p(D | ha) / p(D | hb)
;; k = 0.01 / 0.8
;; k | D_0 = 0.0125

;;;; Evidence : e
;;; another refinement of Bayes Factor
;;; (hopefully produces a more intuitive number)
;;; e = 10 * log_10(k)

;; so given current Data
;; e = log_10(0.0125) * 10
;; e = -19


;; Some Sample Questions and their relative weight

;;    p(resp|  | p(resp|  | response | no-response
;;      human) |   droid) |   value  |   value

;; q1| 0.95    | 0.01     | -20      | 13
;; q2| 0.98    | 0.003    | -25      | 17
;; q3| 0.9     | 0.0012   | -29      | 10
;; q4| 0.99    | 0.001    | -30      | 20
;; q5| 0.999   | 0.05     | -13      | 30

;; where values are added to e


;;;; Where did those values come from?
;;;; Evidence e
;;; e = log_10(p(h1|X) / p(h1|X)) * 10

;;; for q1:
;;; what if no response?
;;; p(no-response | ha[ha = droid]) = 1 - p(resp | droid) = 0.99
;;; p(no-response | hb[human]) = 1 - p(resp | human) = 0.05

;;; e for that data =
;;; e = log_10(p(h1|X) / p(h1|X)) * 10
;;; e = log_10(0.99 / 0.05) * 10
;;; e = log_10(19.8) * 10
;;; e = 13


;; Assume Prior information X
;; tells us that 1/10 subjects
;; are Replicants

;; so starting e = log_10(p(ha | X) / p(hb | X)) * 10
;; e = log_10(1/10 / 9/10) * 10
;; e = log_10(1/9) * 10
;; e =~ -10 (-9.5 rounded to whole number)

;; for this case, positive e means we believe in ha (subject is a droid)
;; conversely, negative e means we believe in hb (subject is human)

;;; e is additive, so new evidence can just be added to existing total


;; Let's say we ask the questions:

;; Q | Resp | val | new total (current belief)

;; 0 | n/a  | n/a | -10       (this is where we started)
;; 1 | no   |  13 |  3
;; 2 | no   |  17 |  20
;; 3 | yes  | -29 | -9
;; 4 | yes  | -30 | -39
;; 5 | yes  | -13 | -52

;; final e = -52, pretty sure subject is human
;; might be a little sociopathic


;; Different subject:

;; Q | Resp | val | new total (current belief)

;; 0 | n/a  | n/a | -10       (this is where we started)
;; 1 | no   |  13 |  3
;; 2 | no   |  17 |  20
;; 3 | no   |  10 |  30
;; 4 | no   |  20 |  50
;; 5 | no   |  30 |  80

;; final e = 80, pretty strong.
;; But not enough example questions given to warrant blowing someone away.
;; Maybe put a round in the leg instead, just to be safe.



;;;;; Distributions, discrete and scandalous
;;;; Discrete
;;; Binomial Distribution
;;; Distribution over binary (win/lose) events:
;;;   flip coin, score goal, born with six fingers, etc

;;; given the following variables:
;;; probability of success : p

;;; P(getting k wins in n trials) =
;;; (n choose k) * p^k * (1 - p)^(n - k)
(defun probability-mass-function (trials wins p-win)
  (* (choose trials wins)
     (expt p-win wins)
     (expt (- 1 p-win)
           (- trials wins))))

(defun probability-mass-list-function (trials win-values p-win)
  (loop for i in win-values
     sum (probability-mass-function trials i p-win)))

(defun probability-mass-range-function (trials min-wins max-wins p-win)
  "Note that this is inclusive of both bounds."
  (loop for i from min-wins to max-wins
     sum (probability-mass-function trials i p-win)))

;;; IN WORDS
;;; P(getting k wins in n trials) =

;;; number of ways to get k wins in n trials
;;;   times
;;; likelihood of any given arrangement of k wins and n-k losses

;;; probability mass function is additive;
;;; to use it on multiple k values, just perform it on each and sum the results


;;;; Continuous
;;;; (What if we knew n and k, but we didn't know p?)
;;; impossible to find accurate p using discrete model (what if p is a continuous decimal?)
;;; if we know n and k but not p, we can use...

;;;;; Beta Distribution

;; p(win-rate | wins/losses) = Beta(\alpha, \beta)
;; probability of the win-rate given the observed wins/losses is equal to
;; beta distribution over alpha and beta, where
;;    alpha = number of wins observed
;;    beta  = number of losses observed

;;; Beta(a, b) = (x^(a - 1) * (1 - x)^(b - 1)) / B(a, b)
;;;      where B(a, b) represents the Beta Function

;;; also known as the Probability Density Function (PDF)

;;; Probability of an exact value within this pdf is 0
;;; Probability of a range of values is equal
;;; to the definite integral of our beta distribution

;;; P(a < p < b) = integral a b of Beta(n, k)



;;;; SIDEQUEST! Integrals and Derivatives!

;;; Rate of Change
;;; given a function f(x)
;;;  compute average rate of change of f(x) at x = a

;;; A.R.C. = Delta f(x) / Delta x
;;;        = (f(x) - f(a)) / (x - a)

;;; Cycle through values getting closer and closer to a from both sides
;;; and continually re-calculate A.R.C. until we can infer real Rate of Change

;;; (possibly retry a + and - 10 ^ decreasing orders
;;; of magnitude until we start seeing 000001 type results
;;; then add/subtract 000001 and return? Maybe return the +/- average?

;;; I'll come back to this later.

