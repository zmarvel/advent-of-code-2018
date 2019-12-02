;; During the second Go / No Go poll, the Elf in charge of the Rocket
;; Equation Double-Checker stops the launch sequence. Apparently, you
;; forgot to include additional fuel for the fuel you just added.

;; Fuel itself requires fuel just like a module - take its mass,
;; divide by three, round down, and subtract 2. However, that fuel also
;; requires fuel, and that fuel requires fuel, and so on. Any mass that
;; would require negative fuel should instead be treated as if it
;; requires zero fuel; the remaining mass, if any, is instead handled by
;; wishing really hard, which has no mass and is outside the scope of
;; this calculation.
;;
;; So, for each module mass, calculate its fuel and add it to the
;; total. Then, treat the fuel amount you just calculated as the input
;; mass and repeat the process, continuing until a fuel requirement is
;; zero or negative. For example:
;;
;; - A module of mass 14 requires 2 fuel. This fuel requires no
;;   further fuel (2 divided by 3 and rounded down is 0, which would call
;;   for a negative fuel), so the total fuel required is still just 2.
;; - At first, a module of mass 1969 requires 654 fuel. Then, this
;;   fuel requires 216 more fuel (654 / 3 - 2). 216 then requires 70 more
;;   fuel, which requires 21 fuel, which requires 5 fuel, which requires no
;;   further fuel. So, the total fuel required for a module of mass 1969 is
;;   654 + 216 + 70 + 21 + 5 = 966.
;; - The fuel required by a module of mass 100756 and its fuel is:
;;   33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2 = 50346.
;;
;; What is the sum of the fuel requirements for all of the modules on
;; your spacecraft when also taking into account the mass of the added
;; fuel? (Calculate the fuel requirements for each module separately,
;; then add them all up at the end.)
;;
;;
;; This program can be run like this:
;; $ cat 1.input | guile 1b.scm

(use-modules (ice-9 binary-ports)
	     (ice-9 rdelim)
	     (ice-9 match)
	     (ice-9 format)
	     (srfi srfi-1) ;; for fold, map, etc
	     (srfi srfi-11) ;; for (let-values ...)
	     ((rnrs) :version (6)) ;; for assert
	     )

(define (calc-fuel-required mass)
  (- (floor/ mass 3) 2))

(define (calc-fuel-for-fuel mass)
  (let ((mass' (calc-fuel-required mass)))
    (if (<= mass' 0)
	0
	(+ mass' (calc-fuel-for-fuel mass')))))

(define (calc-total-fuel masses)
  (fold (lambda (m acc)
	  (+ (let ((fuel (calc-fuel-required m)))
	       (+ fuel (calc-fuel-for-fuel fuel)))
	     acc))
	0 masses))

(assert (eq? (calc-total-fuel '(14)) 2))
(assert (eq? (calc-total-fuel '(1969)) 966))
(assert (eq? (calc-total-fuel '(100756)) 50346))

(define (read-masses)
  (let read-loop ((masses '()))
    (let ((line (read-line (current-input-port))))
      (if (eof-object? line)
	  masses
	  (read-loop
	   (cons (string->number
		  (string-trim-right line))
		 masses))))))

(let ((masses (read-masses)))
  (display (calc-total-fuel masses)) (newline))
