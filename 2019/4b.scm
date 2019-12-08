;; --- Part Two ---
;; 
;; An Elf just remembered one more important detail: the two adjacent
;; matching digits are not part of a larger group of matching digits.
;; 
;; Given this additional criterion, but still ignoring the range rule,
;; the following are now true:
;; 
;;     - 112233 meets these criteria because the digits never decrease and
;;       all repeated digits are exactly two digits long.
;;     - 123444 no longer meets the criteria (the repeated 44 is part of
;;       a larger group of 444).
;;     - 111122 meets the criteria (even though 1 is repeated more than
;;       twice, it still contains a double 22).
;; 
;; How many different passwords within the range given in your puzzle
;; input meet all of the criteria?


(use-modules (ice-9 match)
	     (ice-9 format)
	     (srfi srfi-1) ;; for fold, map, etc
	     (srfi srfi-11) ;; for (let-values ...)
	     ((rnrs) :version (6)) ;; for assert
	     )

(define input-range '(138241 . 674034))

(define (split-digits n)
  ;; Doesn't work for 0, but oh well
  (let loop ((digits '())
	     (n n))
    (if (zero? n)
	digits
	(let-values (((quotient remainder) (floor/ n 10)))
	  (loop (cons remainder digits) quotient)))))

(assert (equal? (split-digits 1234) '(1 2 3 4)))

;; (define (has-double? digits)
;;   ;; Assumes at least 2 digits were provided.
;;   (let loop ((last (car digits))
;; 	     (digits (cdr digits)))
;;     (if (null? digits)
;; 	#f
;; 	(let ((curr (car digits)))
;; 	  (or (= last curr)
;; 	      (loop curr (cdr digits)))))))

;; (assert (has-double? '(1 2 2 1)))
;; (assert (has-double? '(1 2 2 1 1)))
;; (assert (has-double? '(2 2 2)))
;; (assert (has-double? '(1 2 2 2 1)))

(define (group-adjacent digits)
  (let loop ((last (car digits))
	     (digits (cdr digits))
	     (groups (list (list (car digits)))))
    (if (null? digits)
	groups
	(let ((curr (car digits))
	      (group (car groups)))
	  (if (= last curr)
	      ;; If the current digit is part of the group, modify add
	      ;; it to the group at the beginning of the groups list
	      (loop curr (cdr digits) (cons (cons curr group) (cdr groups)))
	      ;; Otherwise, create a new group and add it to the front
	      ;; of the groups list
	      (loop curr (cdr digits) (cons (list curr) groups)))))))

(assert (equal? (group-adjacent '(1 2 2 1)) '((1) (2 2) (1))))
(assert (equal? (group-adjacent '(1 2 1)) '((1) (2) (1))))
(assert (equal? (group-adjacent '(2 2 2)) '((2 2 2))))
(assert (equal? (group-adjacent '(1 2 2 2 1)) '((1) (2 2 2) (1))))

(define (has-exact-double? digits)
  ;; Does the list of digits satisfy the requirement that exactly two
  ;; adjacent digits are the same? Some examples:
  ;; - '(1 1) = #t
  ;; - '(1 1 1) = #f
  ;; - '(1 1 1 2 2) = #t
  (let ((group-sizes (map length (group-adjacent digits))))
    (fold (lambda (size acc)
	    (or (= size 2) acc))
	  #f
	  group-sizes)))

(define (monotonically-increasing? digits)
  (let loop ((last (car digits))
	     (digits (cdr digits)))
    (if (null? digits)
	#t
	(let ((curr (car digits)))
	  (and (<= last curr)
	      (loop curr (cdr digits)))))))

(assert (monotonically-increasing? '(1 2 2 2 3)))
(assert (not (monotonically-increasing? '(1 2 2 2 1))))

(define (valid-password? n)
  (let ((digits (split-digits n)))
    (and (has-exact-double? digits) (monotonically-increasing? digits))))

(let ((end (cdr input-range)))
  (let loop ((i (car input-range))
	     (count 0))
    (if (> i end)
	(begin
	  (display count)
	  (newline))
	(loop (1+ i)
	      (if (valid-password? i)
		  (1+ count)
		  count)))))
