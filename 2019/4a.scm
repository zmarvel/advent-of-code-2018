(use-modules (ice-9 match)
	     (ice-9 format)
	     ;; (srfi srfi-1) ;; for fold, map, etc
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

(define (has-double? digits)
  ;; Assumes at least 1 digit was provided.
  (let loop ((last (car digits))
	     (digits (cdr digits)))
    (if (null? digits)
	#f
	(let ((curr (car digits)))
	  (or (= last curr)
	      (loop curr (cdr digits)))))))

(assert (has-double? '(1 2 2 1)))

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
    (and (has-double? digits) (monotonically-increasing? digits))))

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
