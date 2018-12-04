

;; Given: A list of positive and negative numbers.
;;
;; Problem: Add the numbers together until we see a repeated sum. It
;; might be necessary to cycle through the list multiple times.
;;
;; Output: The sum we first see twice.
;;
;; Thoughts: For the most obvious solution (iterating over the list
;; until we see a sum twice), we need an efficient way to look through
;; the sums we've already seen. A hash table is easiest because it's
;; built in to Guile, but a binary tree (or some similar sorted tree)
;; would also be plenty fast. Searching through a list could be too
;; slow. A bitvector would also be pretty efficient, but it could take
;; up a lot of space (and be potentially sparse), since the maximum
;; and minimum sums are infinite.

(use-modules (ice-9 binary-ports)
	     (ice-9 rdelim)
	     (srfi srfi-1))


(define (read-nums port)
  (let ((line (read-line port)))
    (if (eof-object? line)
	'()
	(cons
	 (string->number (string-trim-right line))
	 (read-nums port)))))

(define (find-duplicate-sum nums sum sums i)
  (let ((sum' (+ (vector-ref nums i) sum)))
    (if (hashv-ref sums sum')
	sum' ;; found it!
	(begin
	  (hashv-set! sums sum' #t)
	  (find-duplicate-sum nums sum' sums
			      (remainder (1+ i) (vector-length nums)))))))

(let* ((nums (list->vector (read-nums (current-input-port)))))
  (display (find-duplicate-sum nums 0 (make-hash-table) 0)))
