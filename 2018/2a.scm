
;; Given a list of "box IDs," which are strings of letters, count how
;; many box IDs contain the same letter exactly two times, as well as
;; how many IDs contain the same letter exactly three times. Return
;; the product of these two counts.

(use-modules (ice-9 binary-ports)
	     (ice-9 rdelim)
	     (ice-9 match)
	     (srfi srfi-1)
	     (srfi srfi-11)
	     )


(define (read-ids port ids)
  (let ((line (read-line port)))
    (if (eof-object? line)
	ids
	(read-ids port (cons
			(string-trim-right line)
			ids)))))

;; Map each id to an a-list containing the counts of letters in the id
(define (count-letters ids)
  (map (lambda (id)
	 (cons id
	       (fold (lambda (letter alist)
		       (let ((pair (assv letter alist)))
			 (if pair
			     ;; if the letter is already in the alist,
			     ;; increment its count.
			     (match-let (((k . count) pair))
					(assv-set! alist letter (1+ count)))
			     ;; otherwise, add it with an initial count of 1
			     (assv-set! alist letter 1))))
		     '()
		     (string->list id))))
       ids))


;; Map each id to a pair:
;; - (0 . 0) - the id contains no letter exactly two or exactly three
;;   times.
;; - (1 . 0) - the id contains at least one letter exactly twice, and
;;   it contains no letter exactly three times.
;; - (0 . 1) - the id contains no letter exactly twice, and it
;;   contains at least one letter exactly three times.
;; - (1 . 1) - the id contains at least one letter exactly twice, and
;;   it contains at least one letter exactly three times.
(define (two-or-three counts-a-list)
  ;; expecting an a-list of (id . count-a-list)
  (map
   (lambda (id-counts)
     (match-let (((id . counts) id-counts))
		(cons
		 id
		 (fold
		  (lambda (letter-count twos-threes)
		    (match-let (((letter . count) letter-count)
				((twos . threes) twos-threes))
			       (cons (or (and (= count 2) 1) twos)
				     (or (and (= count 3) 1) threes))))
		       '(0 . 0)
		       counts))))
     counts-a-list))

;; Compute (sum1 . sum2) from a list of pairs of numbers.
(define (sum-twos-threes two-three-a-list)
  (fold (lambda (id-two-three totals)
	  (match-let* (((id . two-three) id-two-three)
		       ((two . three) two-three)
		       ((two-total . three-total) totals))
		       (cons (+ two two-total) (+ three three-total))))
	'(0 . 0)
	two-three-a-list))

(let* ((ids (read-ids (current-input-port) '()))
       (counts (count-letters ids))
       (twos-threes (two-or-three counts))
       (sums (sum-twos-threes twos-threes)))
  (match-let (((twos . threes) sums))
	     (display (* twos threes))))
