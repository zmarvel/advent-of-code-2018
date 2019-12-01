
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

;; If there are two ids that differ by only the letter at index i,
;; return them as a pair. Otherwise, return #f.
(define (compare-ids dict id ids i len)
  (let* ((left (substring id 0 i))
	 (mid (string-ref id i))
	 (right (substring id (1+ i) len))
	 (pair `(,left . ,right))
	 (entry (hash-ref dict pair)))
    (if entry
	`(,left . ,right)
	(begin
	  (hash-set! dict pair mid)
	  (if (null? ids)
	      #f
	      (or #f (compare-ids dict (car ids) (cdr ids) i len)))))))

(define (search-ids ids n i len)
  (let* ((dict (make-hash-table n))
	 (found (compare-ids dict (car ids) (cdr ids) i len)))
    (if found
	found
	(search-ids ids n (1+ i) len))))


(let* ((ids (read-ids (current-input-port) '()))
       (n (length ids))
       (len (string-length (car ids)))
       (result (search-ids ids n 0 len)))
  (display result))
