

;; Thoughts: Is the answer simply the average?

(use-modules (ice-9 binary-ports)
	     (ice-9 rdelim)
	     (ice-9 match)
	     (srfi srfi-26)
	     (srfi srfi-1)
	     (srfi srfi-11)
	     (srfi srfi-43))

(define (read-records port)
  (let ((line (read-line port)))
    (if (eof-object? line)
	'()
	(cons
	 (string-trim-right line)
	 (read-records port)))))

(define (sort-lines lines)
  (sort lines string<?))

(define (parse-record line)
  (let* ((datetime (substring line 1 17))
	 (event (substring line 19 (string-length line)))
	 (datetime-parts (string-split datetime #\space))
	 (date-parts (map string->number (string-split (car datetime-parts) #\-)))
	 (year (car date-parts))
	 (month (cadr date-parts))
	 (day (caddr date-parts))
	 (time-parts (map string->number (string-split (cadr datetime-parts) #\:)))
	 (hour (car time-parts))
	 (minute (cadr time-parts)))
    (list year month day hour minute event)))

(define (sort-records records)
  (sort records (lambda (l r)
		  (match-let (((lyear lmonth lday lhour lminute levent) l)
			      ((ryear rmonth rday rhour rminute revent) r))
			     (or (< lyear ryear)
				 (< lmonth rmonth)
				 (< lday rday)
				 (< lhour rhour)
				 (< lminute rminute))))))

;; shifts: a hash table of (guard-id => ((wakes-up . falls-asleep) ...))
;; ids: a list of guard-ids
(define (process-shifts records falls-asleep wakes-up guard-id shifts)
  (if (null? records)
      shifts
      (let ((record (car records)))
	(match-let (((year month day hour minute event) record))
		   (cond
		    ((string-prefix? "Guard" event)
		     (let ((guard-id' (string->number
				       (string-drop
					(cadr (string-split event #\space))
					1))))
		       (process-shifts (cdr records)
				       #f ;; invalid falls-asleep
				       #f ;; and wakes-up
				       guard-id'
				       shifts)))
		    ((string-prefix? "falls" event)
		     (process-shifts (cdr records)
				     minute
				     wakes-up
				     guard-id
				     shifts))
		    ((string-prefix? "wakes" event)
		     (let ((wakes-up' minute))
		       (process-shifts (cdr records)
				       falls-asleep
				       wakes-up'
				       guard-id
				       (let ((guard-shifts (hash-ref shifts guard-id '())))
					 (hash-set! shifts guard-id
						    (cons
						     (cons falls-asleep wakes-up')
						     guard-shifts))
					 shifts))))
		    )))))

(define (find-sleepiest shifts)
  (let ((totals (hash-fold
		 (lambda (id sleeps sums)
		   (cons
		    (cons id
			  (fold (lambda (sleep total)
				  (let ((falls-asleep (car sleep))
					(wakes-up (cdr sleep)))
				    (+ total (- wakes-up falls-asleep))))
				0
				sleeps))
		    sums))
		 '()
		 shifts
		 )))
    (fold (lambda (id-total id-sleepiest)
	    (let ((id (car id-total))
		  (maybe-total (cdr id-total))
		  (curr-total (cdr id-sleepiest)))
	      (if (> maybe-total curr-total)
		  id-total
		  id-sleepiest)))
	  '(0 . 0)
	  totals)))

(define (find-sleepiest-minute shifts guard-id minutes)
  (let ((guard-sleeps (hash-ref shifts guard-id)))
    (for-each (lambda (shift)
		(let ((falls-asleep (car shift))
		      (wakes-up (cdr shift)))
		  (let loop ((i falls-asleep))
		    (if (< i wakes-up)
			(begin
			  (vector-set! minutes i (1+ (vector-ref minutes i)))
			  (loop (1+ i)))
			(vector-ref minutes i)))))
	      guard-sleeps)
    (vector-fold (lambda (i i-max elt)
		   (let ((minute (car i-max))
			 (max-duration (cdr i-max)))
		     (if (> elt max-duration)
			 (cons i elt)
			 i-max)))
		 '(0 . 0)
		 minutes)))

(define print-list
  (cute for-each (lambda (elt) (display elt) (newline)) <>))

(define (print-hash-table htab)
  (hash-for-each (cute format #t "~a -> ~a\n" <> <>) htab))

(let ((records (map parse-record (sort-lines (read-records (current-input-port))))))
    (print-list records)
  (let* ((shifts (process-shifts records #f #f #f (make-hash-table)))
	 (guard-ids (hash-fold (lambda (id sleeps ls) (cons id ls)) '() shifts))
	 (sleepiest (find-sleepiest shifts))
	 (sleepiest-id (car sleepiest))
	 (sleepiest-minute (car (find-sleepiest-minute shifts sleepiest-id (make-vector 60 0))))
	 )
      ;;(print-hash-table shifts)
      ;;(print-list (vector->list minutes))
    ;; (display sleepiest) (newline)
    (format #t "~a @ ~a\n" sleepiest-id sleepiest-minute)
    (display (* sleepiest-id sleepiest-minute)) (newline)
    ))
