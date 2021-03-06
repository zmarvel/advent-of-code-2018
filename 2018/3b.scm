

;; Thoughts: I think some kind of topological sort could make this
;; work without going over each cell of each "claim." That is, each
;; overlapping claim should be adjacent.

(use-modules (ice-9 binary-ports)
	     (ice-9 rdelim)
	     (ice-9 match))

(define (read-claims port claims)
  (let ((line (read-line port)))
    (if (eof-object? line)
	claims
	(read-claims port (cons
			(string-trim-right line)
			claims)))))

(define (split-claim claim)
  (match-let* ((chunks (string-split claim #\space))
	       (number (string->number
			(string-trim (list-ref chunks 0) #\#)))
	       ((x y) (map string->number
			   (string-split (string-trim-right
				     (list-ref chunks 2) #\:) #\,)))
	       ((w h) (map string->number
			   (string-split (list-ref chunks 3) #\x))))
	      ;; (format #t "~a ~a,~a ~ax~a\n"
	      ;; 	      number x y w h)
	      `(,number ,x ,y ,w ,h)))

(define (find-extents claims minx miny maxx maxy)
  (if (null? claims)
      `(,minx ,miny ,maxx ,maxy)
      (let ((claim (car claims)))
	(match-let (((n x y w h) claim))
		   (find-extents (cdr claims)
				 (min minx x) (min miny y)
				 (max maxx (+ x w)) (max maxy (+ y h)))))))

;; 998x1000

(define (count-overlaps claims arr)
  (if (null? claims)
      arr
      (begin
	(match-let (((n x y w h) (car claims)))
		   (let loop ((x' x)
			      (y' y))
		     (if (< y' (+ y h))
			 (if (< x' (+ x w))
			     (begin
			       (array-set!
				arr (1+ (array-ref arr x' y')) x' y')
			       (loop (1+ x') y'))
			     (loop x (1+ y')))
			 arr)))
	(count-overlaps (cdr claims) arr))))

(define overlapped 0)
(define (find-no-overlaps claims arr)
  ;; Find the claim which no other claim overlaps
  (if (null? claims)
      #f
      (or (match-let (((n x y w h) (car claims)))
		     
		     (let loop ((x' x)
				(y' y))
		       (if (< y' (+ y h))
			   (if (< x' (+ x w))
			       (and (<= (array-ref arr x' y') 1)
				    (loop (1+ x') y'))
			       ;; (if (> (array-ref arr x' y') 1)
			       ;; 	   (begin (set! overlapped (1+ overlapped))
			       ;; 		  (format #t "~a\t~a\t~a\t~a\n" x y w h)
			       ;; 		  #f)
			       ;; 	   (and #t (loop (1+ x') y')))
			       (loop x (1+ y')))
			   (car claims))))
	  (find-no-overlaps (cdr claims) arr))))

(let* ((arr (make-array 0 1000 1000))
       (claims (map split-claim
		    (read-claims (current-input-port) '())))
       (counts (count-overlaps claims arr)))
  (display (find-no-overlaps claims arr)) (newline)
  (display overlapped) (newline))
