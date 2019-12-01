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

(define (point-x<=? p1 p2)
  (match-let (((x1 . y1) p1)
	      ((x2 . y2) p2))
	     (<= x1 x2)))

(define (point-y<=? p1 p2)
  (match-let (((x1 . y1) p1)
	      ((x2 . y2) p2))
	     (<= y1 y2)))

(define (point-x>=? p1 p2)
  (match-let (((x1 . y1) p1)
	      ((x2 . y2) p2))
	     (>= x1 x2)))

(define (point-y>=? p1 p2)
  (match-let (((x1 . y1) p1)
	      ((x2 . y2) p2))
	     (>= y1 y2)))

(define (failure-msg loc case actual expected)
  (match-let ((`((line . ,line) (column . ,column) (filename . ,filename)) loc))
	     (format #t "~a:~a:\n\t~a: got ~a, expected ~a\n"
		     line column case actual expected)))

(define (point-xmin p1 p2)
  (if (point-x<=? p1 p2) p1 p2))

(define (point-ymin p1 p2)
  (if (point-y<=? p1 p2) p1 p2))

(define (point-xmax p1 p2)
  (if (point-x>=? p1 p2) p1 p2))

(define (point-ymax p1 p2)
  (if (point-y>=? p1 p2) p1 p2))

(define (find-bounds points minx maxx miny maxy)
  ;; Given an empty list, returns the original provided values for everything.
  ;; Otherwise, return (min-x max-x min-y max-y).
  (if (null? points)
      (list minx maxx miny maxy)
      (match-let (((x . y) (car points)))
		 (find-bounds (cdr points)
			      (min x minx)
			      (max x maxx)
			      (min y miny)
			      (max y maxy)))))

(define (find-corners points)
  ;; Find the boundaries of the grid.
  ;; Returns (top-left top-right bottom-left bottom-right)
  (match-let* (((x1 . y1) (car points)) ;; seed
	       ((minx maxx miny maxy) (find-bounds points x1 x1 y1 y1)))
	      ;; width = maxx
	      ;; height = maxy
	      (list (cons minx miny)
		    (cons maxx miny)
		    (cons minx maxy)
		    (cons maxx maxy))))

(define (find-dims points)
  (match-let* (((x1 . y1) (car points)) ;; seed
	       ((minx maxx miny maxy) (find-bounds points x1 x1 y1 y1)))
	      (cons (1+ maxx) (1+ maxy))))

(define (make-closest-array w h)
  (make-array -1 w h))

(define (make-distance-array w h)
  (make-array -1 w h))

(define (display-array w h arr)
  (let loop ((row 0)
	     (col 0))
    (if (< row h)
	(if (< col w)
	    (begin
	      (format #t "~2d " (array-ref arr col row))
	      (loop row (1+ col)))
	    (begin
	      (newline)
	      (loop (1+ row) 0))
	    ))))

(define (point-id->char id)
  (if (= id -2)
      #\.
      (integer->char (+ 97 id))))

(define (display-board w h arr)
  (let loop ((row 0)
	     (col 0))
    (if (< row h)
	(if (< col w)
	    (begin
	      (format #t "~c  " (point-id->char (array-ref arr col row)))
	      (loop row (1+ col)))
	    (begin
	      (newline)
	      (loop (1+ row) 0))
	    ))))

(define (init-board w h points)
  (let ((board (make-array #\. w h))
	(ntiles (* w h)))
    (let loop ((points' points)
	       (i 0))
      (if (null? points')
	  board
	  (begin
	    (match-let (((x . y) (car points')))
	      (array-set! board i x y))
	    (loop (cdr points') (1+ i)))))))

(define (test fn tests)
  ;; tests is a list of (expected . args), and compares
  ;; (apply fn args) to expected.
  (for-each (lambda (test)
	      (let ((expected (car test))
		    (args (cdr test)))
		(let ((actual (apply fn args)))
		  (unless (equal? actual expected)
		    (format #t "~a ~a: got ~a, expected ~a\n"
			    fn test actual expected)))))
	    tests))

(define (point-inside? w h point)
  (match-let (((x . y) point))
    (and (< x w) (>= x 0)
	 (< y h) (>= y 0))))

(let ((w 9)
      (h 10))
  (let ((test-fn (lambda (case)
		   (point-inside? w h case))))
    (test test-fn '((#t (1 . 1))
		    (#t (0 . 1))
		    (#t (0 . 9))
		    (#t (8 . 9))
		    (#f (-1 . 9))
		    (#f (8 . -1))
		    (#f (0 . 10))
		    (#f (9 . 8))
		    ))))

(define (point-borders? dist center point)
  (match-let (((x . y) point)
	      ((cx . cy) center))
    (or (= (abs (- cx x)) dist)
	(= (abs (- cy y)) dist))))

(let ((w 9)
      (h 10))
  (test point-borders? '((#t . (0 (2 . 2) (2 . 2)))
			 (#f . (0 (2 . 2) (1 . 1)))
			 (#t . (1 (2 . 2) (1 . 1)))
			 (#t . (2 (2 . 2) (0 . 0)))
			 (#t . (2 (2 . 2) (2 . 0)))
			 (#t . (2 (2 . 2) (3 . 0)))
			 (#t . (2 (2 . 2) (4 . 0)))
			 (#t . (2 (2 . 2) (2 . 4)))
			 (#t . (2 (2 . 2) (4 . 4)))
			 (#t . (2 (2 . 2) (4 . 3)))
			 (#f . (2 (2 . 2) (3 . 3)))
			 )))

(define (manhattan-distance point other)
  (match-let (((px . py) point)
	      ((ox . oy) other))
    ;; start at point and go to other
    (let loop ((x' px) (y' py))
      (cond
       ;; go left
       ((> x' ox) (1+ (loop (1- x') y')))
       ;; go right
       ((< x' ox) (1+ (loop (1+ x') y')))
       ;; go up
       ((< y' oy) (1+ (loop x' (1+ y'))))
       ;; go down
       ((> y' oy) (1+ (loop x' (1- y'))))
       (else 0)))))

(define (point-id points x y)
  ;; effectively, what is the position of (x . y) in the list points?
  ;; assumes (x . y) *does* exist in points.
    (match-let (((ox . oy) (car points)))
      (if (and (= ox x) (= oy y))
	  0
	  (1+ (point-id (cdr points) x y)))))

(define (find-distance board distance w h points)
  (begin
    (for-each (lambda (point)
		(match-let (((x . y) point))
		  (array-set! board (point-id points x y) x y)
		  (array-set! distance 0 x y)))
	      points)
    (let loop ((i 1) ;; distance from point
	       (seen (length points)))
      ;; (format #t "loop: i:~a seen:~a\n" i seen)
      (if (< seen (* w h))
	  (loop
	   (1+ i)
	   (fold (lambda (point seen')
		   (match-let (((x . y) point))
		     (let loopx ((x' (- x i)))
		       (if (<= x' (+ x i))
			   (let* ((dy (- i (abs (- x' x))))
				  (y-up (- y dy))
				  (y-down (+ y dy))
				  (fn
				   (lambda (y')
				     (let* ((point' (cons x' y'))
					    (m-dist (manhattan-distance point point')))
				       ;; (format #t "~a,~a\n" x' y')
				       (cond
					((point-inside? w h point')
					 (cond
					  ;; the cell has already been seen and is
					  ;; closer to another point--do nothing
					  ;; (< (array-ref distance x' y') i) but
					  ;; (not (= (array-ref distance x' y') -1))
					  ((> (array-ref board x' y') i)
					   0)
					  ;; the cell is unvisited
					  ((= (array-ref board x' y') -1)
					   (begin
					     (array-set! board (point-id points x y) x' y')
					     ;; (array-set! distance i x' y')
					     (array-set! distance m-dist x' y')
					     ;; (format #t "first saw ~a,~a\n" x' y')
					     1))
					  ;; like case 1, but we have to check for -1 first.
					  ((< (array-ref distance x' y') m-dist)
					   0)
					  ;; point' has been visited, and it is
					  ;; equidistant to another point
					  ((and (= (array-ref distance x' y') m-dist)
						(not (= (array-ref board x' y') (point-id points x y))))
					   (begin
					     (array-set! board -2 x' y')
					     0))
					  ;; the cell is closer to point' than another
					  ;; point that populated distance first--
					  ;; should never happen?
					  ((> (array-ref distance x' y') m-dist)
					   (begin
					     (display "unexpected case") (newline)
					     (array-set! board (point-id points x y) x' y')
					     ;; (array-set! distance i x' y')
					     (array-set! distance m-dist x' y')
					     0))
					  (else 0)
					  ;; (else
					  ;; 	(format #t "else: point':~a,~a i:~a board:~a distance:~a\n"
					  ;; 		x' y' i (array-ref board x' y') (array-ref distance x' y'))
					  ;; 	)
					  ))
					(else 0)))
				     )
				   ))
			     (+ (fn y-up) (fn y-down) (loopx (1+ x')))
			     )
			   ;; (let loopy ((y' (- y (- i (abs (- x' x))))))
			   ;;   (if (<= y' (+ y (- i (abs (- x' x)))))
			   ;; 	 (let* ((point' (cons x' y'))
			   ;; 		(m-dist (manhattan-distance point point')))
			   ;; 	   (cond
			   ;; 	    ((and (point-inside? w h point')
			   ;; 		  (point-borders? i point point'))
			   ;; 	     (cond
			   ;; 	      ;; the cell has already been seen and is
			   ;; 	      ;; closer to another point--do nothing
			   ;; 	      ;; (< (array-ref distance x' y') i) but
			   ;; 	      ;; (not (= (array-ref distance x' y') -1))
			   ;; 	      ((> (array-ref board x' y') i)
			   ;; 	       (loopy (1+ y')))
			   ;; 	      ;; the cell is unvisited
			   ;; 	      ((= (array-ref board x' y') -1)
			   ;; 	       (begin
			   ;; 		 (array-set! board (point-id points x y) x' y')
			   ;; 		 ;; (array-set! distance i x' y')
			   ;; 		 (array-set! distance m-dist x' y')
			   ;; 		 ;; (format #t "first saw ~a,~a\n" x' y')
			   ;; 		 (1+ (loopy (1+ y')))))
			   ;; 	      ;; like case 1, but we have to check for -1 first.
			   ;; 	      ((< (array-ref distance x' y') m-dist)
			   ;; 	       (loopy (1+ y')))
			   ;; 	      ;; point' has been visited, and it is
			   ;; 	      ;; equidistant to another point
			   ;; 	      ((= (array-ref distance x' y') m-dist)
			   ;; 	       (begin
			   ;; 		 (array-set! board -2 x' y')
			   ;; 		 (loopy (1+ y'))))
			   ;; 	      ;; the cell is closer to point' than another
			   ;; 	      ;; point that populated distance first--
			   ;; 	      ;; should never happen?
			   ;; 	      ((> (array-ref distance x' y') m-dist)
			   ;; 	       (begin
			   ;; 		 (display "unexpected case") (newline)
			   ;; 		 (array-set! board (point-id points x y) x' y')
			   ;; 		 ;; (array-set! distance i x' y')
			   ;; 		 (array-set! distance m-dist x' y')
			   ;; 		 (loopy (1+ y'))))
			   ;; 	      ;; (else
			   ;; 	      ;; 	(format #t "else: point':~a,~a i:~a board:~a distance:~a\n"
			   ;; 	      ;; 		x' y' i (array-ref board x' y') (array-ref distance x' y'))
			   ;; 	      ;; 	)
			   ;; 	      ))
			   ;; 	    (else (loopy (1+ y')))))
			   ;; 	 (loopx (1+ x'))))
			   seen'))))
		 seen
		 points))
	  board))))

(define (vector-1+! vec i)
  (vector-set! vec i (1+ (vector-ref vec i))))

(define (point-border? w h x y)
  ;; is the point on the border of the board?
  (or (= x 0) (= y 0)
      (= x (1- w)) (= y (1- h))))

(define (count-area board w h counts)
  ;; counts should be a vector where the index is a point-id and
  ;; the value at that index is a count
  (let loop ((x 0)
	     (y 0))
    (if (< y h)
	(if (< x w)
	    ;; if the point is on the edge of the board, indicate
	    ;; that its area is "infinity," or -1
	    (begin
	      (if (point-border? w h x y)
		(vector-set! counts (array-ref board x y) -1)
		(vector-1+! counts (array-ref board x y)))
	      (loop (1+ x) y))
	    (loop 0 (1+ y)))
	counts)))

(define (vector-max vec len)
  (let loop ((i 0))
    (if (< i len)
	(max (vector-ref vec i) (loop (1+ i)))
	0)))

(define (draw-board w h points)
  (let ((board (init-board w h points)))
    (display-array w h board)))

(define (parse-coords line)
  (let* ((parts (string-split (string-trim line) #\space))
	 (x (string->number (string-trim-right (car parts) #\,)))
	 (y (string->number (cadr parts))))
    (cons x y)))

(let* ((points (map parse-coords (read-records (current-input-port))))
       (dims (find-dims points))
       (w (car dims))
       (h (cdr dims))
       (board (make-closest-array w h))
       (distance (make-distance-array w h))
       )
  (format #t "dims: ~ax~a\n" w h)
  (let* ((board' (find-distance board distance w h points))
	 (n (length points))
	 (counts (count-area board w h (make-vector n 0))))
    (display (vector-max counts n)) (newline)
    ;; (display-board w h board')
    ;; (newline)
    ;; (display-array w h distance)

    ))
