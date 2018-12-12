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
  (if (or (null? points) (null? (cdr points)))
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
	      (cons maxx maxy)))

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
	      (display (array-ref arr col row))
	      (loop row (1+ col)))
	    (begin
	      (newline)
	      (loop (1+ row) 0))
	    ))))

(define (draw-board w h points)
  (let ((board (make-array #\. w h))
	(ntiles (* w h)))
    (let loop ((points' points)
	       (i 0))
      (unless (null? points')
	(begin
	  (match-let (((x . y) (car points')))
		     (array-set! board i x y))
	  (loop (cdr points') (1+ i)))))
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
       (closest (make-closest-array w h))
       (distance (make-distance-array w h))
       )
  (draw-board w h points)
   )
