;; --- Part Two ---
;; 
;; It turns out that this circuit is very timing-sensitive; you actually
;; need to minimize the signal delay.
;; 
;; To do this, calculate the number of steps each wire takes to reach
;; each intersection; choose the intersection where the sum of both
;; wires' steps is lowest. If a wire visits a position on the grid
;; multiple times, use the steps value from the first time it visits that
;; position when calculating the total value of a specific intersection.
;; 
;; The number of steps a wire takes is the total number of grid squares
;; the wire has entered to get to that location, including the
;; intersection being considered. Again consider the example from above:
;; 
;; ...........
;; .+-----+...
;; .|.....|...
;; .|..+--X-+.
;; .|..|..|.|.
;; .|.-X--+.|.
;; .|..|....|.
;; .|.......|.
;; .o-------+.
;; ...........
;; 
;; In the above example, the intersection closest to the central port is
;; reached after 8+5+5+2 = 20 steps by the first wire and 7+6+4+3 = 20
;; steps by the second wire for a total of 20+20 = 40 steps.
;; 
;; However, the top-right intersection is better: the first wire takes
;; only 8+5+2 = 15 and the second wire takes only 7+6+2 = 15, a total of
;; 15+15 = 30 steps.
;; 
;; Here are the best steps for the extra examples from above:
;; 
;;     - R75,D30,R83,U83,L12,D49,R71,U7,L72
;;       U62,R66,U55,R34,D71,R55,D58,R83 = 610 steps
;;     - R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
;;       U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = 410 steps
;; 
;; What is the fewest combined steps the wires must take to reach an
;; intersection?


(use-modules (ice-9 binary-ports)
	     (ice-9 rdelim)
	     (ice-9 match)
	     (ice-9 format)
	     ;; (srfi srfi-1) ;; for fold, map, etc
	     (srfi srfi-11) ;; for (let-values ...)
	     ((rnrs) :version (6)) ;; for assert
	     )

(define (calc-bounds directions)
  (let loop ((x 0)
	     (y 0)
	     (minx 0)
	     (maxx 0)
	     (miny 0)
	     (maxy 0)
	     (directions directions))
    (if (null? directions)
	`(,minx ,maxx ,miny . ,maxy)
	(let* ((dx (match (car directions)
		     (('left . n) (- n))
		     (('right . n) n)
		     (_ 0)))
	       (dy (match (car directions)
		     (('up . n) n)
		     (('down . n) (- n))
		     (_ 0)))
	       (x' (+ x dx))
	       (y' (+ y dy)))
	  ;; (format #t "x'=~a y'=~a\n" x' y')
	  (loop x' y'
		(min x' minx) (max x' maxx)
		(min y' miny) (max y' maxy)
		(cdr directions))))))

(define (calc-dims bounds)
  (match-let (((minx maxx miny . maxy) bounds))
    ;; (format #t "bounds=~a\n" bounds)
    (let ((width (1+ (- maxx minx)))
	  (height (1+ (- maxy miny))))
      (cons width height))))

(match-let (((w . h) (calc-dims '(-2 4 0 . 5))))
  (assert (= w 7))
  (assert (= h 6)))

(define (get-direction str)
  (match str
    (#\R 'right)
    (#\L 'left)
    (#\U 'up)
    (#\D 'down)))

(define (parse-directions str)
  (let ((parts (string-split str #\,)))
    (map
     (lambda (part)
       (let ((direction (get-direction (string-ref part 0)))
	     (magnitude (string->number (substring part 1))))
	 (cons direction magnitude)))
     parts)))

(define test1-directions
  '(((right . 8) (up . 5) (left . 5) (down . 3))
    ((up . 7) (right . 6) (down . 4) (left . 4))))

(define test-string1 "R8,U5,L5,D3")
(define test-string2 "U7,R6,D4,L4")

(assert (equal? (parse-directions test-string1) (car test1-directions)))
(assert (equal? (parse-directions test-string2) (cadr test1-directions)))

(define test-string3 "U4,R2,U1,L4")
(define test-string4 "R4,U2,L1,D1")

(define (calc-board-bounds bounds-ls)
  (let loop ((bounds-ls bounds-ls)
	     (minx 0)
	     (maxx 0)
	     (miny 0)
	     (maxy 0))
    (if (null? bounds-ls)
	`(,minx ,maxx ,miny . ,maxy)
	(match-let (((ominx omaxx ominy . omaxy) (car bounds-ls)))
	  (loop (cdr bounds-ls)
		(min minx ominx) (max maxx omaxx)
		(min miny ominy) (max maxy omaxy))))))

(define (calc-board-dims bounds)
  (match-let (((minx maxx miny . maxy) bounds))
    (cons (1+ (- maxx minx)) (1+ (- maxy miny)))))

(let* ((path1 (parse-directions test-string3))
       (path2 (parse-directions test-string4))
       (bounds1 (calc-bounds path1))
       (bounds2 (calc-bounds path2))
       (dims1 (calc-dims bounds1))
       (dims2 (calc-dims bounds2)))
  (match-let (((w . h) dims1))
    ;; (format #t "bounds1 ~a\n" bounds1)
    ;; (format #t "dims1 ~a\n" dims1)
    (assert (= w 5))
    (assert (= h 6)))
  (match-let (((w . h) dims2))
    ;; (format #t "bounds2 ~a\n" bounds2)
    ;; (format #t "dims2 ~a\n" dims2)
    (assert (= w 5))
    (assert (= h 3)))
  (let ((board-dims (calc-board-dims (calc-board-bounds `(,bounds1 ,bounds2)))))
    (match-let (((w . h) board-dims))
      ;; (format #t "board-dims ~a\n" board-dims)
      (assert (= w 7))
      (assert (= h 6)))))

;; R8,U5,L5,D3
(let ((path (car test1-directions)))
  (match-let ((`(,width . ,height) (calc-dims (calc-bounds path))))
    ;; (format #t "width=~a height=~a\n" width height)
    (assert (eq? width 9))
    (assert (eq? height 6))))

;; U7,R6,D4,L4
(let ((path (cadr test1-directions)))
  (match-let ((`(,width . ,height) (calc-dims (calc-bounds path))))
    ;; (display (format #f "width=~a height=~a\n" width height))
    (assert (eq? width 7))
    (assert (eq? height 8))))

(define (calc-origin bounds)
  (match-let (((minx maxx miny . maxy) bounds))
    (cons (abs minx) (abs miny))))

(define (point->index origin point)
  (match-let (((ox . oy) origin)
	      ((px . py) point))
    (cons (+ px ox) (+ py oy))))

(define (index->point origin index)
  (match-let (((ox . oy) origin)
	      ((ix . iy) index))
    (cons (- ix ox) (- iy oy))))

(define (board-ref board origin point)
  (match-let (((x . y) (point->index origin point)))
    ;; (format #t "~a → ~a\n" point (cons x y))
    (vector-ref (vector-ref board x) y)))

(define (board-set! board origin point value)
  (match-let (((col . row) (point->index origin point)))
    (vector-set! (vector-ref board col) row value)))

(define (make-board w h origin)
  ;; Allocate the "board" (a 2D vector) and set the origin to -1.
  (let ((board (make-vector w 0)))
    (let x-loop ((x 0))
      (if (< x w)
	  (begin
	    (vector-set! board x (make-vector h 0))
	    (x-loop (1+ x)))))
    (board-set! board origin '(0 . 0) -1)
    board))

(define (draw-vector! board origin intersections distances vec id x y path-length)
  (match-let ((`(,direction . ,magnitude) vec))
    ;; (format #t "~a\n" vec)
    (let mag-loop ((i 0)
		   (x x)
		   (y y))
      (if (eq? i magnitude)
	  (values x y)
	  (let* ((dx (match direction
		       ('left -1)
		       ('right 1)
		       (_ 0)))
		 (dy (match direction
		       ('up 1)
		       ('down -1)
		       (_ 0)))
		 (point (cons x y))
		 (cell-value (board-ref board origin point)))
	    ;; If a vector has already been drawn here, mark it as an
	    ;; intersection
	    (begin
	      (cond
	       ;; This is the first path to visit the point
	       ((= cell-value 0) (begin
				   (board-set! board origin point id)
				   (board-set! distances origin point (+ path-length i))))
	       ;; This is the second path to visit the point--mark the
	       ;; intersection map accordingly
	       ((and (> cell-value 0)
		     (not (= cell-value id)))
		(board-set! intersections origin point
			    (+
			     (board-ref distances origin point)
			     (+ path-length i))))
	       ;; Otherwise, the path has crossed itself--do nothing
	       )
	      (mag-loop (1+ i) (+ x dx) (+ y dy))))))))

(define (populate-board! board origin intersections distances paths)
  (define (draw-path! path id x y length)
    ;; Draw a path--that is, a list of '((direction magnitude) ...)
    ;; vectors--and return the point '(x y) at the end of the path, even
    ;; though it shouldn't be needed for anything.
    (if (null? path)
	length
	(let ((vec (car path)))
	  ;; (format #t "path ~a\n" path)
	  (match-let (((direction . magnitude) vec))
	    (let-values (((x' y') (draw-vector! board origin intersections distances vec id x y length)))
	      (draw-path! (cdr path) id x' y' (+ length magnitude)))))))
  (let paths-loop ((paths paths)
		   (i 1))
    (if (not (null? paths))
	(let ((path (car paths)))
	  (draw-path! path i 0 0 0)
	  (paths-loop (cdr paths) (1+ i))))))

(define (display-board board)
  ;; Display the board (but upside-down--the description uses
  ;; bottom-left as the origin).
  (let ((cols (vector-length board))
	(rows (vector-length (vector-ref board 0))))
    (let col-loop ((col 0))
      (if (< col cols)
	  (begin
	    (let row-loop ((row 0))
	      (if (< row rows)
		  (begin
		    (format #t "~3a" (vector-ref (vector-ref board col) row))
		    (row-loop (1+ row)))))
	    (newline)
	    (col-loop (1+ col)))))))

(define (manhattan-distance p1 p2)
  (match-let (((x1 . y1) p1)
	      ((x2 . y2) p2))
    (+ (abs (- x2 x1)) (abs (- y2 y1)))))

(define (find-closest-intersection board origin intersections w h)
  ;; We could build up a list of intersections as we traverse the
  ;; board the first time, but since the board is not that big, we
  ;; might as well just traverse the intersection mask (same size as
  ;; the board)
  (let col-loop ((col 0)
		 (min-steps 999999))
    (if (< col w)
	(col-loop
	 (1+ col)
	 (let row-loop ((row 0)
			(min-steps min-steps))
	   (let ((point (index->point origin (cons col row))))
	     (cond
	      ((< row h)
	       (let* ((combined-steps (board-ref intersections origin point))
		      (min-steps'
		       ;; We'll skip the origin because it is set to -1
		       (if (> combined-steps 0)
			   (min min-steps combined-steps)
			   min-steps)))
		 (row-loop (1+ row) min-steps')))
	      (else min-steps)))))
	min-steps)))

(define (calc-board-origin bounds-ls)
  (let bounds-loop ((bounds-ls bounds-ls)
		    (x 0)
		    (y 0))
    (if (null? bounds-ls)
	(cons x y)
     (match-let (((other-x . other-y) (calc-origin (car bounds-ls))))
       (bounds-loop (cdr bounds-ls)
		    (max x other-x) (max y other-y))))))

(match-let* ((path1 (parse-directions test-string1))
	     (path2 (parse-directions test-string2))
	     (bounds1 (calc-bounds path1))
	     (bounds2 (calc-bounds path2))
	     (bounds-ls `(,bounds1
			  ,bounds2))
	     ((w . h) (calc-board-dims (calc-board-bounds bounds-ls)))
	     (origin (calc-board-origin bounds-ls))
	     (board (make-board w h origin))
	     (intersections (make-board w h origin))
	     (distances (make-board w h origin)))
  (populate-board! board origin intersections distances `(,path1 ,path2))
  (display-board board) (newline)
  (display-board intersections) (newline)
  (display-board distances) (newline)
  (let ((closest-intersection (find-closest-intersection board origin intersections w h)))
    (display closest-intersection) (newline)
    (assert (= closest-intersection 30))))

(define test-string5 "R75,D30,R83,U83,L12,D49,R71,U7,L72")
(define test-string6 "U62,R66,U55,R34,D71,R55,D58,R83")

(match-let* ((path1 (parse-directions test-string5))
	     (path2 (parse-directions test-string6))
	     (bounds1 (calc-bounds path1))
	     (bounds2 (calc-bounds path2))
	     (bounds-ls `(,bounds1
			  ,bounds2))
	     ((w . h) (calc-board-dims (calc-board-bounds bounds-ls)))
	     (origin (calc-board-origin bounds-ls))
	     (board (make-board w h origin))
	     (intersections (make-board w h origin))
	     (distances (make-board w h origin)))
  ;; (display (format #t "w=~a h=~a o=~a,~a\n" w h (car origin) (cdr origin)))
  (populate-board! board origin intersections distances `(,path1 ,path2))
  ;; (display-board board) (newline)
  ;; (display-board intersections) (newline)
  (let ((closest-intersection (find-closest-intersection board origin intersections w h)))
    (assert (= closest-intersection 610))))

(define test-string7 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
(define test-string8 "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

(match-let* ((path1 (parse-directions test-string7))
	     (path2 (parse-directions test-string8))
	     (bounds1 (calc-bounds path1))
	     (bounds2 (calc-bounds path2))
	     (bounds-ls `(,bounds1
			  ,bounds2))
	     ((w . h) (calc-board-dims (calc-board-bounds bounds-ls)))
	     (origin (calc-board-origin bounds-ls))
	     (board (make-board w h origin))
	     (intersections (make-board w h origin))
	     (distances (make-board w h origin)))
  ;; (display (format #t "w=~a h=~a o=~a,~a\n" w h (car origin) (cdr origin)))
  (populate-board! board origin intersections distances `(,path1 ,path2))
  ;; (display-board board) (newline)
  ;; (display-board intersections) (newline)
  (let ((closest-intersection (find-closest-intersection board origin intersections w h)))
    (assert (= closest-intersection 410))))

(define (read-paths)
  (let ((port (current-input-port)))
    (let read-loop ((paths '()))
      (let ((line (read-line port)))
	(if (eof-object? line)
	    paths
	    (read-loop (cons (parse-directions (string-trim-right line)) paths)))))))

(match-let* ((paths (read-paths))
	     (bounds-ls (map calc-bounds paths))
	     ((w . h) (calc-board-dims (calc-board-bounds bounds-ls)))
	     (origin (calc-board-origin bounds-ls))
	     (board (make-board w h origin))
	     (intersections (make-board w h origin))
	     (distances (make-board w h origin)))
  (format #t "w=~a h=~a o=~a,~a\n" w h (car origin) (cdr origin))
  (populate-board! board origin intersections distances paths)
  ;; (display-board board) (newline)
  ;; (display-board intersections) (newline)
  (let ((closest-intersection (find-closest-intersection board origin intersections w h)))
    (display closest-intersection) (newline)))
