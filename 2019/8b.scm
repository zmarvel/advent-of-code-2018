(use-modules (ice-9 textual-ports)
	     (ice-9 rdelim)
	     (ice-9 match)
	     (ice-9 format)
	     (srfi srfi-1) ;; for fold, map, etc
	     (srfi srfi-13) ;; for extended hash table operations
	     ;; (srfi srfi-11) ;; for (let-values ...)
	     ;; (srfi srfi-111) ;; for boxes
	     ((rnrs) :version (6)) ;; for assert
	     )

(define (test eq? actual expected)
  (if (not (eq? actual expected))
      (begin
	(format #t "FAIL: actual=~a expected=~a\n" actual expected)
	(assert #f))))

(define (split-layers input dims)
  ;; Input should be a list
  (match-let (((w . h) dims))
    (let ((size (* w h)))
      (let loop ((input input)
		 (layer '())
		 (i 0))
	(if (null? input)
	    (list (reverse layer))
	    (if (< i size)
		(loop (cdr input) (cons (car input) layer) (1+ i))
		(cons (reverse layer) (loop input '() 0))))))))

(define test-transmission1 '(1 2 3 4 5 6 7 8 9 0 1 2))

(let ((actual (split-layers test-transmission1 '(3 . 2)))
      (expected '((1 2 3 4 5 6) (7 8 9 0 1 2))))
  (test equal? actual expected))

(define (count-digits layer)
  (let ((counts (make-hash-table 10)))
    (let loop ((layer layer))
      (if (null? layer)
	  counts
	  (let* ((digit (car layer))
		 (count (hashv-ref counts digit 0)))
	    (hashv-set! counts digit (1+ count))
	    (loop (cdr layer)))))))

(let ((layers (split-layers test-transmission1 '(3 . 2))))
  (let ((actual (hash-map->list cons (count-digits (car layers))))
	(expected '((1 . 1) (2 . 1) (3 . 1) (4 . 1) (5 . 1) (6 . 1))))
    (test (lambda (a b) (lset= equal? a b)) actual expected))
  (let ((actual (hash-map->list cons (count-digits (cadr layers))))
	(expected '((7 . 1) (8 . 1) (9 . 1) (0 . 1) (1 . 1) (2 . 1))))
    (test (lambda (a b) (lset= equal? a b)) actual expected)))

(define (read-input port)
  (let loop ()
    (let ((char (get-char port)))
      (if (or (eof-object? char) (eqv? char #\newline))
	  '()
	  (cons (- (char->integer char) 48) (loop))))))

(let ((actual (call-with-input-string "12345" read-input))
      (expected '(1 2 3 4 5)))
  (test equal? actual expected))

(define (transparent-pixel? v)
  (= v 2))

(define (overlay-pixel a b)
  ;; Overlay pixel b on top of a
  (if (transparent-pixel? b)
      a
      b))

(define (overlay-layer a b)
  ;; Overlay layer b on top of a
  (let* ((size (vector-length a))
	 (output (make-vector size 2)))
    (let loop ((i 0))
      (if (= i size)
	  output
	  (begin
	    (vector-set! output i (overlay-pixel (vector-ref a i)
						 (vector-ref b i)))
	    (loop (1+ i)))))))

(define (overlay layers)
  (let ((layers (map list->vector layers)))
    (fold (lambda (layer acc)
	    (overlay-layer layer acc))
	  ;; seed is a transparent image
	  (make-vector (vector-length (car layers)) 2)
	  layers)))

(define (row-col->index row col w)
  (+ (* row w) col))

(define (display-image image dims)
  (match-let (((w . h) dims))
    (let row-loop ((row 0))
      (if (< row h)
	  (begin
	    (let col-loop ((col 0))
	      (if (< col w)
		  (begin
		    (format #t "~a" (if (= (vector-ref image (row-col->index row col w)) 1)
					#\.
					#\space
					))
		    (col-loop (1+ col)))))
	    (newline)
	    (row-loop (1+ row)))))))

(let* ((dims '(25 . 6))
       (input (call-with-input-file "8.input" read-input))
       (layers (split-layers input dims))
       (image (overlay layers)))
  (display-image image dims))
