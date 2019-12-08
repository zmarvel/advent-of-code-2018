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


(let* ((input (call-with-input-file "8.input" read-input))
       (layers (split-layers input '(25 . 6)))
       (counts (map count-digits layers))
       (min-counts
	;; find the hash map with fewest 0s
	(let loop ((min (let ((hash (make-hash-table 10)))
			  (hashv-set! hash 0 9999)
			  hash))
		   (counts counts))
	  (if (null? counts) min
	      (loop (if (< (hashv-ref (car counts) 0 0) (hashv-ref min 0))
			(car counts)
			min)
		    (cdr counts))))))
  (display (hash-map->list cons min-counts)) (newline)
  (display (* (hashv-ref min-counts 1 0) (hashv-ref min-counts 2 0))) (newline))
