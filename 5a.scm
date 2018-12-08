

(use-modules (ice-9 binary-ports)
	     (ice-9 rdelim)
	     (ice-9 match)
	     (ice-9 format)
	     (srfi srfi-26)
	     (srfi srfi-1)
	     (srfi srfi-11)
	     (srfi srfi-43))

(define (read-input port)
  (let ((line (read-line port)))
    (string-trim-right line)))

(define (reacts? c1 c2)
  (or (and (char-upper-case? c1) (char-lower-case? c2)
	   (char=? (char-downcase c1) c2))
      (and (char-lower-case? c1) (char-upper-case? c2)
	   (char=? c1 (char-downcase c2)))))

(define (reacts-until str len lpos)
  (if (< lpos (1- len))
      (let ((l (string-ref str lpos))
	    (r (string-ref str (1+ lpos))))
	(if (reacts? l r)
	    (reacts-until str len (+ 2 lpos))
	    lpos))
      lpos))


(let ((tests '(("AMmMma" 0 . 0)
	       ("MmMm" 0 . 4)
	       ("AMmMma" 1 . 5))))
  (for-each (lambda (test)
	      (let ((test-string (car test))
		    (start-pos (cadr test))
		    (expected (cddr test)))
		(let ((result (reacts-until test-string
					    (string-length test-string)
					    start-pos)))
		  (unless (= result expected)
		    (format #t "~a: got ~a, expected ~a\n"
			    test result expected)))
		))
	    tests))

(define (react-polymers str len lpos rpos)
  (if (or (< lpos 0) (>= rpos len))
      0
      (let ((l (string-ref str lpos))
	    (r (string-ref str rpos)))
	(if (= lpos (1- rpos))
	    (if (reacts? l r)
		(let*
		    ;; - end: The index of the first polymer that doesn't
		    ;;   react with its immediate neighbor
		    ;; - reacts-len: Number of polymers that react.
		    ((end (reacts-until str len lpos))
		     (reacts-len (- end lpos)))
		  (+ reacts-len
		     (let*
			 ;; - reacts-around: How many surrounding polymers
			 ;;   react? For example, in acbBAC with lpos=2 (b),
			 ;;   the answer should be 2.
			 ;; - end': The end, after accounting for
			 ;;   reacts-around. In the example above, end is
			 ;;   4, so end' is 6.
			 ((reacts-around (react-polymers str len (1- lpos) end))
			  (end' (+ end (/ reacts-around 2))))
		       (+ reacts-around
			  (react-polymers str len end' (1+ end'))))
		     ))
		(react-polymers str len (1+ lpos) (1+ rpos)))
	    (if (reacts? l r)
		(+ 2 (react-polymers str len (1- lpos) (1+ rpos)))
		0)))))
	    

	    ;; (if (and (= lpos (1- rpos)) (reacts? l r))
	    ;; 	(let*
	    ;; 	    ;; - end: The index of the first polymer that doesn't
	    ;; 	    ;;   react with its immediate neighbor
	    ;; 	    ;; - reacts-len: Number of polymers that react.
	    ;; 	    ((end (reacts-until str len lpos))
	    ;; 	     (reacts-len (- end lpos)))
	    ;; 	  (+ reacts-len
	    ;; 	     (let*
	    ;; 		 ;; - reacts-around: How many surrounding polymers
	    ;; 		 ;;   react? For example, in acbBAC with lpos=2 (b),
	    ;; 		 ;;   the answer should be 2.
	    ;; 		 ;; - end': The end, after accounting for
	    ;; 		 ;;   reacts-around. In the example above, end is
	    ;; 		 ;;   4, so end' is 6.
	    ;; 		 ((reacts-around (react-polymers str len (1- lpos) end))
	    ;; 		  (end' (+ end (/ reacts-around 2))))
	    ;; 	       (react-polymers str len end' (1+ end')))
	    ;; 	     ))
	    ;; 	(if (reacts? l r)
	    ;; 	    (+ 2 (react-polymers str len (1- lpos) (1+ rpos)))
	    ;; 	    0)))))
  ;; (cond
  ;;  ((< lpos 0) 0)
  ;;  ((>= rpos len) 0)
  ;;  ((reacts? (string-ref str lpos) (string-ref str rpos))
  ;;   (begin (format #t "~a~a\n" (string-ref str lpos) (string-ref str rpos))
  ;; 	   (let*
  ;; 	       ;; - end: The index of the first polymer that doesn't
  ;; 	       ;;   react with its immediate neighbor
  ;; 	       ;; - reacts-len: Number of polymers that react.
  ;; 	       ((end (reacts-until str len lpos))
  ;; 		  (reacts-len (- end lpos)))
  ;; 	     (+ reacts-len
  ;; 		(let*
  ;; 		    ;; - reacts-around: How many surrounding polymers
  ;; 		    ;;   react? For example, in acbBAC with lpos=2 (b),
  ;; 		    ;;   the answer should be 2.
  ;; 		    ;; - end': The end, after accounting for
  ;; 		    ;;   reacts-around. In the example above, end is
  ;; 		    ;;   4, so end' is 6.
  ;; 		    ((reacts-around (react-polymers str len (1- lpos) end))
  ;; 		       (end' (+ end (/ reacts-around 2))))
  ;; 		  (react-polymers str len end' (1+ end')))
  ;; 		))))
  ;;  ((= lpos (1- rpos))
  ;;   (react-polymers str len rpos (1+ rpos)))
  ;;  (else 0)))


(let* ((input (read-input (current-input-port)))
       (len (string-length input))
       (reaction-count (react-polymers input len 0 1))
       (remaining-polymers (- len reaction-count)))
  (format #t "~a total polymers; ~a polymers reacted; ~a remaining\n"
	  len reaction-count remaining-polymers)
  )
