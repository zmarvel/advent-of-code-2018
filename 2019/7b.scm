(use-modules (ice-9 textual-ports)
	     (ice-9 rdelim)
	     (ice-9 match)
	     (ice-9 format)
	     (srfi srfi-1) ;; for fold, map, etc
	     (srfi srfi-11) ;; for (let-values ...)
	     (srfi srfi-111) ;; for boxes
	     ((rnrs) :version (6)) ;; for assert
	     )

(define (split-instruction n)
  (let ((digits (make-vector 5 0)))
    (let loop ((i 1)
	       (n n))
      (if (zero? n)
	  digits
	  (let-values (((quotient remainder) (floor/ n 10)))
	    (vector-set! digits (- 5 i) remainder)
	    (loop (1+ i) quotient))))))

(assert (equal? (split-instruction 1002) #(0 1 0 0 2)))
(assert (equal? (split-instruction 11002) #(1 1 0 0 2)))

(define (decode-parameter-mode n)
  (if (= n 0) 'position 'immediate))

(define (repack-opcode n1 n2)
  (+ (* n1 10) n2))

(define (decode-instruction instr)
  (let* ((digits (split-instruction instr))
	 (mode3 (decode-parameter-mode (vector-ref digits 0)))
	 (mode2 (decode-parameter-mode (vector-ref digits 1)))
	 (mode1 (decode-parameter-mode (vector-ref digits 2)))
	 (opcode (repack-opcode (vector-ref digits 3) (vector-ref digits 4))))
    ;; (display digits) (newline)
    `(,mode3 ,mode2 ,mode1 . ,opcode)))

(define (get-op program mode i)
  (let ((op (vector-ref program i)))
    (if (eq? mode 'position)
	(begin
	  ;; (format #t "get program[~a] (~a)\n" op (vector-ref program op))
	  (vector-ref program op))
	op)))

(define (set-op! program mode i value)
  (if (eq? mode 'position)
      (let ((op (vector-ref program i)))
	;; (format #t "set! program[~a]=~a\n" op value)
	(vector-set! program op value))))

(define (eval-instr! program i get-input put-output)
  (let ((decoded (decode-instruction (vector-ref program i))))
    ;; (display decoded) (newline)
    (match-let (((mode3 mode2 mode1 . opcode) decoded))
      (cond
	 ;; add and multiply instructions have 3 operands. we can assume
	 ;; that op3 has mode 'position
	 ((= opcode 1)
	  (let ((op1 (get-op program mode1 (+ i 1)))
		(op2 (get-op program mode2 (+ i 2))))
	    (set-op! program mode3 (+ i 3) (+ op1 op2))
	    4))
	 ((= opcode 2)
	  (let ((op1 (get-op program mode1 (+ i 1)))
		(op2 (get-op program mode2 (+ i 2))))
	    (set-op! program mode3 (+ i 3) (* op1 op2))
	    4))
	 ;; input and output instructions have only 1 operand
	 ((= opcode 3) ;; in
	  (let ((op1 (vector-ref program (+ i 1)))
		(value (get-input)))
	    ;; (format #t "set! program[~a]=~a\n" op1 value)
	    (vector-set! program op1 value)
	    2))
	 ((= opcode 4) ;; out
	  (let ((op1 (get-op program mode1 (+ i 1))))
	    (put-output op1)
	    2))
	 ((= opcode 5) ;; jump if true
	  (let ((op1 (get-op program mode1 (+ i 1)))
		(op2 (get-op program mode2 (+ i 2))))
	    (if (not (zero? op1))
		(- op2 i)
		3)))
	 ((= opcode 6) ;; jump if false
	  (let ((op1 (get-op program mode1 (+ i 1)))
		(op2 (get-op program mode2 (+ i 2))))
	    (if (zero? op1)
		(- op2 i)
		3)))
	 ((= opcode 7) ;; less than
	  (let ((op1 (get-op program mode1 (+ i 1)))
		(op2 (get-op program mode2 (+ i 2))))
	    (set-op! program mode3 (+ i 3)
		     (if (< op1 op2) 1 0))
	    4))
	 ((= opcode 8) ;; equals
	  (let ((op1 (get-op program mode1 (+ i 1)))
		(op2 (get-op program mode2 (+ i 2))))
	    (set-op! program mode3 (+ i 3)
		     (if (= op1 op2) 1 0))
	    4))
	 ))))

(define thunk (lambda () #t))

(let ((program (vector 1 4 5 6 21 22 0)))
  (eval-instr! program 0 thunk thunk)
  ;; (display (vector-ref program 6)) (newline)
  (assert (eq? (vector-ref program 6) 43)))

(let ((program (vector 2 4 5 6 21 2 0)))
  (eval-instr! program 0 thunk thunk)
  (assert (eq? (vector-ref program 6) 42)))

(define (eval-program! program get-input put-output)
  (let eval-loop ((i 0))
    (if (eq? (vector-ref program i) 99)
	program
	(let ((advance (eval-instr! program i get-input put-output)))
	  (eval-loop (+ i advance))))))

(let ((program (vector 2 5 6 7 99 21 2 0)))
  (eval-program! program thunk thunk)
  (assert (eq? (vector-ref program 7) 42)))

(let ((program (vector 2 9 10 11
		       1 11 11 11
		       99
		       21 2 0)))
  (eval-program! program thunk thunk)
  (assert (eq? (vector-ref program 11) 84)))

(define (vector-eq? vec1 vec2)
  (let ((len (min (vector-length vec1) (vector-length vec2))))
    (let loop ((i 0))
      (if (eq? i len)
	  #t
	  (and (eq? (vector-ref vec1 i) (vector-ref vec2 i))
	       (loop (1+ i)))))))

(let ((program (vector 1 0 0 0 99)))
  (eval-program! program thunk thunk)
  (assert (vector-eq? program #(2 0 0 0 99))))

(let ((program (vector 2 3 0 3 99)))
  (eval-program! program thunk thunk)
  (assert (vector-eq? program #(2 3 0 6 99))))

(let ((program (vector 2 4 4 5 99 0)))
  (eval-program! program thunk thunk)
  (assert (vector-eq? program #(2 4 4 5 99 9801))))

(let ((program (vector 1 1 1 4 99 5 6 0 99)))
  (eval-program! program thunk thunk)
  (assert (vector-eq? program #(30 1 1 4 2 5 6 0 99))))

(define (vector-resize vec new-size)
  (let ((old-size (vector-length vec))
	(vec' (make-vector new-size)))
    (let copy-loop ((i 0))
      (if (eq? i old-size)
	  vec'
	  (begin
	    (vector-set! vec' i (vector-ref vec i))
	    (copy-loop (1+ i)))))))

(let* ((vec #(1 2 3))
       (vec' (vector-resize vec 4)))
  (let loop ((i 0))
    (if (< i (vector-length vec))
	(assert (eq? (vector-ref vec i)
		     (vector-ref vec' i))))))

(define (read-int port)
  ;; Read until the next comma and convert the result into an integer
  (let read-loop ((digits '()))
    (if (char-ready? port)
	(let ((char (get-char port)))
	  (if (or (eq? char #\,) (eq? char #\newline))
	      (string->number (list->string (reverse digits)))
	      (read-loop (cons char digits))))
	(string->number (list->string (reverse digits))))))

(define (read-program port)
  (let read-loop ((position 0)
		  (program (make-vector 2)))
    (if (eof-object? (peek-char port))
	program
	(let ((program'
	       (if (>= position (vector-length program))
		   (vector-resize
		    program
		    (* 2 (vector-length program)))
		   program))
	      (int (read-int port)))
	  (vector-set! program' position int)
	  ;; (display int) (newline)
	  (read-loop (1+ position) program')))))

(define inputs (make-vector 10 0))
(define input-idx 0)
(define (get-input)
  (let ((input (vector-ref inputs input-idx)))
    (set! input-idx (1+ input-idx))
    input))

(define output 0)
(define (put-output o)
  (set! output o))

(define (test eq? actual expected)
  (if (not (eq? actual expected))
      (begin
	(format #t "FAIL: actual=~a expected=~a\n" actual expected)
	(assert #f))))

(define (unique-phases? a b c d e)
  (not (or (= a b) (= a c) (= a d) (= a e)
	   (= b c) (= b d) (= b e)
	   (= c d) (= c e)
	   (= d e))))

(define (eval-until-output! program i input output-box)
  ;; Returns (program-counter . done)
  (let loop ((i i))
    (cond
     ((= (vector-ref program i) 4) ;; output instr
      (let ((advance (eval-instr!
		      program i
		      thunk
		      (lambda (o) (set-box! output-box o)))))
	;; (format #t "i=~a out=~a\n" i out)
	(cons (+ i advance) #f)))
     ((= (vector-ref program i) 99) ;; halt
      (cons i #t))
     (else
      (let ((advance (eval-instr! program i (lambda () input) thunk)))
	;; (format #t "i=~a out=~a\n" i out)
	(loop (+ i advance)))))))

(define (eval-loop! program a b c d e)
  (let ((program-a (vector-copy program))
	(program-b (vector-copy program))
	(program-c (vector-copy program))
	(program-d (vector-copy program))
	(program-e (vector-copy program))
	(pc-a 0)
	(pc-b 0)
	(pc-c 0)
	(pc-d 0)
	(pc-e 0)
	(output-a (box 0))
	(output-b (box 0))
	(output-c (box 0))
	(output-d (box 0))
	(output-e (box 0))
	)

    ;; only have to get phase on first input
    (let ((advance (eval-instr! program-a 0 (lambda () a) thunk)))
      (set! pc-a (+ pc-a advance)))
    (let ((advance (eval-instr! program-b 0 (lambda () b) thunk)))
      (set! pc-b (+ pc-b advance)))
    (let ((advance (eval-instr! program-c 0 (lambda () c) thunk)))
      (set! pc-c (+ pc-c advance)))
    (let ((advance (eval-instr! program-d 0 (lambda () d) thunk)))
      (set! pc-d (+ pc-d advance)))
    (let ((advance (eval-instr! program-e 0 (lambda () e) thunk)))
      (set! pc-e (+ pc-e advance)))

    (let loop ((output 0))
      ;; (format #t "~a out=~a\n" (list a b c d e) output)
      (match-let (((pc . done) (eval-until-output!
				program-a pc-a
				(unbox output-e) output-a)))
	(set! pc-a pc)
	(if done
	    (unbox output-e)
	    (match-let (((pc . done) (eval-until-output!
				      program-b pc-b
				      (unbox output-a) output-b)))
	      (set! pc-b pc)
	      (if done
		  (unbox output-b)
		  (match-let (((pc . done) (eval-until-output!
					    program-c pc-c
					    (unbox output-b) output-c)))
		    (set! pc-c pc)
		    (if done
			(unbox output-c)
			(match-let (((pc . done) (eval-until-output!
						  program-d pc-d
						  (unbox output-c) output-d)))
			  (set! pc-d pc)
			  (if done
			      (unbox output-d)
			      (match-let (((pc . done) (eval-until-output!
							program-e pc-e
							(unbox output-d) output-e)))
				(set! pc-e pc)
				(if done
				    (unbox output-e)
				    (loop output-e)))))))
		  )))))))

(define (maximize-output program)
  (let inputs-loop ((a 5)
		    (b 5)
		    (c 5)
		    (d 5)
		    (e 5)
		    (max-input '(0 0 0 0 0))
		    (max-output 0))

    (define (continue max-input max-output)
      (cond
       ((< e 9) (inputs-loop a b c d (1+ e) max-input max-output))
       ((< d 9) (inputs-loop a b c (1+ d) 5 max-input max-output))
       ((< c 9) (inputs-loop a b (1+ c) 5 5 max-input max-output))
       ((< b 9) (inputs-loop a (1+ b) 5 5 5 max-input max-output))
       ((< a 9) (inputs-loop (1+ a) 5 5 5 5 max-input max-output))
       (else (cons max-input max-output))))

    (let ((program (vector-copy program)))
      (if (unique-phases? a b c d e)
	  (let ((output (eval-loop! program a b c d e)))
	    ;; (format #t "phases=~a out=~a\n" (list a b c d e) output)
	    (continue (if (> output max-output)
			  (list a b c d e)
			  max-input)
		      (if (> output max-output)
			  output
			  max-output)))
	  (continue max-input max-output)))))

(display "test1") (newline)
(let ((program (call-with-input-file "7b-1.test" read-program)))
  (match-let (((i . max) (maximize-output program)))
    (display (cons i max)) (newline)
    (test = max 139629729)
    (test equal? i '(9 8 7 6 5))))

(display "test2") (newline)
(let ((program (call-with-input-file "7b-2.test" read-program)))
  (match-let (((i . max) (maximize-output program)))
    (display (cons i max)) (newline)
    (test = max 18216)
    (test equal? i '(9 7 8 5 6))))

(let ((program (call-with-input-file "7.input" read-program)))
  (match-let (((i . max) (maximize-output program)))
    (format #t "setting=~a output=~a\n" i max)))
 
;; 65210 too low
