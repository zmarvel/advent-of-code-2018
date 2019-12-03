;; "Good, the new computer seems to be working correctly! Keep it nearby
;; during this mission - you'll probably use it again. Real Intcode
;; computers support many more features than your new one, but we'll let
;; you know what they are as you need them."
;;
;; "However, your current priority should be to complete your gravity
;; assist around the Moon. For this mission to succeed, we should settle
;; on some terminology for the parts you've already built."
;;
;; Intcode programs are given as a list of integers; these values are
;; used as the initial state for the computer's memory. When you run an
;; Intcode program, make sure to start by initializing memory to the
;; program's values. A position in memory is called an address (for
;; example, the first value in memory is at "address 0").
;;
;; Opcodes (like 1, 2, or 99) mark the beginning of an instruction. The
;; values used immediately after an opcode, if any, are called the
;; instruction's parameters. For example, in the instruction 1,2,3,4, 1
;; is the opcode; 2, 3, and 4 are the parameters. The instruction 99
;; contains only an opcode and has no parameters.
;;
;; The address of the current instruction is called the instruction
;; pointer; it starts at 0. After an instruction finishes, the
;; instruction pointer increases by the number of values in the
;; instruction; until you add more instructions to the computer, this is
;; always 4 (1 opcode + 3 parameters) for the add and multiply
;; instructions. (The halt instruction would increase the instruction
;; pointer by 1, but it halts the program instead.)
;;
;; "With terminology out of the way, we're ready to proceed. To complete
;; the gravity assist, you need to determine what pair of inputs produces
;; the output 19690720."
;;
;; The inputs should still be provided to the program by replacing the
;; values at addresses 1 and 2, just like before. In this program, the
;; value placed in address 1 is called the noun, and the value placed in
;; address 2 is called the verb. Each of the two input values will be
;; between 0 and 99, inclusive.
;;
;; Once the program has halted, its output is available at address 0,
;; also just like before. Each time you try a pair of inputs, make sure
;; you first reset the computer's memory to the values in the program
;; (your puzzle input) - in other words, don't reuse memory from a
;; previous attempt.
;;
;; Find the input noun and verb that cause the program to produce the
;; output 19690720. What is 100 * noun + verb? (For example, if noun=12
;; and verb=2, the answer would be 1202.)

(use-modules (ice-9 binary-ports)
	     (ice-9 rdelim)
	     (ice-9 match)
	     (ice-9 format)
	     (srfi srfi-1) ;; for fold, map, etc
	     (srfi srfi-11) ;; for (let-values ...)
	     ((rnrs) :version (6)) ;; for assert
	     )

(define (eval-instr! program i)
  (let* ((code (vector-ref program i))
	 (op1-addr (vector-ref program (+ i 1)))
	 (op1 (vector-ref program op1-addr))
	 (op2-addr (vector-ref program (+ i 2)))
	 (op2 (vector-ref program op2-addr))
	 (dest-addr (vector-ref program (+ i 3))))
    (cond
     ((eq? code 1) (vector-set! program dest-addr (+ op1 op2)))
     ((eq? code 2) (vector-set! program dest-addr (* op1 op2))))))

(let ((program (vector 1 4 5 6 21 22 0)))
  (eval-instr! program 0)
  (assert (eq? (vector-ref program 6) 43)))

(let ((program (vector 2 4 5 6 21 2 0)))
  (eval-instr! program 0)
  (assert (eq? (vector-ref program 6) 42)))

(define (eval-program! program)
  (let eval-loop ((i 0))
    (if (eq? (vector-ref program i) 99)
	program
	(begin
	  (eval-instr! program i)
	  (eval-loop (+ i 4))))))

(let ((program (vector 2 5 6 7 99 21 2 0)))
  (eval-program! program)
  (assert (eq? (vector-ref program 7) 42)))

(let ((program (vector 2 9 10 11
		       1 11 11 11
		       99
		       21 2 0)))
  (eval-program! program)
  (assert (eq? (vector-ref program 11) 84)))

(define (vector-eq? vec1 vec2)
  (let ((len (min (vector-length vec1) (vector-length vec2))))
    (let loop ((i 0))
      (if (eq? i len)
	  #t
	  (and (eq? (vector-ref vec1 i) (vector-ref vec2 i))
	       (loop (1+ i)))))))

(let ((program (vector 1 0 0 0 99)))
  (eval-program! program)
  (assert (vector-eq? program #(2 0 0 0 99))))

(let ((program (vector 2 3 0 3 99)))
  (eval-program! program)
  (assert (vector-eq? program #(2 3 0 6 99))))

(let ((program (vector 2 4 4 5 99 0)))
  (eval-program! program)
  (assert (vector-eq? program #(2 4 4 5 99 9801))))

(let ((program (vector 1 1 1 4 99 5 6 0 99)))
  (eval-program! program)
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

(define (read-program)
  (let ((port (current-input-port)))
    (let read-loop ((position 0)
		    (program (make-vector 2)))
      (if (char-ready? port)
	  (let ((program'
		 (if (>= position (vector-length program))
		     (vector-resize
		      program
		      (* 2 (vector-length program)))
		     program))
		(int (read-int port)))
	    (vector-set! program' position int)
	    (read-loop (1+ position) program'))
	  program))))

(let ((orig-program (read-program)))
  (let noun-loop ((noun 0))
    (if (<= noun 99)
	(let verb-loop ((verb 0))
	  (if (<= verb 99)
	      (begin
		(let ((program (vector-copy orig-program)))
		  (vector-set! program 1 noun)
		  (vector-set! program 2 verb)
		  (eval-program! program)
		  (if (eq? (vector-ref program 0) 19690720)
		      (display (format #f "noun=~a verb=~a result=~a\n"
				       noun verb
				       (+ (* 100 noun) verb)))
		      (verb-loop (1+ verb)))))
	      (noun-loop (1+ noun))
	      )))))
