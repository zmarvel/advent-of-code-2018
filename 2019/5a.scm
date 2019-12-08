;; --- Day 5: Sunny with a Chance of Asteroids ---
;; 
;; You're starting to sweat as the ship makes its way toward Mercury. The
;; Elves suggest that you get the air conditioner working by upgrading
;; your ship computer to support the Thermal Environment Supervision
;; Terminal.
;; 
;; The Thermal Environment Supervision Terminal (TEST) starts by running
;; a diagnostic program (your puzzle input). The TEST diagnostic program
;; will run on your existing Intcode computer after a few modifications:
;; 
;; First, you'll need to add two new instructions:
;; 
;;     Opcode 3 takes a single integer as input and saves it to the
;;     position given by its only parameter. For example, the instruction
;;     3,50 would take an input value and store it at address 50.
;; 
;;     Opcode 4 outputs the value of its only parameter. For example, the
;;     instruction 4,50 would output the value at address 50.
;; 
;; Programs that use these instructions will come with documentation that
;; explains what should be connected to the input and output. The program
;; 3,0,4,0,99 outputs whatever it gets as input, then halts.
;; 
;; Second, you'll need to add support for parameter modes:
;; 
;; Each parameter of an instruction is handled based on its parameter
;; mode. Right now, your ship computer already understands parameter mode
;; 0, position mode, which causes the parameter to be interpreted as a
;; position - if the parameter is 50, its value is the value stored at
;; address 50 in memory. Until now, all parameters have been in position
;; mode.
;; 
;; Now, your ship computer will also need to handle parameters in mode 1,
;; immediate mode. In immediate mode, a parameter is interpreted as a
;; value - if the parameter is 50, its value is simply 50.
;; 
;; Parameter modes are stored in the same value as the instruction's
;; opcode. The opcode is a two-digit number based only on the ones and
;; tens digit of the value, that is, the opcode is the rightmost two
;; digits of the first value in an instruction. Parameter modes are
;; single digits, one per parameter, read right-to-left from the opcode:
;; the first parameter's mode is in the hundreds digit, the second
;; parameter's mode is in the thousands digit, the third parameter's mode
;; is in the ten-thousands digit, and so on. Any missing modes are 0.
;; 
;; For example, consider the program 1002,4,3,4,33.
;; 
;; The first instruction, 1002,4,3,4, is a multiply instruction - the
;; rightmost two digits of the first value, 02, indicate opcode 2,
;; multiplication. Then, going right to left, the parameter modes are 0
;; (hundreds digit), 1 (thousands digit), and 0 (ten-thousands digit, not
;; present and therefore zero):
;; 
;; ABCDE
;;  1002
;; 
;; DE - two-digit opcode,      02 == opcode 2
;;  C - mode of 1st parameter,  0 == position mode
;;  B - mode of 2nd parameter,  1 == immediate mode
;;  A - mode of 3rd parameter,  0 == position mode,
;;                                   omitted due to being a leading zero
;; 
;; This instruction multiplies its first two parameters. The first
;; parameter, 4 in position mode, works like it did before - its value is
;; the value stored at address 4 (33). The second parameter, 3 in
;; immediate mode, simply has value 3. The result of this operation, 33 *
;; 3 = 99, is written according to the third parameter, 4 in position
;; mode, which also works like it did before - 99 is written to address
;; 4.
;; 
;; Parameters that an instruction writes to will never be in immediate
;; mode.
;; 
;; Finally, some notes:
;; 
;;     It is important to remember that the instruction pointer should
;;     increase by the number of values in the instruction after the
;;     instruction finishes. Because of the new instructions, this amount
;;     is no longer always 4.
;; 
;;     Integers can be negative: 1101,100,-1,4,0 is a valid program (find
;;     100 + -1, store the result in position 4).
;; 
;; The TEST diagnostic program will start by requesting from the user the
;; ID of the system to test by running an input instruction - provide it
;; 1, the ID for the ship's air conditioner unit.
;; 
;; It will then perform a series of diagnostic tests confirming that
;; various parts of the Intcode computer, like parameter modes, function
;; correctly. For each test, it will run an output instruction indicating
;; how far the result of the test was from the expected value, where 0
;; means the test was successful. Non-zero outputs mean that a function
;; is not working correctly; check the instructions that were run before
;; the output instruction to see which one failed.
;; 
;; Finally, the program will output a diagnostic code and immediately
;; halt. This final output isn't an error; an output followed immediately
;; by a halt means the program finished. If all outputs were zero except
;; the diagnostic code, the diagnostic program ran successfully.
;; 
;; After providing 1 to the only input instruction and passing all the
;; tests, what diagnostic code does the program produce?


(use-modules (ice-9 textual-ports)
	     (ice-9 rdelim)
	     (ice-9 match)
	     (ice-9 format)
	     (srfi srfi-1) ;; for fold, map, etc
	     (srfi srfi-11) ;; for (let-values ...)
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

(define (eval-instr! program i)
  (let ((decoded (decode-instruction (vector-ref program i))))
    ;; (display decoded) (newline)
    (match-let (((mode3 mode2 mode1 . opcode) decoded))
      (let ((op1 (get-op program mode1 (+ i 1))))
	(cond
	 ;; add and multiply instructions have 3 operands. we can assume
	 ;; that op3 has mode 'position
	 ((= opcode 1)
	  (let ((op2 (get-op program mode2 (+ i 2))))
	    (set-op! program mode3 (+ i 3) (+ op1 op2))
	    4))
	 ((= opcode 2)
	  (let ((op2 (get-op program mode2 (+ i 2))))
	    (set-op! program mode3 (+ i 3) (* op1 op2))
	    4))
	 ;; input and output instructions have only 1 operand
	 ((= opcode 3)
	  (let ((op1 (vector-ref program (+ i 1)))
		(value (read-int (current-input-port))))
	    (vector-set! program op1 value)
	    2))
	 ((= opcode 4)
	  (begin
	    (display op1) (newline)
	    2)))))))

(let ((program (vector 1 4 5 6 21 22 0)))
  (eval-instr! program 0)
  ;; (display (vector-ref program 6)) (newline)
  (assert (eq? (vector-ref program 6) 43)))

(let ((program (vector 2 4 5 6 21 2 0)))
  (eval-instr! program 0)
  (assert (eq? (vector-ref program 6) 42)))

(define (eval-program! program)
  (let eval-loop ((i 0))
    (if (eq? (vector-ref program i) 99)
	program
	(let ((advance (eval-instr! program i)))
	  (eval-loop (+ i advance))))))

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

(call-with-input-file "5.input"
  (lambda (port)
    (let ((program (read-program port)))
      ;; (display program) (newline)
      (eval-program! program))))

;; (let ((orig-program (read-program)))
;;   (let noun-loop ((noun 0))
;;     (if (<= noun 99)
;; 	(let verb-loop ((verb 0))
;; 	  (if (<= verb 99)
;; 	      (begin
;; 		(let ((program (vector-copy orig-program)))
;; 		  (vector-set! program 1 noun)
;; 		  (vector-set! program 2 verb)
;; 		  (eval-program! program)
;; 		  (if (eq? (vector-ref program 0) 19690720)
;; 		      (display (format #f "noun=~a verb=~a result=~a\n"
;; 				       noun verb
;; 				       (+ (* 100 noun) verb)))
;; 		      (verb-loop (1+ verb)))))
;; 	      (noun-loop (1+ noun))
;; 	      )))))
