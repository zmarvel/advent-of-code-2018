;; --- Part Two ---
;; 
;; The air conditioner comes online! Its cold air feels good for a while,
;; but then the TEST alarms start to go off. Since the air conditioner
;; can't vent its heat anywhere but back into the spacecraft, it's
;; actually making the air inside the ship warmer.
;; 
;; Instead, you'll need to use the TEST to extend the thermal
;; radiators. Fortunately, the diagnostic program (your puzzle input) is
;; already equipped for this. Unfortunately, your Intcode computer is
;; not.
;; 
;; Your computer is only missing a few opcodes:
;; 
;;     Opcode 5 is jump-if-true: if the first parameter is non-zero, it
;;     sets the instruction pointer to the value from the second
;;     parameter. Otherwise, it does nothing.
;; 
;;     Opcode 6 is jump-if-false: if the first parameter is zero, it sets
;;     the instruction pointer to the value from the second
;;     parameter. Otherwise, it does nothing.
;; 
;;     Opcode 7 is less than: if the first parameter is less than the
;;     second parameter, it stores 1 in the position given by the third
;;     parameter. Otherwise, it stores 0.
;; 
;;     Opcode 8 is equals: if the first parameter is equal to the second
;;     parameter, it stores 1 in the position given by the third
;;     parameter. Otherwise, it stores 0.
;; 
;; Like all instructions, these instructions need to support parameter
;; modes as described above.
;; 
;; Normally, after an instruction is finished, the instruction pointer
;; increases by the number of values in that instruction. However, if the
;; instruction modifies the instruction pointer, that value is used and
;; the instruction pointer is not automatically increased.
;; 
;; For example, here are several programs that take one input, compare it
;; to the value 8, and then produce one output:
;; 
;;     3,9,8,9,10,9,4,9,99,-1,8 - Using position mode, consider whether
;;     the input is equal to 8; output 1 (if it is) or 0 (if it is not).
;; 
;;     3,9,7,9,10,9,4,9,99,-1,8 - Using position mode, consider whether
;;     the input is less than 8; output 1 (if it is) or 0 (if it is not).
;; 
;;     3,3,1108,-1,8,3,4,3,99 - Using immediate mode, consider whether
;;     the input is equal to 8; output 1 (if it is) or 0 (if it is not).
;; 
;;     3,3,1107,-1,8,3,4,3,99 - Using immediate mode, consider whether
;;     the input is less than 8; output 1 (if it is) or 0 (if it is not).
;; 
;; Here are some jump tests that take an input, then output 0 if the
;; input was zero or 1 if the input was non-zero:
;; 
;;     3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 (using position mode)
;; 
;;     3,3,1105,-1,9,1101,0,0,12,4,12,99,1 (using immediate mode)
;; 
;; Here's a larger example:
;; 
;; 3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
;; 1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
;; 999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
;; 
;; The above example program uses an input instruction to ask for a
;; single number. The program will then output 999 if the input value is
;; below 8, output 1000 if the input value is equal to 8, or output 1001
;; if the input value is greater than 8.
;; 
;; This time, when the TEST diagnostic program runs its input instruction
;; to get the ID of the system to test, provide it 5, the ID for the
;; ship's thermal radiator controller. This diagnostic test suite only
;; outputs one number, the diagnostic code.
;; 
;; What is the diagnostic code for system ID 5?


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
	    2))
	 ((= opcode 5) ;; jump if true
	  (let ((op2 (get-op program mode2 (+ i 2))))
	    (if (not (zero? op1))
		(- op2 i)
		3)))
	 ((= opcode 6) ;; jump if false
	  (let ((op2 (get-op program mode2 (+ i 2))))
	    (if (zero? op1)
		(- op2 i)
		3)))
	 ((= opcode 7) ;; less than
	  (let ((op2 (get-op program mode2 (+ i 2))))
	    (set-op! program mode3 (+ i 3)
		     (if (< op1 op2) 1 0))
	    4))
	 ((= opcode 8) ;; equals
	  (let ((op2 (get-op program mode2 (+ i 2))))
	    (set-op! program mode3 (+ i 3)
		     (if (= op1 op2) 1 0))
	    4))
	 )))))

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

;; (call-with-input-file "5-1.test"
;; (call-with-input-file "5-2.test"
;; (call-with-input-file "5-3.test"
;;   (lambda (port)
;;     (let ((program (read-program port)))
;;       ;; (display program) (newline)
;;       (eval-program! program))))

(call-with-input-file "5.input"
  (lambda (port)
    (let ((program (read-program port)))
      ;; (display program) (newline)
      (eval-program! program))))
