;; --- Day 9: Sensor Boost ---
;; 
;; You've just said goodbye to the rebooted rover and left Mars when you
;; receive a faint distress signal coming from the asteroid belt. It must
;; be the Ceres monitoring station!
;; 
;; In order to lock on to the signal, you'll need to boost your
;; sensors. The Elves send up the latest BOOST program - Basic Operation
;; Of System Test.
;; 
;; While BOOST (your puzzle input) is capable of boosting your sensors,
;; for tenuous safety reasons, it refuses to do so until the computer it
;; runs on passes some checks to demonstrate it is a complete Intcode
;; computer.
;; 
;; Your existing Intcode computer is missing one key feature: it needs
;; support for parameters in relative mode.
;; 
;; Parameters in mode 2, relative mode, behave very similarly to
;; parameters in position mode: the parameter is interpreted as a
;; position. Like position mode, parameters in relative mode can be read
;; from or written to.
;; 
;; The important difference is that relative mode parameters don't count
;; from address 0. Instead, they count from a value called the relative
;; base. The relative base starts at 0.
;; 
;; The address a relative mode parameter refers to is itself plus the
;; current relative base. When the relative base is 0, relative mode
;; parameters and position mode parameters with the same value refer to
;; the same address.
;; 
;; For example, given a relative base of 50, a relative mode parameter of
;; -7 refers to memory address 50 + -7 = 43.
;; 
;; The relative base is modified with the relative base offset instruction:
;; 
;;     Opcode 9 adjusts the relative base by the value of its only
;;     parameter. The relative base increases (or decreases, if the value is
;;     negative) by the value of the parameter.
;; 
;; For example, if the relative base is 2000, then after the instruction
;; 109,19, the relative base would be 2019. If the next instruction were
;; 204,-34, then the value at address 1985 would be output.
;; 
;; Your Intcode computer will also need a few other capabilities:
;; 
;;     - The computer's available memory should be much larger than the
;;       initial program. Memory beyond the initial program starts with the
;;       value 0 and can be read or written like any other memory. (It is
;;       invalid to try to access memory at a negative address, though.)
;;     - The computer should have support for large numbers. Some
;;       instructions near the beginning of the BOOST program will verify this
;;       capability.
;; 
;; Here are some example programs that use these features:
;; 
;;     - 109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99 takes
;;       no input and produces a copy of itself as output.
;;     - 1102,34915192,34915192,7,4,7,99,0 should output a 16-digit number.
;;     - 104,1125899906842624,99 should output the large number in the
;;       middle.
;; 
;; The BOOST program will ask for a single input; run it in test mode by
;; providing it the value 1. It will perform a series of checks on each
;; opcode, output any opcodes (and the associated parameter modes) that
;; seem to be functioning incorrectly, and finally output a BOOST
;; keycode.
;; 
;; Once your Intcode computer is fully functional, the BOOST program
;; should report no malfunctioning opcodes when run in test mode; it
;; should only output a single value, the BOOST keycode. What BOOST
;; keycode does it produce?


(use-modules (ice-9 textual-ports)
	     (ice-9 rdelim)
	     (ice-9 match)
	     (ice-9 format)
	     (srfi srfi-1) ;; for fold, map, etc
	     (srfi srfi-11) ;; for (let-values ...)
	     (srfi srfi-111) ;; for boxes
	     ((rnrs) :version (6)) ;; for assert
	     )

(define (test eq? actual expected)
  (if (not (eq? actual expected))
      (begin
	(format #t "FAIL: actual=~a expected=~a\n" actual expected)
	(assert #f))))

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
  (cond
   ((= n 0) 'position)
   ((= n 1) 'immediate)
   ((= n 2) 'relative)))

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

(define (eval-instr! get-input put-output relative-base program i)

  (define (get-op mode i)
    (let ((op (vector-ref program i)))
      (match mode
	('position (vector-ref program op))
	('immediate op)
	('relative (vector-ref program (+ (unbox relative-base) op))))))

  (define (set-op! mode i value)
    (let ((op (vector-ref program i)))
      ;; (format #t "set! program[~a]=~a\n" op value)
      (match mode
	('position (vector-set! program op value))
	;; invalid--cause an exception
	('immediate (vector-set! program -1 #f))
	('relative (vector-set! program (+ (unbox relative-base) op) value)))))

  (let ((decoded (decode-instruction (vector-ref program i))))
    ;; (format #t "~a: ~a\n" i decoded)
    (match-let (((mode3 mode2 mode1 . opcode) decoded))
      (cond
       ;; add and multiply instructions have 3 operands. we can assume
       ;; that op3 has mode 'position
       ((= opcode 1)
	(let ((op1 (get-op mode1 (+ i 1)))
	      (op2 (get-op mode2 (+ i 2))))
	  (set-op! mode3 (+ i 3) (+ op1 op2))
	  4))
       ((= opcode 2)
	(let ((op1 (get-op mode1 (+ i 1)))
	      (op2 (get-op mode2 (+ i 2))))
	  (set-op! mode3 (+ i 3) (* op1 op2))
	  4))
       ;; input and output instructions have only 1 operand
       ((= opcode 3) ;; input
	(let ((value (get-input)))
	  ;; (format #t "set! program[~a]=~a\n" (+ i 1) value)
	  (set-op! mode1 (+ i 1) value)
	  2))
       ((= opcode 4) ;; output
	(let ((op1 (get-op mode1 (+ i 1))))
	  (put-output op1)
	  2))
       ((= opcode 5) ;; jump if true
	(let ((op1 (get-op mode1 (+ i 1)))
	      (op2 (get-op mode2 (+ i 2))))
	  (if (not (zero? op1))
	      (- op2 i)
	      3)))
       ((= opcode 6) ;; jump if false
	(let ((op1 (get-op mode1 (+ i 1)))
	      (op2 (get-op mode2 (+ i 2))))
	  (if (zero? op1)
	      (- op2 i)
	      3)))
       ((= opcode 7) ;; less than
	(let ((op1 (get-op mode1 (+ i 1)))
	      (op2 (get-op mode2 (+ i 2))))
	  (set-op! mode3 (+ i 3)
		   (if (< op1 op2) 1 0))
	  4))
       ((= opcode 8) ;; equals
	(let ((op1 (get-op mode1 (+ i 1)))
	      (op2 (get-op mode2 (+ i 2))))
	  (set-op! mode3 (+ i 3)
		   (if (= op1 op2) 1 0))
	  4))
       ((= opcode 9) ;; add to relative base
	(let ((op1 (get-op mode1 (+ i 1))))
	  (set-box! relative-base (+ (unbox relative-base) op1))
	  2))))))

(define (thunk) #t)

(let ((program (vector 1 4 5 6 21 22 0)))
  (eval-instr! thunk thunk (box 0) program 0)
  ;; (display (vector-ref program 6)) (newline)
  (assert (eq? (vector-ref program 6) 43)))

(let ((program (vector 2 4 5 6 21 2 0)))
  (eval-instr! thunk thunk (box 0) program 0)
  (assert (eq? (vector-ref program 6) 42)))

(define (eval-program! get-input put-output program)
  (let ((relative-base (box 0)))
    (let eval-loop ((i 0))
      (if (eq? (vector-ref program i) 99)
	  program
	  (let ((advance (eval-instr! get-input put-output
				      relative-base program i)))
	    ;; (display program) (newline)
	    (eval-loop (+ i advance)))))))

(let ((program (vector 2 5 6 7 99 21 2 0)))
  (eval-program! thunk thunk program)
  (assert (eq? (vector-ref program 7) 42)))

(let ((program (vector 2 9 10 11
		       1 11 11 11
		       99
		       21 2 0)))
  (eval-program! thunk thunk program)
  (assert (eq? (vector-ref program 11) 84)))

(define (vector-eq? vec1 vec2)
  (let ((len (min (vector-length vec1) (vector-length vec2))))
    (let loop ((i 0))
      (if (eq? i len)
	  #t
	  (and (eq? (vector-ref vec1 i) (vector-ref vec2 i))
	       (loop (1+ i)))))))

(let ((program (vector 1 0 0 0 99)))
  (eval-program! thunk thunk program)
  (assert (vector-eq? program #(2 0 0 0 99))))

(let ((program (vector 2 3 0 3 99)))
  (eval-program! thunk thunk program)
  (assert (vector-eq? program #(2 3 0 6 99))))

(let ((program (vector 2 4 4 5 99 0)))
  (eval-program! thunk thunk program)
  (assert (vector-eq? program #(2 4 4 5 99 9801))))

(let ((program (vector 1 1 1 4 99 5 6 0 99)))
  (eval-program! thunk thunk program)
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
    (let ((char (get-char port)))
      (if char
	  (if (or (eq? char #\,) (eq? char #\newline))
	      (string->number (list->string (reverse digits)))
	      (read-loop (cons char digits)))
	  (string->number (list->string (reverse digits)))
	  ))))

(define program-size 2048)
(define (read-program port)
  (let read-loop ((position 0)
		  (program (make-vector program-size 0)))
    (if (eof-object? (peek-char port))
	program
	(let ((int (read-int port)))
	  (vector-set! program position int)
	  ;; (display int) (newline)
	  (read-loop (1+ position) program)))))

(define outputs (make-vector 100 0))
(define output-idx 0)
(define (put-output! value)
  (vector-set! outputs output-idx value)
  (set! output-idx (1+ output-idx)))
(define (reset-outputs!)
  (set! output-idx 0)
  (let loop ((i 0))
    (if (< i (vector-length outputs))
	(vector-set! outputs i 0))))

(define inputs (make-vector 100 0))
(define input-idx 0)
(define (get-input)
  (let ((value (vector-ref inputs input-idx)))
    (set! input-idx (1+ input-idx))
    value))
(define (reset-inputs!)
  (set! input-idx 0)
  (let loop ((i 0))
    (if (< i (vector-length inputs))
	(vector-set! inputs i 0))))

(format #t "Running 5-1.test\n")
(call-with-input-file "5-1.test"
  (lambda (port)
    (let ((program (read-program port)))
      ;; (display program) (newline)
      (reset-inputs!)
      (reset-outputs!)
      (vector-set! inputs 0 0)
      (eval-program! get-input put-output! program)
      (test = (vector-ref outputs 0) 0))))

(format #t "Running 5-2.test\n")
(call-with-input-file "5-2.test"
  (lambda (port)
    (let ((program (read-program port)))
      ;; (display program) (newline)
      (reset-inputs!)
      (reset-outputs!)
      (vector-set! inputs 0 1)
      (eval-program! get-input put-output! program)
      (test = (vector-ref outputs 0) 1))))

(format #t "Running 5-3.test\n")
(call-with-input-file "5-3.test"
  (lambda (port)
    (let ((program (read-program port)))
      ;; (display program) (newline)
      (reset-inputs!)
      (reset-outputs!)
      (vector-set! inputs 0 8)
      (eval-program! get-input put-output! program)
      (test = (vector-ref outputs 0) 1000))))

(define (vector=? eq? v1 v2 n)
  ;; Compares the first n elements of v1 and v2
  (let loop ((i 0))
    (if (< i n)
	(and (eq? (vector-ref v1 i) (vector-ref v2 i))
	     (loop (1+ i)))
	#t)))

(format #t "Running 9a-1.test\n")
(call-with-input-file "9a-1.test"
  (lambda (port)
    (let* ((program (read-program port))
	   (program-copy (vector-copy program)))
      ;; (display program) (newline)
      (reset-inputs!)
      (reset-outputs!)
      (eval-program! get-input put-output! program)
      (test (lambda (v1 v2) (vector=? = v1 v2 16)) program program-copy)
      )))

(format #t "Running 9a-2.test\n")
(call-with-input-file "9a-2.test"
  (lambda (port)
    (let ((program (read-program port)))
      ;; (display program) (newline)
      (reset-inputs!)
      (reset-outputs!)
      (eval-program! get-input put-output! program)
      (test = (vector-ref outputs 0) 1219070632396864))))

(format #t "Running 9a-3.test\n")
(call-with-input-file "9a-3.test"
  (lambda (port)
    (let ((program (read-program port)))
      ;; (display program) (newline)
      (reset-inputs!)
      (reset-outputs!)
      (eval-program! get-input put-output! program)
      (test = (vector-ref outputs 0) 1125899906842624))))

(format #t "Running 9.input\n")
(call-with-input-file "9.input"
  (lambda (port)
    (let ((program (read-program port)))
      ;; (display program) (newline)
      (reset-inputs!)
      (reset-outputs!)
      (vector-set! inputs 0 1) ;; run in "test mode"
      (eval-program! get-input put-output! program)
      (display (vector-ref outputs 0)) (newline)
      )))
;; (call-with-input-file "5.input"
;;   (lambda (port)
;;     (let ((program (read-program port)))
;;       ;; (display program) (newline)
;;       (eval-program! program))))
