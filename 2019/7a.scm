;; --- Day 7: Amplification Circuit ---
;; 
;; Based on the navigational maps, you're going to need to send more
;; power to your ship's thrusters to reach Santa in time. To do this,
;; you'll need to configure a series of amplifiers already installed on
;; the ship.
;; 
;; There are five amplifiers connected in series; each one receives an
;; input signal and produces an output signal. They are connected such
;; that the first amplifier's output leads to the second amplifier's
;; input, the second amplifier's output leads to the third amplifier's
;; input, and so on. The first amplifier's input value is 0, and the last
;; amplifier's output leads to your ship's thrusters.
;; 
;;     O-------O  O-------O  O-------O  O-------O  O-------O
;; 0 ->| Amp A |->| Amp B |->| Amp C |->| Amp D |->| Amp E |-> (to thrusters)
;;     O-------O  O-------O  O-------O  O-------O  O-------O
;; 
;; The Elves have sent you some Amplifier Controller Software (your
;; puzzle input), a program that should run on your existing Intcode
;; computer. Each amplifier will need to run a copy of the program.
;; 
;; When a copy of the program starts running on an amplifier, it will
;; first use an input instruction to ask the amplifier for its current
;; phase setting (an integer from 0 to 4). Each phase setting is used
;; exactly once, but the Elves can't remember which amplifier needs which
;; phase setting.
;; 
;; The program will then call another input instruction to get the
;; amplifier's input signal, compute the correct output signal, and
;; supply it back to the amplifier with an output instruction. (If the
;; amplifier has not yet received an input signal, it waits until one
;; arrives.)
;; 
;; Your job is to find the largest output signal that can be sent to the
;; thrusters by trying every possible combination of phase settings on
;; the amplifiers. Make sure that memory is not shared or reused between
;; copies of the program.
;; 
;; For example, suppose you want to try the phase setting sequence
;; 3,1,2,4,0, which would mean setting amplifier A to phase setting 3,
;; amplifier B to setting 1, C to 2, D to 4, and E to 0. Then, you could
;; determine the output signal that gets sent from amplifier E to the
;; thrusters with the following steps:
;; 
;;     Start the copy of the amplifier controller software that will run
;;     on amplifier A. At its first input instruction, provide it the
;;     amplifier's phase setting, 3. At its second input instruction, provide
;;     it the input signal, 0. After some calculations, it will use an output
;;     instruction to indicate the amplifier's output signal.
;; 
;;     Start the software for amplifier B. Provide it the phase setting
;;     (1) and then whatever output signal was produced from amplifier A. It
;;     will then produce a new output signal destined for amplifier C.
;; 
;;     Start the software for amplifier C, provide the phase setting (2)
;;     and the value from amplifier B, then collect its output signal.
;; 
;;     Run amplifier D's software, provide the phase setting (4) and
;;     input value, and collect its output signal.
;; 
;;     Run amplifier E's software, provide the phase setting (0) and
;;     input value, and collect its output signal.
;; 
;; The final output signal from amplifier E would be sent to the
;; thrusters. However, this phase setting sequence may not have been the
;; best one; another sequence might have sent a higher signal to the
;; thrusters.
;; 
;; Here are some example programs:
;; 
;;     Max thruster signal 43210 (from phase setting sequence 4,3,2,1,0):
;; 
;;     3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0
;; 
;;     Max thruster signal 54321 (from phase setting sequence 0,1,2,3,4):
;; 
;;     3,23,3,24,1002,24,10,24,1002,23,-1,23,
;;     101,5,23,23,1,24,23,23,4,23,99,0,0
;; 
;;     Max thruster signal 65210 (from phase setting sequence 1,0,4,3,2):
;; 
;;     3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
;;     1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0
;; 
;; Try every combination of phase settings on the amplifiers. What is the
;; highest signal that can be sent to the thrusters?


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

(define (eval-instr! program i get-input put-output)
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
	 ((= opcode 3) ;; in
	  (let ((op1 (vector-ref program (+ i 1)))
		(value (get-input)))
	    ;; (format #t "set! program[~a]=~a\n" op1 value)
	    (vector-set! program op1 value)
	    2))
	 ((= opcode 4) ;; out
	  (begin
	    (put-output op1)
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

;; (call-with-input-file "5-1.test"
;; (call-with-input-file "5-2.test"
;; (call-with-input-file "5-3.test"
;;   (lambda (port)
;;     (let ((program (read-program port)))
;;       ;; (display program) (newline)
;;       (eval-program! program))))

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
  (not (or
	(= a b)
	(= a c)
	(= a d)
	(= a e)
	(= b c)
	(= b d)
	(= b e)
	(= c d)
	(= c e)
	(= d e))))

(define (maximize-output program)
  (let inputs-loop ((a 0)
		    (b 0)
		    (c 0)
		    (d 0)
		    (e 0)
		    (max-input '(0 0 0 0 0))
		    (max-output 0))
    (set! output 0)
    (set! input-idx 0)
    (vector-set! inputs 0 a)
    (vector-set! inputs 2 b)
    (vector-set! inputs 4 c)
    (vector-set! inputs 6 d)
    (vector-set! inputs 8 e)
    (let ((program (vector-copy program)))
      (vector-set! inputs 1 output)
      (eval-program! program get-input put-output)
      (vector-set! inputs 3 output)
      (eval-program! program get-input put-output)
      (vector-set! inputs 5 output)
      (eval-program! program get-input put-output)
      (vector-set! inputs 7 output)
      (eval-program! program get-input put-output)
      (vector-set! inputs 9 output)
      (eval-program! program get-input put-output))

    (match-let (((max-input . max-output)
		 (if (and (> output max-output) (unique-phases? a b c d e))
		     (cons (list a b c d e) output)
		     (cons max-input max-output))))
      ;; (format #t "~a~a~a~a~a → ~a\n" a b c d e output)
      (cond
       ((< e 4) (inputs-loop a b c d (1+ e) max-input max-output))
       ((< d 4) (inputs-loop a b c (1+ d) 0 max-input max-output))
       ((< c 4) (inputs-loop a b (1+ c) 0 0 max-input max-output))
       ((< b 4) (inputs-loop a (1+ b) 0 0 0 max-input max-output))
       ((< a 4) (inputs-loop (1+ a) 0 0 0 0 max-input max-output))
       (else (cons max-input max-output))))))

(display "test1") (newline)
(let ((program (call-with-input-file "7-1.test" read-program)))
  (match-let (((i . max) (maximize-output program)))
    (display (cons i max)) (newline)
    (test = max 43210)
    (test equal? i '(4 3 2 1 0))))

(display "test2") (newline)
(let ((program (call-with-input-file "7-2.test" read-program)))
  (match-let (((i . max) (maximize-output program)))
    (display (cons i max)) (newline)
    (test = max 54321)
    (test equal? i '(0 1 2 3 4))))

(display "test3") (newline)
(let ((program (call-with-input-file "7-3.test" read-program)))
  (match-let (((i . max) (maximize-output program)))
    (display (cons i max)) (newline)
    (test = max 65210)
    (test equal? i '(1 0 4 3 2))))

(let ((program (call-with-input-file "7.input" read-program)))
  (match-let (((i . max) (maximize-output program)))
    (format #t "setting=~a output=~a\n" i max)))
 
;; 65210 too low
