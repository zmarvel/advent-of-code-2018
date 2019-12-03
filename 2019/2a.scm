;; On the way to your gravity assist around the Moon, your ship computer
;; beeps angrily about a "1202 program alarm". On the radio, an Elf is
;; already explaining how to handle the situation: "Don't worry, that's
;; perfectly norma--" The ship computer bursts into flames.
;;
;; You notify the Elves that the computer's magic smoke seems to have
;; escaped. "That computer ran Intcode programs like the gravity assist
;; program it was working on; surely there are enough spare parts up
;; there to build a new Intcode computer!"
;;
;; An Intcode program is a list of integers separated by commas (like
;; 1,0,0,3,99). To run one, start by looking at the first integer (called
;; position 0). Here, you will find an opcode - either 1, 2, or 99. The
;; opcode indicates what to do; for example, 99 means that the program is
;; finished and should immediately halt. Encountering an unknown opcode
;; means something went wrong.
;;
;; Opcode 1 adds together numbers read from two positions and stores the
;; result in a third position. The three integers immediately after the
;; opcode tell you these three positions - the first two indicate the
;; positions from which you should read the input values, and the third
;; indicates the position at which the output should be stored.
;;
;; For example, if your Intcode computer encounters 1,10,20,30, it should
;; read the values at positions 10 and 20, add those values, and then
;; overwrite the value at position 30 with their sum.
;;
;; Opcode 2 works exactly like opcode 1, except it multiplies the two
;; inputs instead of adding them. Again, the three integers after the
;; opcode indicate where the inputs and outputs are, not their values.
;;
;; Once you're done processing an opcode, move to the next one by
;; stepping forward 4 positions.
;;
;; ... some explanation omitted ...
;;
;; Here are the initial and final states of a few more small programs:
;;
;;     1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2).
;;     2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6).
;;     2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801).
;;     1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99.
;;
;; ;; Once you have a working computer, the first step is to restore the
;; gravity assist program (your puzzle input) to the "1202 program alarm"
;; state it had just before the last computer caught fire. To do this,
;; before running the program, replace position 1 with the value 12 and
;; replace position 2 with the value 2. What value is left at position 0
;; after the program halts?

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

(let ((program (read-program)))
  (vector-set! program 1 12)
  (vector-set! program 2 2)
  (eval-program! program)
  (display program) (newline))
