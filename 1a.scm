

(use-modules (ice-9 binary-ports)
	     (ice-9 rdelim)
	     (srfi srfi-1))


(define (read-nums port nums)
  (let ((line (read-line port)))
    (if (eof-object? line)
	nums
	(read-nums port (cons
			 (string->number (string-trim-right line))
			 nums)))))


(display (fold + 0 (read-nums (current-input-port) '())))
