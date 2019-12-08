;; --- Part Two ---
;; 
;; Now, you just need to figure out how many orbital transfers you (YOU)
;; need to take to get to Santa (SAN).
;; 
;; You start at the object YOU are orbiting; your destination is the
;; object SAN is orbiting. An orbital transfer lets you move from any
;; object to an object orbiting or orbited by that object.
;; 
;; For example, suppose you have the following map:
;; 
;; COM)B
;; B)C
;; C)D
;; D)E
;; E)F
;; B)G
;; G)H
;; D)I
;; E)J
;; J)K
;; K)L
;; K)YOU
;; I)SAN
;; 
;; Visually, the above map of orbits looks like this:
;; 
;;                           YOU
;;                          /
;;         G - H       J - K - L
;;        /           /
;; COM - B - C - D - E - F
;;                \
;;                 I - SAN
;; 
;; In this example, YOU are in orbit around K, and SAN is in orbit around
;; I. To move from K to I, a minimum of 4 orbital transfers are required:
;; 
;;     K to J
;;     J to E
;;     E to D
;;     D to I
;; 
;; Afterward, the map of orbits looks like this:
;; 
;;         G - H       J - K - L
;;        /           /
;; COM - B - C - D - E - F
;;                \
;;                 I - SAN
;;                  \
;;                   YOU
;; 
;; What is the minimum number of orbital transfers required to move from
;; the object YOU are orbiting to the object SAN is orbiting? (Between
;; the objects they are orbiting - not between YOU and SAN.)



(use-modules (ice-9 textual-ports)
	     (ice-9 rdelim)
	     (ice-9 match)
	     (srfi srfi-1)
	     ((rnrs) :version (6)))

;; Two bodies can orbit the same body
;; An orbiting body can be orbited

(define (build-graph orbits)
  ;; Given a list of pairs ((a . b) ...), meaning b orbits a,
  ;; build a graph representing which bodies orbit which bodies.
  (let loop ((orbits orbits)
	     (graph '()))
    ;; (display graph) (newline)
    (if (null? orbits)
	graph
	(match-let (((parent . child) (car orbits)))
	  (let* ((child-ls (assq-ref graph parent))
		 (graph'
		  (if child-ls
		      (assq-set! graph parent (cons child child-ls))
		      (assq-set! graph parent (list child)))))
	    (loop (cdr orbits) graph'))))))

(define test-orbits
  '(("com" . "b") ("b" . "c") ("c" . "d") ("d" . "e") ("e" . "f") ("b" . "g")
    ("g" . "h") ("d" . "i") ("e" . "j") ("j" . "k") ("k" . "l")))

(let* ((actual (build-graph test-orbits))
       (expected '(("k" "l") ("j" "k") ("g" "h") ("e" "j" "f")
		   ("d" "i" "e") ("c" "d") ("b" "g" "c") ("com" "b"))))
  (assert (equal? actual expected)))

(define (count-direct-orbits graph)
  ;; Given ((a . (b c)) ...), where the first cell means c and b
  ;; directly orbit a, count the total number of direct orbits.
  (if (null? graph)
      0
      (+ (length (cdar graph)) (count-direct-orbits (cdr graph)))))

(let ((actual (count-direct-orbits (build-graph test-orbits)))
      (expected 11))
  ;; (format #t "actual=~a expected=~a\n" actual expected)
  (assert (eqv? actual expected)))

(define (get-direct-orbits graph parent)
  ;; (format #t "~a ~a\n" graph parent)
  (let ((kv (assq-ref graph parent)))
    (if kv
	kv
	'())))

(define (count-direct-and-indirect-orbits graph node)
  (let ((children (get-direct-orbits graph node)))
    ;; (display children) (newline)
    (if (null? children)
	0
	(+ (length children)
	   (fold +
		 0
		 (map (lambda (node)
			(count-direct-and-indirect-orbits graph node))
		      children))))))

(let ((graph '((a b c) (b d))))
  ;; (display (count-direct-and-indirect-orbits graph 'a)) (newline)
  (assert (eqv? (count-direct-and-indirect-orbits graph 'a) 3)))


(define (count-indirect-orbits graph)
  ;; Given ((a . (b c)) (b . (d)) ...), where d indirectly orbits a
  ;; (because d orbits b and b orbits a), count all direct and
  ;; indirect orbits
  (let loop ((graph' graph))
    (if (null? graph')
	0
	(let ((parent (caar graph'))
	      (children (cdar graph')))
	  (+
	   (fold +
		 0
		 (map (lambda (child)
			(count-direct-and-indirect-orbits graph child))
		      children))
	   (loop (cdr graph')))))))

(let ((actual (count-indirect-orbits (build-graph test-orbits)))
      (expected 31))
  ;; (format #t "actual=~a expected=~a\n" actual expected)
  (assert (eqv? actual expected)))

(let* ((graph (build-graph test-orbits))
       (direct (count-direct-orbits graph))
       (indirect (count-indirect-orbits graph)))
  (assert (eqv? (+ direct indirect) 42)))

(define (read-orbits port)
  (let loop ((orbits '()))
    (let ((line (read-line port 'trim)))
      (if (eof-object? line)
	  orbits
	  (loop (cons line orbits))))))

(define (parse-orbit str)
  (match-let (((parent child) (string-split str #\))))
    (let ((pair (cons parent child)))
      ;; (display pair)
      pair)))


;; (define (build-matrix graph)
;;   (let* ((nodes (list->vector (map car graph)))
;; 	 (matrix
;; 	  (let ((matrix (make-vector (vector-length nodes))))
;; 	    (let vector-fill-loop ((i 0))
;; 	      (if (< i (vector-length nodes))
;; 		  (begin
;; 		    (vector-set! matrix i (make-vector (vector-length nodes) 0))
;; 		    (vector-fill-loop (1+ i)))
;; 		  matrix)))))
;;     (display graph) (newline)
;;     (display matrix) (newline)
;;     (let populate-matrix-loop ((graph graph))
;;       (if (null? graph)
;; 	  matrix
;; 	  (match-let (((parent . children) (car graph)))
;; 	    (define (find-node-idx nodes node node-idx)
;; 	      (if (eq? (vector-ref nodes node-idx) node)
;; 		  node-idx
;; 		  (find-node-idx nodes node (1+ node-idx))))
;; 	    (define (set-cell! matrix nodes noder nodec)
;; 	      (let ((node-idxr (find-node-idx nodes noder 0))
;; 		    (node-idxc (find-node-idx nodes nodec 0)))
;; 		(vector-set! (vector-ref matrix node-idxr) node-idxc 1)))
;; 	    (let loop ((children children))
;; 	      (if (null? children)
;; 		  matrix
;; 		  (let ((child (car children)))
;; 		    (set-cell! matrix nodes parent child)
;; 		    (loop (cdr children))))))))))

(define (get-nodes orbits)
  ;; Given a list of orbits, return a vector of unique nodes (orbital
  ;; bodies).
  (let loop ((nodes '())
	     (orbits orbits))
    (if (null? orbits)
	(list->vector nodes)
	(match-let (((parent . child) (car orbits)))
	  (let ((nodes (if (member parent nodes)
			   nodes
			   (cons parent nodes))))
	    (let ((nodes (if (member child nodes)
			     nodes
			     (cons child nodes))))
	      (loop nodes (cdr orbits))))))))

(define (test actual expected eq?)
  (if (not (eq? actual expected))
      (begin
	(format #t "FAIL: actual=~a expected=~a\n" actual expected)
	(assert #f))))

(let ((actual (vector->list (get-nodes test-orbits)))
      (expected '("com" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l")))
  (test actual expected (lambda (a b) (lset= equal? a b)))
  )

(define (node-idx nodes node)
  (let ((n (vector-length nodes)))
   (let loop ((i 0))
     (cond
      ((= i n) #f)
      ((equal? (vector-ref nodes i) node) i)
      (else (loop (1+ i)))))))

;; could just substitude orbits with their positions in the nodes
;; vector
(define (matrix-ref matrix nodes rnode cnode)
  (let ((ridx (node-idx nodes rnode))
	(cidx (node-idx nodes cnode)))
    (array-ref matrix ridx cidx)))

(define (matrix-set! matrix nodes rnode cnode value)
  (let ((ridx (node-idx nodes rnode))
	(cidx (node-idx nodes cnode)))
    (array-set! matrix value ridx cidx)))

(define (build-matrix orbits nodes)
  (let* ((n (vector-length nodes))
	 (matrix (make-array 0 n n)))
    (let loop ((orbits orbits))
      (if (null? orbits)
	  matrix
	  (match-let (((parent . child) (car orbits)))
	    (matrix-set! matrix nodes parent child 1)
	    (loop (cdr orbits)))))))

(define (display-matrix matrix)
  ;; assumes the matrix is square
  (let* ((n (array-length matrix)))
    (let row-loop ((row 0))
      (if (< row n)
	  (begin
	    (let col-loop ((col 0))
	      (if (< col n)
		  (begin
		    (format #t "~a " (array-ref matrix row col))
		    (col-loop (1+ col)))))
	    (newline)
	    (row-loop (1+ row)))))))

(let* ((nodes (get-nodes test-orbits))
       (matrix (build-matrix test-orbits nodes)))
  (display nodes) (newline)
  (display-matrix matrix))

(define (parent-indices matrix child-col)
  (let ((n (array-length matrix)))
      (let row-loop ((row 0))
	(if (< row n)
	    (if (> (array-ref matrix row child-col) 0)
		(cons row (row-loop (1+ row)))
		(row-loop (1+ row)))
	    '()))))

(define (child-indices matrix parent-row)
  (let ((n (array-length matrix)))
      (let col-loop ((col 0))
	(if (< col n)
	    (if (> (array-ref matrix parent-row col) 0)
		(cons col (col-loop (1+ col)))
		(col-loop (1+ col)))
	    '()))))

(let* ((nodes (get-nodes test-orbits))
       (matrix (build-matrix test-orbits nodes)))
  (assert (lset= eq?
		 (parent-indices matrix (node-idx nodes "k"))
		 (list (node-idx nodes "j"))))
  (assert (lset= eq?
		 (child-indices matrix (node-idx nodes "d"))
		 (list (node-idx nodes "i") (node-idx nodes "e")))))


(define (orbit-bfs matrix nodes start-node end-node)
  (let ((start-idx (node-idx nodes start-node))
	(end-idx (node-idx nodes end-node))
	(seen (make-vector (vector-length nodes) #f)))
    (format #t "start ~a (~a). end ~a (~a).\n" start-node start-idx end-node end-idx)
    (let loop ((q (list (cons 0 start-idx))) ;; queue contains (distance . node-idx)
	       )
      ;; intentionally don't check for null q--this means no path
      ;; between start and end exists, which is an error
      (match-let (((distance . curr-idx) (car q)))
	(if (vector-ref seen curr-idx)
	    (loop (cdr q))
	    (begin
	      (vector-set! seen curr-idx 1)
	      (if (= curr-idx end-idx)
		  distance
		  (let* ((parents (parent-indices matrix curr-idx))
			 (children (child-indices matrix curr-idx))
			 (q'
			  (append (cdr q)
				  (map (lambda (n) (cons (1+ distance) n))
				       parents)
				  (map (lambda (n) (cons (1+ distance) n))
				       children))))
		    ;; (display (map
		    ;; 	      (lambda (ent)
		    ;; 		(cons (car ent) (vector-ref nodes (cdr ent))))
		    ;; 	      q))
		    ;; (newline)
		    ;; (display (cons distance (vector-ref nodes curr-idx))) (newline)
		    (loop q'))))))
      )))


(let* ((nodes (get-nodes test-orbits))
       (matrix (build-matrix test-orbits nodes))
       (actual (orbit-bfs matrix nodes "k" "i")))
  (test actual 4 =))




;; (display (build-matrix (build-graph test-orbits))) (newline)

(let* ((orbits (map parse-orbit (call-with-input-file "6.input" read-orbits)))
       (nodes (get-nodes orbits))
       (matrix (build-matrix orbits nodes))
       )
  (format #t "number of nodes=~a\n" (vector-length nodes))
  (display (- (orbit-bfs matrix nodes "YOU" "SAN") 2)) (newline))
