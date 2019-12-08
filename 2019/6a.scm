;; --- Day 6: Universal Orbit Map ---
;; 
;; You've landed at the Universal Orbit Map facility on Mercury. Because
;; navigation in space often involves transferring between orbits, the
;; orbit maps here are useful for finding efficient routes between, for
;; example, you and Santa. You download a map of the local orbits (your
;; puzzle input).
;; 
;; Except for the universal Center of Mass (COM), every object in space
;; is in orbit around exactly one other object. An orbit looks roughly
;; like this:
;; 
;;                   \
;;                    \
;;                     |
;;                     |
;; AAA--> o            o <--BBB
;;                     |
;;                     |
;;                    /
;;                   /
;; 
;; In this diagram, the object BBB is in orbit around AAA. The path that
;; BBB takes around AAA (drawn with lines) is only partly shown. In the
;; map data, this orbital relationship is written AAA)BBB, which means
;; "BBB is in orbit around AAA".
;; 
;; Before you use your map data to plot a course, you need to make sure
;; it wasn't corrupted during the download. To verify maps, the Universal
;; Orbit Map facility uses orbit count checksums - the total number of
;; direct orbits (like the one shown above) and indirect orbits.
;; 
;; Whenever A orbits B and B orbits C, then A indirectly orbits C. This
;; chain can be any number of objects long: if A orbits B, B orbits C,
;; and C orbits D, then A indirectly orbits D.
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
;; 
;; Visually, the above map of orbits looks like this:
;; 
;;         G - H       J - K - L
;;        /           /
;; COM - B - C - D - E - F
;;                \
;;                 I
;; 
;; In this visual representation, when two objects are connected by a
;; line, the one on the right directly orbits the one on the left.
;; 
;; Here, we can count the total number of orbits as follows:
;; 
;;     D directly orbits C and indirectly orbits B and COM, a total of 3
;;     orbits.
;; 
;;     L directly orbits K and indirectly orbits J, E, D, C, B, and COM,
;;     a total of 7 orbits.
;; 
;;     COM orbits nothing.
;; 
;; The total number of direct and indirect orbits in this example is 42.
;; 
;; What is the total number of direct and indirect orbits in your map
;; data?


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
  '((com . b) (b . c) (c . d) (d . e) (e . f) (b . g)
    (g . h) (d . i) (e . j) (j . k) (k . l)))

(let* ((actual (build-graph test-orbits))
       (expected '((k l) (j k) (g h) (e j f) (d i e) (c d) (b g c) (com b))))
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
  (match-let (((parent child) (map string->symbol (string-split str #\)))))
    (cons parent child)))

(let* ((orbits (map parse-orbit (call-with-input-file "6.input" read-orbits)))
       (graph (build-graph orbits))
       (direct (count-direct-orbits graph))
       (indirect (count-indirect-orbits graph)))
  (display graph) (newline)
  (display (+ direct indirect)) (newline))
