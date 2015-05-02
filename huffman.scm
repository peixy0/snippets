; Huffman coding

(define (tree? node)
  (eq? 'tree (car node)))

(define (leaf? node)
  (eq? 'leaf (car node)))

(define (weight-of node)
  (cond
    ((leaf? node) (caddr node))
    ((tree? node) (cadddr node))
    (else 0)))

(define (symbol-of leaf)
  (if (leaf? leaf) (cadr leaf)
    '()))

; Compare nodes by their weights
(define (node-op? op la lb)
  (op (weight-of la) (weight-of lb)))

(define (node<? la lb)
  (node-op? < la lb))

(define (node>? la lb)
  (node-op? > la lb))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (make-tree left right)
  (list 'tree left right (+ (weight-of left) (weight-of right))))

(define (left-child tree)
  (if (tree? tree) (cadr tree)
    '()))

(define (right-child tree)
  (if (tree? tree) (caddr tree)
    '()))

; Increase weight of a symbol by 1
(define (incr-symbol symbol leaves)
  (if (null? leaves) (list (make-leaf symbol 1))
    (let ((x (car leaves)))
      (if (eq? symbol (symbol-of x))
        (cons (make-leaf symbol (+ (weight-of x) 1)) (cdr leaves))
        (cons x (incr-symbol symbol (cdr leaves)))))))

; Generate symbol weight from a symbol list
(define (gen-leaves ls)
  (let loop ((r '())
             (ls ls))
    (if (null? ls) r
      (loop (incr-symbol (car ls) r) (cdr ls)))))

; Sort the leaves by their weight
(define (sort-leaves ls)
  (if (null? ls) '()
    (let ((pivot (car ls)))
      (let-values (((a b) (partition (lambda (x) (node<? x pivot)) (cdr ls))))
        (append
          (sort-leaves a)
          (list pivot)
          (sort-leaves b))))))

; Insert a node to an ordered list
(define (insert-node n ls)
  (if (null? ls) (list n)
    (let ((x (car ls)))
      (if (node<? x n)
        (cons x (insert-node n (cdr ls)))
        (cons n ls)))))

; Merge the first 2 nodes to a tree
(define (merge-step ls)
  (cons (make-tree (car ls) (cadr ls))
        (cddr ls)))

; Merge the ordered leaves to a huffman tree
(define (merge ls)
  (if (= 1 (length ls)) (car ls)
    (let ((r (merge-step ls)))
      (merge (insert-node (car r) (cdr r))))))

; Huffman tree
(define (huffman-tree ls)
  (merge (sort-leaves (gen-leaves ls))))

; Create symbol table from a huffman tree
(define (symbol-table tree)
  (let build ((path '())
              (tree tree))
    (if (leaf? tree) (list (list (symbol-of tree) (reverse path)))
      (append
        (build (cons 0 path) (left-child tree))
        (build (cons 1 path) (right-child tree))))))

; Encode single symbol
(define (encode-symbol table symbol)
  (if (null? table) '()
    (let ((t (car table)))
      (if (eq? symbol (car t)) (cadr t)
        (encode-symbol (cdr table) symbol)))))

; Encode a list of symbols
(define (encode table ls)
  (map (lambda (x) (encode-symbol table x)) ls))

; Decode a single symbol
(define (decode-symbol tree ls)
  (if (null? ls) (symbol-of tree)
    (if (= 0 (car ls))
      (decode-symbol (left-child tree) (cdr ls))
      (decode-symbol (right-child tree) (cdr ls)))))

; Decode a list of symbols
(define (decode tree ls)
  (map (lambda (x) (decode-symbol tree x)) ls))

; Output should be a list of input symbols
(let* ((ls (string->list (read)))
       (h (huffman-tree ls))
       (t (symbol-table h))
       (e (encode t ls))
       (d (decode h e)))
  (map display (foldr append '() e))
  (newline)
  (map display d)
  (newline))

