;;;; srfi-69.lisp

(cl:in-package :srfi-69.internal)

(def-suite srfi-69)

(in-suite srfi-69)

(defvar *default-bound* (- (expt 2 29) 3))

(define-function (%string-hash s ch-conv bound)
  (let ((hash 31)
	(len (string-length s)))
    (do ((index 0 (+ index 1)))
      ((>= index len) (modulo hash bound))
      (set! hash (modulo (+ (* 37 hash)
			    (char->integer (funcall ch-conv (string-ref s index))))
			 *default-bound*)))))

(define-function (string-hash s . maybe-bound)
  (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
    (%string-hash s (lambda (x) x) bound)))

(define-function (string-ci-hash s . maybe-bound)
  (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
    (%string-hash s #'char-downcase bound)))

(define-function (symbol-hash s . maybe-bound)
  (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
    (%string-hash (symbol->string s) (lambda (x) x) bound)))

(define-function (hash obj . maybe-bound)
  (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
    (cond ((integer? obj) (modulo obj bound))
	  ((string? obj) (string-hash obj bound))
	  ((symbol? obj) (symbol-hash obj bound))
	  ((real? obj) (modulo (+ (numerator obj) (denominator obj)) bound))
	  ((number? obj)
	   (modulo (+ (hash (real-part obj)) (* 3 (hash (imag-part obj))))
		   bound))
	  ((char? obj) (modulo (char->integer obj) bound))
	  ((vector? obj) (vector-hash obj bound))
	  ((pair? obj) (modulo (+ (hash (car obj)) (* 3 (hash (cdr obj))))
			       bound))
	  ((null? obj) 0)
	  ((not obj) 0)
	  ((procedure? obj) (error "hash: procedures cannot be hashed" obj))
	  (:else 1))))

(define-function hash-by-identity #'hash)

(define-function (vector-hash v bound)
  (let ((hashvalue 571)
	(len (vector-length v)))
    (do ((index 0 (+ index 1)))
      ((>= index len) (modulo hashvalue bound))
      (set! hashvalue (modulo (+ (* 257 hashvalue) (hash (vector-ref v index)))
			      *default-bound*)))))

(define-function %make-hash-node #'cons)
(define-function %hash-node-set-value! #'set-cdr!)
(define-function %hash-node-key #'car)
(define-function %hash-node-value #'cdr)

(define-record-type <srfi-hash-table>
  (%make-hash-table size hash compare associate entries)
  hash-table?
  (size hash-table-size hash-table-set-size!)
  (hash hash-table-hash-function)
  (compare hash-table-equivalence-function)
  (associate hash-table-association-function)
  (entries hash-table-entries hash-table-set-entries!))

(defvar *default-table-size* 64)

;;; FIXME
(define-function (appropriate-hash-function-for comparison)
  (or (and (eq? comparison #'eq?) #'hash-by-identity)
      (and (eq? comparison #'string=?) #'string-hash)
      (and (eq? comparison #'string-ci=?) #'string-ci-hash)
      #'hash))

(define-function (make-hash-table . args)
  (let* ((comparison (if (null? args) #'equal? (car args)))
	 (hash
	   (if (or (null? args) (null? (cdr args)))
	     (appropriate-hash-function-for comparison) (cadr args)))
	 (size
	   (if (or (null? args) (null? (cdr args)) (null? (cddr args)))
	     *default-table-size* (caddr args)))
	 (association
	   (or (and (eq? comparison #'eq?) #'assq)
	       (and (eq? comparison #'eqv?) #'assv)
	       (and (eq? comparison #'equal?) #'assoc)
	       (letrec
		 ((associate
		    (lambda (val alist)
		      (cond ((null? alist) nil)
			    ((funcall comparison val (caar alist)) (car alist))
			    (:else (associate val (cdr alist)))))))
		 associate))))
    (%make-hash-table 0 hash comparison association (make-vector size '()))))

(define-function (make-hash-table-maker comp hash)
  (lambda args (apply #'make-hash-table (cons comp (cons hash args)))))
(define-function make-symbol-hash-table
  (make-hash-table-maker #'eq? #'symbol-hash))
(define-function make-string-hash-table
  (make-hash-table-maker #'string=? #'string-hash))
(define-function make-string-ci-hash-table
  (make-hash-table-maker #'string-ci=? #'string-ci-hash))
(define-function make-integer-hash-table
  (make-hash-table-maker #'= #'modulo))

(define-function (%hash-table-hash hash-table key)
  (funcall (hash-table-hash-function hash-table)
           key (vector-length (hash-table-entries hash-table))))

(define-function (%hash-table-find entries associate hash key)
  (funcall associate key (vector-ref entries hash)))

(define-function (%hash-table-add! entries hash key value)
  (vector-set! entries hash
	       (cons (%make-hash-node key value)
		     (vector-ref entries hash))))

(define-function (%hash-table-delete! entries compare hash key)
  (declare (optimize (debug 1)))
  (let ((entrylist (vector-ref entries hash)))
    (cond ((null? entrylist) nil)
	  ((funcall compare key (caar entrylist))
	   (vector-set! entries hash (cdr entrylist)) T)
	  (:else
           (labels ((loop (current previous)
                      (cond ((null? current) nil)
                            ((funcall compare key (caar current))
                             (set-cdr! previous (cdr current)) t)
                            (:else (loop (cdr current) current)) )))
             (loop (cdr entrylist) entrylist) )))))

(define-function (%hash-table-walk proc entries)
  (do ((index (- (vector-length entries) 1) (- index 1)))
    ((< index 0)) (for-each proc (vector-ref entries index))))

(define-function (%hash-table-maybe-resize! hash-table)
  (let* ((old-entries (hash-table-entries hash-table))
	 (hash-length (vector-length old-entries)))
    (if (> (hash-table-size hash-table) hash-length)
      (let* ((new-length (* 2 hash-length))
	     (new-entries (make-vector new-length '()))
	     (hash (hash-table-hash-function hash-table)))
	(%hash-table-walk
	  (lambda (node)
	    (%hash-table-add! new-entries
			      (funcall hash (%hash-node-key node) new-length)
			      (%hash-node-key node) (%hash-node-value node)))
	  old-entries)
	(hash-table-set-entries! hash-table new-entries)))))

(define-function (hash-table-ref hash-table key . maybe-default)
  (srfi-61:cond
    ((%hash-table-find (hash-table-entries hash-table)
                       (hash-table-association-function hash-table)
                       (%hash-table-hash hash-table key) key)
     :=> #'%hash-node-value)
    ((null? maybe-default)
     (error "hash-table-ref: no value associated with" key) )
    (:else (funcall (car maybe-default))) ))

(define-function (hash-table-ref/default hash-table key default)
  (hash-table-ref hash-table key (lambda () default)))

(define-function (hash-table-set! hash-table key value)
  (let ((hash (%hash-table-hash hash-table key))
	(entries (hash-table-entries hash-table)) )
    (srfi-61:cond
      ((%hash-table-find entries
                         (hash-table-association-function hash-table)
                         hash key)
       :=> (lambda (node) (%hash-node-set-value! node value)))
      (:else (%hash-table-add! entries hash key value)
             (hash-table-set-size! hash-table
                                   (+ 1 (hash-table-size hash-table)) )
             (%hash-table-maybe-resize! hash-table) ))))

(define-function (hash-table-update! hash-table key function . maybe-default)
  (let ((hash (%hash-table-hash hash-table key))
	(entries (hash-table-entries hash-table)))
    (srfi-61:cond
      ((%hash-table-find entries
                         (hash-table-association-function hash-table)
                         hash key)
	   :=> (lambda (node)
                 (%hash-node-set-value!
                  node (funcall function (%hash-node-value node)))))
      ((null? maybe-default)
       (error "hash-table-update!: no value exists for key" key))
      (:else (%hash-table-add! entries hash key
                               (funcall function
                                        (funcall (car maybe-default))))
             (hash-table-set-size! hash-table
                                   (+ 1 (hash-table-size hash-table)))
             (%hash-table-maybe-resize! hash-table)))))

(define-function (hash-table-update!/default hash-table key function default)
  (hash-table-update! hash-table key function (lambda () default)))

(define-function (hash-table-delete! hash-table key)
  (if (%hash-table-delete! (hash-table-entries hash-table)
			   (hash-table-equivalence-function hash-table)
			   (%hash-table-hash hash-table key) key)
    (hash-table-set-size! hash-table (- (hash-table-size hash-table) 1))))

(define-function (hash-table-exists? hash-table key)
  (and (%hash-table-find (hash-table-entries hash-table)
			 (hash-table-association-function hash-table)
			 (%hash-table-hash hash-table key) key) T))

(define-function (hash-table-walk hash-table proc)
  (%hash-table-walk
    (lambda (node) (funcall proc (%hash-node-key node) (%hash-node-value node)))
    (hash-table-entries hash-table)))

(define-function (hash-table-fold hash-table f acc)
  (hash-table-walk hash-table
		       (lambda (key value) (set! acc (funcall f key value acc))))
  acc)

(define-function (alist->hash-table alist . args)
  (let* ((comparison (if (null? args) #'equal? (car args)))
	 (hash
	   (if (or (null? args) (null? (cdr args)))
	     (appropriate-hash-function-for comparison) (cadr args)))
	 (size
	   (if (or (null? args) (null? (cdr args)) (null? (cddr args)))
	     (max *default-table-size* (* 2 (length alist))) (caddr args)))
	 (hash-table (make-hash-table comparison hash size)))
    (for-each
      (lambda (elem)
	(hash-table-update!/default
	  hash-table (car elem) (lambda (x) x) (cdr elem)))
      alist)
    hash-table))

(define-function (hash-table->alist hash-table)
  (hash-table-fold hash-table
		   (lambda (key val acc) (cons (cons key val) acc)) '()))

(define-function (hash-table-copy hash-table)
  (let ((new (make-hash-table (hash-table-equivalence-function hash-table)
  			      (hash-table-hash-function hash-table)
			      (max *default-table-size*
				   (* 2 (hash-table-size hash-table))))))
    (hash-table-walk hash-table
		     (lambda (key value) (hash-table-set! new key value)))
    new))

(define-function (hash-table-merge! hash-table1 hash-table2)
  (hash-table-walk
    hash-table2
    (lambda (key value) (hash-table-set! hash-table1 key value)))
  hash-table1)

(define-function (hash-table-keys hash-table)
  (hash-table-fold hash-table
                   (lambda (key val acc)
                     (declare (ignore val))
                     (cons key acc))
                   '()))

(define-function (hash-table-values hash-table)
  (hash-table-fold hash-table
                   (lambda (key val acc)
                     (declare (ignore key))
                     (cons val acc) )
                   '() ))

;;; eof
