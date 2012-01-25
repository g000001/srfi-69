(cl:in-package :srfi-69.internal)

(def-suite srfi-69)

(in-suite srfi-69)

(defmacro isqu (x y)
  `(is (equal ,x ,y)))

(defvar *test-alist*
  (cl:loop :for i :from 0 :to 200
     :collect (cons (gensym "KEY-") i)))

(test |Type constructors and predicate|
  (is-true (hash-table? (make-hash-table)))
  (isqu (let ((ht (make-hash-table)))
          (hash-table-set! ht :foo 1)
          (hash-table-set! ht :bar 2)
          (hash-table-fold ht #'list* '()) )
        '(:FOO 1 :BAR 2) )
  (isqu (let ((ht (alist->hash-table '((:foo . 1)
                                       (:bar . 2)
                                       (:baz . 3) ))))
          (hash-table-set! ht :foo 100)
          (hash-table-set! ht :bar 200)
          (hash-table-fold ht #'list* '()) )
        '(:FOO 100 :BAR 200 :BAZ 3) )
  (isqu '()
        (set-difference
         (hash-table-fold (alist->hash-table *test-alist*) #'acons '())
         *test-alist*
         :test #'equal)))

(test |Reflective queries|
  (is-true (eq #'equal?
               (hash-table-equivalence-function
                (make-hash-table))))
  (is-true (eq #'string=
               (hash-table-equivalence-function
                (make-hash-table #'string=))))
  (is-true (eq #'hash
               (hash-table-hash-function (make-hash-table))))
  (let ((hash (lambda (&rest args) (apply #'hash args))))
    (is-true (eq hash
                 (hash-table-hash-function (make-hash-table #'equal? hash))))))

(test |Dealing with single elements|
  (let ((tab (make-hash-table))
        (val 1) )
    (hash-table-set! tab :foo val)
    (is (= val (hash-table-ref tab :foo))) )
  ;;
  (let ((tab (make-hash-table))
        (val 1) )
    (hash-table-set! tab :foo 1)
    (is (= val (hash-table-ref/default tab :bar 1))) )
  ;;
  (let ((tab (make-hash-table)) )
    (hash-table-set! tab :foo 1)
    (hash-table-delete! tab :foo)
    (is (eq :default (hash-table-ref/default tab :foo :default))) )
  ;;
  (let ((tab (make-hash-table)) )
    (hash-table-set! tab :foo 1)
    (hash-table-delete! tab :foo)
    (is-false (hash-table-exists? tab :foo)))
  ;;
  (let ((tab (make-hash-table)) )
    (hash-table-set! tab :foo 1)
    (hash-table-update! tab :foo (constantly :new))
    (eq :new (hash-table-ref tab :foo)))
  ;;
  (let ((tab (make-hash-table)) )
    (hash-table-update!/default tab :foo #'values :old)
    (eq :old (hash-table-ref tab :foo))))

(test |Dealing with the whole contents|
  (is (= (length *test-alist*)
         (hash-table-size (alist->hash-table *test-alist*)) ))
  (isqu '()
        (set-difference (hash-table-keys (alist->hash-table *test-alist*))
                        (mapcar #'car *test-alist*)
                        :test #'equal))
  (isqu '()
        (set-difference (hash-table-values (alist->hash-table *test-alist*))
                        (mapcar #'cdr *test-alist*)
                        :test #'equal))
  (isqu '()
        (set-difference *test-alist*
                        (let ((ans '()))
                          (hash-table-walk
                           (alist->hash-table *test-alist*)
                           (lambda (k v) (push (cons k v) ans)) )
                          ans )
                        :test #'equal))
  (isqu '()
        (set-difference *test-alist*
                        (let ((ans '()))
                          (hash-table-walk
                           (hash-table-copy
                            (alist->hash-table *test-alist*))
                           (lambda (k v) (push (cons k v) ans)) )
                          ans )
                        :test #'equal))
  (let ((tab (alist->hash-table *test-alist*)))
    (isqu '()
          (set-difference *test-alist*
                          (let ((ans '()))
                            (hash-table-walk
                             (hash-table-merge! tab (hash-table-copy tab))
                             (lambda (k v) (push (cons k v) ans)) )
                            ans )
                          :test #'equal))))

(test |Hashing|
  (is (= (hash "foo")
         (hash "foo")))
  (is (= (string-hash "foo")
         (string-hash "foo")))
  (is (= (string-ci-hash "Foo")
         (string-ci-hash "fOo")
         (string-ci-hash "fOO")))
  (is (= (hash-by-identity "foo")
         (hash-by-identity "foo"))))

;;; eof
