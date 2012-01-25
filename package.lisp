;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-69
  (:use)
  (:export
   :make-hash-table :hash-table? :alist->hash-table
   :hash-table-equivalence-function :hash-table-hash-function :hash-table-ref
   :hash-table-ref/default :hash-table-set! :hash-table-delete!
   :hash-table-exists? :hash-table-update! :hash-table-update!/default
   :hash-table-size :hash-table-keys :hash-table-values :hash-table-walk
   :hash-table-fold :hash-table->alist :hash-table-copy :hash-table-merge! :hash
   :string-hash :string-ci-hash :hash-by-identity))

(defpackage :srfi-69.internal
  (:use :srfi-69 :cl :fiveam :srfi-23 :srfi-9)
  (:shadowing-import-from :srfi-23 :error)
  (:shadowing-import-from :srfi-69
                          :hash-table-size
                          :make-hash-table)
  (:shadow :lambda :loop :assoc))
