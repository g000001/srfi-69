;;;; package.lisp

(cl:in-package :cl-user)


(defpackage "https://github.com/g000001/srfi-69"
  (:use)
  (:export
   make-hash-table hash-table? alist->hash-table
   hash-table-equivalence-function hash-table-hash-function
   hash-table-ref hash-table-ref/default hash-table-set!
   hash-table-delete!  hash-table-exists? hash-table-update!
   hash-table-update!/default hash-table-size hash-table-keys
   hash-table-values hash-table-walk hash-table-fold hash-table->alist
   hash-table-copy hash-table-merge! hash string-hash string-ci-hash
   hash-by-identity))


(defpackage "https://github.com/g000001/srfi-69#internals"
  (:use "https://github.com/g000001/srfi-69"
        "https://github.com/g000001/srfi-9"
        "https://github.com/g000001/srfi-23"
        "https://github.com/g000001/srfi-61"
        cl
        fiveam)
  (:shadowing-import-from "https://github.com/g000001/srfi-23"
                          error)
  (:shadowing-import-from "https://github.com/g000001/srfi-61"
                          cond)
  (:shadowing-import-from "https://github.com/g000001/srfi-69"
                          hash-table-size
                          make-hash-table)
  (:shadow lambda loop assoc))


;;; *EOF*
