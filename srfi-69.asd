;;;; srfi-69.asd

(cl:in-package :asdf)


(defsystem :srfi-69
  :version "20200229"
  :description "SRFI 69 for CL: Basic hash tables"
  :long-description "SRFI 69 for CL: Basic hash tables
https://srfi.schemers.org/srfi-69"
  :author "CHIBA Masaomi"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:fiveam :srfi-23 :srfi-9 :srfi-61)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-69")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-69))))
  (let ((name "https://github.com/g000001/srfi-69")
        (nickname :srfi-69))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-69))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-69#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-69)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
