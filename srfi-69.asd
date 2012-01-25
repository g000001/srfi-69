;;;; srfi-69.asd

(cl:in-package :asdf)

(defsystem :srfi-69
  :serial t
  :depends-on (:fiveam :srfi-23 :srfi-9 :srfi-61)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-69")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-69))))
  (load-system :srfi-69)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-69.internal :srfi-69))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
