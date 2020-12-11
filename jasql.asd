;;;; jasql.asd

(defsystem "jasql"
  :author "Sebastian Christ <rudolfo.christ@pm.me>"
  :maintainer "Sebastian Christ <rudolfo.christ@pm.me>"
  :license "LGPL"
  :homepage "https://github.com/rudolfochrist/jasql"
  :version (:read-file-line "version")
  :depends-on ("cl-ppcre"
               (:require "uiop"))
  :components ((:file "package")
               (:file "jasql")
               (:module "adapters"
                :components ((:file "protocol"))))
  :description "Simple SQL in Common Lisp. A direct port of Python's anosql."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.txt"))
  :in-order-to ((test-op (test-op jasql/test))))


(defvar *test-interactive* nil)

(defsystem "jasql/test"
  :depends-on ("uiop"
               "fiasco"
               "jasql")
  :pathname "t/"
  :components ((:file "tests")
               (:static-file "users.sql"))
  :perform (test-op (op c)
                    (uiop:symbol-call
                     :fiasco
                     :run-package-tests
                     :package :jasql.test
                     :interactive *test-interactive*)))

(export '*test-interactive*)
