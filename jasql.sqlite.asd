
;;;; jasql.sqlite.asd

(defvar *test-interactive* nil)
(export '*test-interactive*)

(defsystem "jasql.sqlite"
  :author "Sebastian Christ <rudolfo.christ@pm.me>"
  :maintainer "Sebastian Christ <rudolfo.christ@pm.me>"
  :license "LGPL"
  :homepage "https://github.com/rudolfochrist/jasql"
  :version (:read-file-line "version")
  :depends-on ("sqlite"
               "jasql")
  :pathname "adapters/"
  :components ((:file "sqlite"))
  :description "Use jasql with SQLite."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.txt"))
  :in-order-to ((test-op (load-op "jasql.sqlite/test")))
  :perform (test-op (op c)
                    (uiop:symbol-call
                     :jasql.sqlite.test
                     :run
                     :interactive *test-interactive*)))

(defsystem "jasql.sqlite/test"
  :depends-on ("uiop"
               "fiasco"
               "jasql.sqlite")
  :pathname "t/"
  :components ((:file "sqlite")
               (:static-file "test.sql")))


