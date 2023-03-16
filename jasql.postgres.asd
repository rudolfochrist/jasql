;;;; jasql.postgres.asd

(defsystem "jasql.postgres"
  :author "Sebastian Christ <rudolfo.christ@pm.me>"
  :maintainer "Sebastian Christ <rudolfo.christ@pm.me>"
  :license "MPL-2.0"
  :homepage "https://github.com/rudolfochrist/jasql"
  :version (:read-file-line "version")
  :depends-on ("cl+ssl"
               "postmodern"
               "jasql"
               "cl-ppcre")
  :pathname "adapters/"
  :components ((:file "postgres"))
  :description "PostgreSQL adadpter for jasql."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.txt"))
  :in-order-to ((test-op (test-op jasql.postgres/test))))


(defsystem "jasql.postgres/test"
  :depends-on ("uiop"
               "fiasco"
               "jasql.postgres")
  :pathname "t/"
  :components ((:file "postgres")
               (:static-file "postrges-test.sql"))
  :perform (test-op (op c)
                    (uiop:symbol-call
                     :jasql.postgres.test
                     :run)))

