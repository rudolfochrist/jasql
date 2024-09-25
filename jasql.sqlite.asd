
;;;; jasql.sqlite.asd


(defsystem "jasql.sqlite"
  :author "Sebastian Christ <rudolfo.christ@pm.me>"
  :maintainer "Sebastian Christ <rudolfo.christ@pm.me>"
  :license "MPL-2.0"
  :homepage "https://github.com/rudolfochrist/jasql"
  :version (:read-file-line "version")
  :depends-on ("sqlite"
               "jasql")
  :pathname "adapters/"
  :components ((:file "sqlite"))
  :description "Use jasql with SQLite."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.org"))
  :in-order-to ((test-op (load-op "jasql.sqlite/test")))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call :5am :run! :jasql.sqlite.test)
                      #-(or swank slynk)
                      (error "Tests failed."))))

(defsystem "jasql.sqlite/test"
  :depends-on ("uiop"
               "fiveam"
               "fiveam-matchers"
               "jasql.sqlite")
  :pathname "t/"
  :components ((:file "sqlite")
               (:static-file "test.sql")))


