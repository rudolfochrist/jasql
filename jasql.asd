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
     (uiop:subpathname *load-pathname* "README.txt")))
