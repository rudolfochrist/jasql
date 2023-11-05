;;;; jasql.asd

(defsystem "jasql"
  :author "Sebastian Christ <rudolfo.christ@pm.me>"
  :maintainer "Sebastian Christ <rudolfo.christ@pm.me>"
  :mailto "rudolfo.christ@pm.me"
  :license "MPL-2.0"
  :homepage "https://github.com/rudolfochrist/jasql"
  :bug-tracker "https://github.com/rudolfochrist/jasql/issues"
  :source-control (:git "https://github.com/rudolfochrist/jasql.git")
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
