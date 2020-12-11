;;;; tests.lisp

(defpackage #:jasql.test
  (:use :cl #:jasql
        ))

(in-package #:jasql.test)

(load-sql #.(merge-pathnames
             "t/users.sql"
             (asdf:system-source-directory "jasql")))
