;;;; sqlite.lisp

(fiasco:define-test-package #:jasql.sqlite.test
  (:use #:jasql.sqlite))

(in-package #:jasql.sqlite.test)


(defparameter *test-db* nil)
(defparameter *test-sql-path* (asdf:system-relative-pathname "jasql.sqlite" "t/test.sql"))


(jasql:load-sql *test-sql-path*)


(defun initialize-environemt ()
  (setf *test-db* (sqlite:connect ":memory:")))


(defun finalize-environment ()
  (sqlite:disconnect *test-db*)
  (setf *test-db* nil))
