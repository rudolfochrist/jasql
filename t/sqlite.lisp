;;;; sqlite.lisp

(defpackage #:jasql.sqlite.test
  (:use :cl :fiveam :fiveam-matchers :jasql.sqlite))

(in-package #:jasql.sqlite.test)

(in-suite* :jasql.sqlite.test)

(defparameter *test-db-path* (asdf:system-relative-pathname "jasql" "t/test.sqlite3"))

(defparameter *test-db*
  (make-instance 'jasql.sqlite:sqlite-handle :path *test-db-path*))


(defun count-users ()
  (sqlite:with-open-database (db (path *test-db*))
    (sqlite:execute-single db "select count(*) from users;")))

;;; fixtures

(defmacro with-test-db (() &body body)
  `(unwind-protect (progn
                     (create-test-db)
                     ,@body)
     (when (probe-file *test-db-path*)
       (delete-file *test-db-path*))))

;;; tests

(jasql:load-sql "t/sqlite-test.sql" :system "jasql.sqlite")


(defun create-test-db ()
  (create-users-table *test-db*))


(test test-insert-many
  (with-test-db ()
    (bulk-insert-users
     *test-db*
     (list :username "foo" :firstname "Deborah" :lastname "Miller")
     (list :username "pfm" :firstname "Percy" :lastname "Miller")
     (list :username "src" :firstname "Sebastian" :lastname "Christ"))
    (assert-that (count-users) (equal-to 3))))


(test test-insert-returning
  (with-test-db ()
    (let* ((id (insert-user-returning *test-db*
                                      :username "lol"
                                      :firstname "Bob"
                                      :lastname "Bobbins"))
           (user (sqlite:with-open-database (db (path *test-db*))
                   (sqlite:execute-to-list
                    db
                    "select * from users where _id = ?" id))))
      (assert-that (fourth (first user)) (equal-to "Bobbins")))))


(test test-insert-delete
  (with-test-db ()
    (let  ((count (count-users))
           (frank (insert-user-returning *test-db* :username "frank")))
      (assert-that (count-users) (equal-to (incf count)))
      (delete-user *test-db* :id frank)
      (assert-that (count-users) (equal-to (decf count))))))


(defun user-firstname (id)
  (sqlite:with-open-database (db (path *test-db*))
    (sqlite:execute-single db "select firstname from users where _id = ?" id)))

(test test-update-user
  (with-test-db ()
    (let ((id (insert-user-returning *test-db* :username "cool" :firstname "McCool")))
      (update-name *test-db* :id id :firstname "SuperCool")
      (assert-that (user-firstname id) (equal-to "SuperCool")))))


(test test-get-single-user
  (with-test-db ()
    (assert-that (type-of (first (get-single-user *test-db* :id 1))) (is-not (equal-to 'cons)))))


(test test-get-all-by-last-name
  (with-test-db ()
    (bulk-insert-users
     *test-db*
     (list :username "foo" :firstname "Deborah" :lastname "Miller")
     (list :username "pfm" :firstname "Percy" :lastname "Miller")
     (list :username "src" :firstname "Sebastian" :lastname "Christ"))
    (assert-that (length (get-all-by-lastname *test-db* :lastname "Miller"))
                 (equal-to 2))))

(test test-transactions
  (with-test-db ()
    (ignore-errors
     (sqlite:with-open-database (db (path *test-db*))
       (sqlite:with-transaction db
         (insert-user db :username "foo")
         (insert-user db :username "foo"))))
    (assert-that (count-users) (equal-to 0))))
