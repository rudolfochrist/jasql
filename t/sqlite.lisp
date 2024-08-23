;;;; sqlite.lisp

(fiasco:define-test-package #:jasql.sqlite.test
  (:use :jasql.sqlite))

(in-package #:jasql.sqlite.test)


(defparameter *test-db-path* (asdf:system-relative-pathname "jasql" "t/test.sqlite3"))

(defparameter *test-db*
  (make-instance 'jasql.sqlite:sqlite-handle :path *test-db-path*))


(defun run (&key interactive)
  (when (probe-file *test-db-path*)
    (delete-file *test-db-path*))
  (run-package-tests :package :jasql.sqlite.test :interactive interactive))

(defun count-users ()
  (sqlite:with-open-database (db (path *test-db*))
    (sqlite:execute-single db "select count(*) from users;")))

;;; tests


(jasql:load-sql "t/sqlite-test.sql" :system "jasql.sqlite")


(deftest test-create-db ()
  (finishes (create-users-table *test-db*)))


(deftest test-insert-many ()
  (bulk-insert-users
   *test-db*
   (list :username "foo" :firstname "Deborah" :lastname "Miller")
   (list :username "pfm" :firstname "Percy" :lastname "Miller")
   (list :username "src" :firstname "Sebastian" :lastname "Christ"))
  (is (= (count-users) 3)))


(deftest test-insert-returning ()
  (let* ((id (insert-user-returning *test-db*
                                    :username "lol"
                                    :firstname "Bob"
                                    :lastname "Bobbins"))
         (user (sqlite:with-open-database (db (path *test-db*))
                 (sqlite:execute-to-list
                  db
                  "select * from users where _id = ?" id))))
    (is (string= "Bobbins" (fourth (first user))))))


(deftest test-insert-delete ()
  (let  ((count (count-users)))
    (insert-user *test-db* :username "frank")
    (is (= (count-users) (incf count)))
    (delete-user *test-db* :id 3)
    (is (= (count-users) (decf count)))))


(deftest test-update-user ()
  (let ((id (insert-user-returning *test-db* :username "cool" :firstname "McCool")))
    (update-name *test-db* :id id :firstname "SuperCool")
    (is (string= (sqlite:with-open-database (db (path *test-db*))
                   (sqlite:execute-single db "select firstname from users where _id = ?" id))
                 "SuperCool"))))


(deftest test-get-single-user ()
  (is (not (consp (first (get-single-user *test-db* :id 1))))))


(deftest test-get-all-by-last-name ()
  (is (= 2
         (length (get-all-by-lastname *test-db* :lastname "Miller")))))
