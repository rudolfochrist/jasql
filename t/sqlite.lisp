;;;; sqlite.lisp

(fiasco:define-test-package #:jasql.sqlite.test
  (:use #:jasql.sqlite))

(in-package #:jasql.sqlite.test)


(defparameter *test-db* nil)


(defun setup ()
  (setf *test-db* (sqlite:connect ":memory:")))


(defun teardown ()
  (sqlite:disconnect *test-db*)
  (setf *test-db* nil))

(defun run (&key interactive)
  (setup)
  (run-package-tests :package :jasql.sqlite.test :interactive interactive)
  (teardown))

(defun count-users (db)
  (sqlite:execute-single db "select count(*) from users;"))

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
  (is (= (count-users *test-db*) 3)))


(deftest test-insert-returning ()
  (let* ((id (insert-user-returning *test-db*
                                    :username "lol"
                                    :firstname "Bob"
                                    :lastname "Bobbins"))
         (user (sqlite:execute-to-list
                *test-db*
                "select * from users where _id = ?" id)))
    (is (string= "Bobbins" (fourth (first user))))))


(deftest test-insert-delete ()
  (let  ((count (count-users *test-db*)))
    (insert-user *test-db* :username "frank")
    (is (= (count-users *test-db*) (incf count)))
    (delete-user *test-db* :id 3)
    (is (= (count-users *test-db*) (decf count)))))


(deftest test-update-user ()
  (let ((id (insert-user-returning *test-db* :username "cool" :firstname "McCool")))
    (update-name *test-db* :id id :firstname "SuperCool")
    (is (string= (sqlite:execute-single *test-db* "select firstname from users where _id = ?" id)
                 "SuperCool"))))


(deftest test-get-single-user ()
  (is (not (consp (first (get-single-user *test-db* :id 1))))))


(deftest test-get-all-by-last-name ()
  (is (= 2
         (length (get-all-by-lastname *test-db* :lastname "Miller")))))
