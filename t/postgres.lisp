;;;; postgres.lisp

(fiasco:define-test-package #:jasql.postgres.test
    (:use #:jasql.postgres))

(in-package #:jasql.postgres.test)

(defvar *test-db* nil)

(defun setup ()
  (setf *test-db* (make-instance 'postgres-handle
                                 :database "jasqltest"
                                 :username "jasql"
                                 :password nil
                                 :host :unix
                                 :port 15432)))


(defun teardown ()
  (with-postmodern-connection (*test-db*)
    (pomo:query "drop table users;")))


(defun run (&key interactive)
  (setup)
  (unwind-protect
       (run-package-tests :package :jasql.postgres.test :interactive interactive)
    (teardown)))


(defun count-users (db)
  (with-postmodern-connection (db)
    (pomo:query "select count (*) from users;" :single)))

;;; tests

(jasql:load-sql "t/postgres-test.sql" :system "jasql.postgres")

(deftest test-creat-db ()
  (finishes (create-users-table *test-db*)))


(deftest test-insert-many ()
  (bulk-insert-users
   *test-db*
   (list :username "foo" :firstname "Deborah" :lastname "Miller")
   (list :username "pfm" :firstname "Percy" :lastname "Miller")
   (list :username "src" :firstname "Sebastian" :lastname "Christ"))
  (is (= (count-users *test-db*) 3)))


(pomo:defprepared user-for-id
    "select lastname from users where _id = $1"
    :row)

(deftest test-insert-returning ()
  (let* ((id (first
              (insert-user-returning *test-db*
                                     :username "lol"
                                     :firstname "Bob"
                                     :lastname "Bobbins")))
         (user (with-postmodern-connection (*test-db*)
                 (user-for-id id))))
    (is (string= "Bobbins" (first user)))))


(deftest test-insert-delete ()
  (let  ((count (count-users *test-db*)))
    (insert-user *test-db* :username "frank")
    (is (= (count-users *test-db*) (incf count)))
    (delete-user *test-db* :id 3)
    (is (= (count-users *test-db*) (decf count)))))


(deftest test-update-user ()
  (let ((id (first (insert-user-returning *test-db* :username "cool" :lastname "McCool"))))
    (update-name *test-db* :id id :lastname "SuperCool")
    (is (string= (first (with-postmodern-connection (*test-db*)
                          (user-for-id id)))
                 "SuperCool"))))


(deftest test-get-single-user ()
  (is (not (consp (first (get-single-user *test-db* :id 1))))))


(deftest test-get-all-by-last-name ()
  (is (= 2
         (length (get-all-by-lastname *test-db* :lastname "Miller")))))
