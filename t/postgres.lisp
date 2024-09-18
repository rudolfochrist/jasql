;;;; postgres.lisp

(defpackage #:jasql.postgres.test
  (:use :cl :fiveam :fiveam-matchers :jasql :jasql.postgres))

(in-package #:jasql.postgres.test)

(in-suite* :jasql.postgres.test)

(defvar *test-db*
  (make-instance 'postgres-handle
                 :database "jasqltest"
                 :username "jasql"
                 :password nil
                 :host :unix))


(defun count-users (&optional db)
  (with-postmodern-connection ((or db *test-db*))
    (pomo:query "select count (*) from users;" :single)))

;;; fixtures

(defmacro with-test-db (() &body body)
  `(progn
     (create-users-table *test-db*)
     (unwind-protect (progn ,@body)
       (with-postmodern-connection (*test-db*)
         (pomo:drop-table 'users :if-exists t :cascade t)))))

;;; tests

(jasql:load-sql "t/postgres-test.sql" :system "jasql.postgres")


(test test-insert-many
  (with-test-db ()
    (bulk-insert-users
     *test-db*
     (list :username "foo" :firstname "Deborah" :lastname "Miller")
     (list :username "pfm" :firstname "Percy" :lastname "Miller")
     (list :username "src" :firstname "Sebastian" :lastname "Christ"))
    (assert-that (count-users) (equal-to 3))))


(pomo:defprepared user-for-id
    "select lastname from users where _id = $1"
    :row)

(test test-insert-returning
  (with-test-db ()
    (let* ((id (insert-user-returning *test-db*
                                      :username "lol"
                                      :firstname "Bob"
                                      :lastname "Bobbins"))
           (user (with-postmodern-connection (*test-db*)
                   (user-for-id id))))
      (assert-that (first user) (equal-to "Bobbins")))))


(test test-insert-delete
  (with-test-db ()
    (let  ((count (count-users))
           (id (insert-user-returning *test-db* :username "frank")))
      (assert-that (count-users) (equal-to (incf count)))
      (delete-user *test-db* :id 1)
      (assert-that (count-users) (equal-to (decf count))))))


(test test-update-user
  (with-test-db ()
    (let* ((id (insert-user-returning *test-db* :username "cool" :lastname "McCool"))
           (user
             ))
      (update-name *test-db* :id id :lastname "SuperCool")
      (assert-that (first (with-postmodern-connection (*test-db*)
                            (user-for-id id)))
                   (equal-to "SuperCool")))))


(test test-get-single-user
  (with-test-db ()
    (insert-user *test-db* :username "foo")
    (assert-that (type-of (first (get-single-user *test-db* :id 1)))
                 (is-not (equal-to 'cons)))))


(test test-get-all-by-last-name
  (with-test-db ()
    (bulk-insert-users
     *test-db*
     (list :username "foo" :firstname "Deborah" :lastname "Miller")
     (list :username "pfm" :firstname "Percy" :lastname "Miller")
     (list :username "src" :firstname "Sebastian" :lastname "Christ"))
    (assert-that (length (get-all-by-lastname *test-db* :lastname "Miller"))
                 (equal-to 2))))
