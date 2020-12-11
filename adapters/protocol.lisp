;;;; protocol.lisp

(in-package #:jasql)


(defgeneric insert-returning (db sql &optional parameters)
  (:documentation "Run INSERT statement and return auto-generated
  value.

An auto-generated value can be the (e.g.) ID of the last inserted
value."))


(defgeneric insert-update-delete (db sql &optional parameters)
  (:documentation "Run an INSERT/UPDATE/DELETE statement."))


(defgeneric insert-update-delete-many (db sql &optional parameters-list)
  (:documentation "Run an INSERT/UPDATE/DELETE statement in bulk.

PARAMETERS-LIST is a list of parameters that would have been passed to
`insert-update-delete'."))


(defgeneric execute-script (db sql)
  (:documentation "Run SQL without any variable substitution."))


(defgeneric select-one-row (db sql &optional parameters)
  (:documentation "Run SELECT and return a single row."))


(defgeneric select (db sql &optional parameters)
  (:documentation "Run SELECT statement."))



