;;;; protocol.lisp

(in-package #:jasql)

(defclass adapter ()
  ((connection :accessor connection
               :initform nil
               :documentation "The database connection.")
   (connection-spec :accessor connection-spec
                    :initform nil
                    :initarg :connection-spec
                    :documentation "Details to establish a database connection."))
  (:documentation "Abstract database adapter."))


(defgeneric connect (adapter &optional connection-spec)
  (:documentation "Establish a connection to the database.

If CONNECTION-SPEC is nil it uses the connection specification (or
connection string) from the adapter."))


(defgeneric insert-returning (adapter sql parameters)
  (:documentation "Run INSERT statement and return auto-generated
  value.

An auto-generated value can be the (e.g.) ID of the last inserted
value."))


(defgeneric insert-update-delete (adapter sql parameters)
  (:documentation "Run an INSERT/UPDATE/DELETE statement."))


(defgeneric insert-update-delete-many (adapter sql parameters-list)
  (:documentation "Run an INSERT/UPDATE/DELETE statement in bulk.

PARAMETERS-LIST is a list of parameters that would have been passed to
`insert-update-delete'."))


(defgeneric execute-script (adapter sql)
  (:documentation "Run SQL without any variable substitution."))


(defgeneric select-one-row (adapter sql parameters)
  (:documentation "Run SELECT and return a single row."))


(defgeneric select (adapter sql parameters)
  (:documentation "Run SELECT statement."))



