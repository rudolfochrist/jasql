;;;; sqlite.lisp

(defpackage #:jasql.sqlite
  (:use :cl #:jasql)
  (:import-from #:sqlite
                #:sqlite-handle))

(in-package #:jasql.sqlite)


(defmethod insert-returning ((db sqlite-handle) sql &optional parameters)
  (apply #'sqlite:execute-non-query/named db sql parameters)
  (sqlite:last-insert-rowid db))


(defmethod insert-update-delete ((db sqlite-handle) sql &optional parameters)
  (apply #'sqlite:execute-non-query/named db sql parameters))


(defmethod insert-update-delete-many ((db sqlite-handle) sql &optional parameters-list)
  (sqlite:with-transaction db
    (loop for parameters in parameters-list
          do (apply #'sqlite:execute-non-query/named db sql parameters))))


(defmethod execute-script ((db sqlite-handle) sql)
  (sqlite:execute-non-query db sql))


(defmethod select-one-row ((db sqlite-handle) sql &optional parameters)
  (apply #'sqlite:execute-single/named db sql parameters))


(defmethod select ((db sqlite-handle) sql &optional parameters)
  (apply #'sqlite:execute-to-list db sql parameters))
