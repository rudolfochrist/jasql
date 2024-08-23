;;;; sqlite.lisp

(defpackage #:jasql.sqlite
  (:use :cl #:jasql)
  (:import-from #:sqlite
                #:with-transaction
                #:with-open-database)
  (:export
   #:*default-busy-timeout*
   #:sqlite-handle
   #:path
   #:busy-timeout))

(in-package #:jasql.sqlite)

(defclass sqlite-handle ()
  ((path :initarg :path
         :accessor path)
   (busy-timeout :initarg :busy-timeout
                 :initform 5000
                 :accessor busy-timeout)))


(defun escape-parameters (params)
  (labels ((recu (params result)
             (cond
               ((null params)
                (reverse result))
               ((keywordp (first params))
                (recu (rest params)
                      (cons (format nil ":~A" (string-downcase (string (first params)))) result)))
               ((consp (first params))
                (recu (rest params)
                      (cons (recu (first params) '()) result)))
               (t
                (recu (rest params) (cons (first params) result))))))
    (recu params '())))


(defmacro with-escaped-parameters ((parameters) &body body)
  (let ((gparams (gensym "params")))
    `(let* ((,gparams (escape-parameters ,parameters))
            (,(intern (symbol-name parameters)) ,gparams))
       ,@body)))


(defmethod insert-returning ((handle sqlite-handle) sql &optional parameters)
  (with-open-database (db (path handle) :busy-timeout (busy-timeout handle))
    (with-transaction db
      (with-escaped-parameters (parameters)
        (apply #'sqlite:execute-non-query/named db sql parameters))
      (sqlite:last-insert-rowid db))))


(defmethod insert-update-delete ((handle sqlite-handle) sql &optional parameters)
  (with-open-database (db (path handle) :busy-timeout (busy-timeout handle))
    (with-escaped-parameters (parameters)
      (apply #'sqlite:execute-non-query/named db sql parameters))))


(defmethod insert-update-delete-many ((handle sqlite-handle) sql &optional parameters-list)
  (with-open-database (db (path handle) :busy-timeout (busy-timeout handle))
    (with-escaped-parameters (parameters-list)
      (with-transaction db
        (loop for parameters in parameters-list
              do (apply #'sqlite:execute-non-query/named db sql parameters))))))


(defmethod execute-script ((handle sqlite-handle) sql)
  (with-open-database (db (path handle) :busy-timeout (busy-timeout handle))
    (sqlite:execute-non-query db sql)))


(defun make-keyword (string)
  (let ((symbol-name (nstring-upcase string)))
    (or (find-symbol symbol-name :keyword)
        (intern symbol-name :keyword))))


(defmethod select-one-row ((handle sqlite-handle) sql &optional parameters)
  (with-open-database (db (path handle) :busy-timeout (busy-timeout handle))
    (with-escaped-parameters (parameters)
      (let* ((query (sqlite:prepare-statement db sql))
             (fields (mapcar #'make-keyword (sqlite:statement-column-names query))))
        (loop for (pn pv) on parameters by #'cddr
              do (sqlite:bind-parameter query pn pv))
        (when (sqlite:step-statement query)
          (loop for field in fields
                for i from 0
                nconc (list field (sqlite:statement-column-value query i))))))))


(defmethod select ((handle sqlite-handle) sql &optional parameters)
  (with-open-database (db (path handle) :busy-timeout (busy-timeout handle))
    (with-escaped-parameters (parameters)
      (let* ((query (sqlite:prepare-statement db sql))
             (fields (mapcar #'make-keyword (sqlite:statement-column-names query))))
        (loop for (pn pv) on parameters by #'cddr
              do (sqlite:bind-parameter query pn pv))
        (loop for row = (sqlite:step-statement query)
              while row
              collect (loop for field in fields
                            for i from 0
                            nconc (list field (sqlite:statement-column-value query i))))))))
