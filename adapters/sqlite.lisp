;;;; sqlite.lisp

(defpackage #:jasql.sqlite
  (:use :cl #:jasql)
  (:import-from #:sqlite
                #:with-transaction
                #:with-open-database)
  (:export
   #:sqlite-handle
   #:path
   #:busy-timeout
   #:make-handle))

(in-package #:jasql.sqlite)

(defclass sqlite-handle ()
  ((path :initarg :path
         :accessor path
         :documentation "Path to the SQLite database.")
   (busy-timeout :initarg :busy-timeout
                 :initform 5000
                 :accessor busy-timeout
                 :documentation "Busy timeout for this connection."))
  (:documentation "Wraps a database connection to a SQLite database."))

(defun make-handle (&key path busy-timeout)
  "Create a fresh database handle"
  (make-instance 'sqlite-handle :path path :busy-timeout busy-timeout))

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
      (insert-returning db sql parameters))))

(defmethod insert-returning ((handle sqlite:sqlite-handle) sql &optional parameters)
  (with-escaped-parameters (parameters)
    (apply #'sqlite:execute-non-query/named handle sql parameters))
  (sqlite:last-insert-rowid handle))


(defmethod insert-update-delete ((handle sqlite-handle) sql &optional parameters)
  (with-open-database (db (path handle) :busy-timeout (busy-timeout handle))
    (insert-update-delete db sql parameters)))

(defmethod insert-update-delete ((handle sqlite:sqlite-handle) sql &optional parameters)
  (with-escaped-parameters (parameters)
    (apply #'sqlite:execute-non-query/named handle sql parameters)))


(defmethod insert-update-delete-many ((handle sqlite-handle) sql &optional parameters-list)
  (with-open-database (db (path handle) :busy-timeout (busy-timeout handle))
    (with-transaction db
      (insert-update-delete-many db sql parameters-list))))


(defmethod insert-update-delete-many ((handle sqlite:sqlite-handle) sql &optional parameters-list)
  (with-escaped-parameters (parameters-list)
    (loop for parameters in parameters-list
          do (apply #'sqlite:execute-non-query/named handle sql parameters))))


(defmethod execute-script ((handle sqlite-handle) sql)
  (with-open-database (db (path handle) :busy-timeout (busy-timeout handle))
    (execute-script db sql)))

(defmethod execute-script ((handle sqlite:sqlite-handle) sql)
  (sqlite:execute-non-query handle sql))


(defun make-keyword (string)
  (let ((symbol-name (nstring-upcase string)))
    (or (find-symbol symbol-name :keyword)
        (intern symbol-name :keyword))))


(defmethod select-one-row ((handle sqlite-handle) sql &optional parameters)
  (with-open-database (db (path handle) :busy-timeout (busy-timeout handle))
    (select-one-row db sql parameters)))

(defmethod select-one-row ((handle sqlite:sqlite-handle) sql &optional parameters)
  (with-escaped-parameters (parameters)
    (let* ((query (sqlite:prepare-statement handle sql))
           (fields (mapcar #'make-keyword (sqlite:statement-column-names query))))
      (loop for (pn pv) on parameters by #'cddr
            do (sqlite:bind-parameter query pn pv))
      (when (sqlite:step-statement query)
        (loop for field in fields
              for i from 0
              nconc (list field (sqlite:statement-column-value query i)))))))


(defmethod select ((handle sqlite-handle) sql &optional parameters)
  (with-open-database (db (path handle) :busy-timeout (busy-timeout handle))
    (select db sql parameters)))

(defmethod select ((handle sqlite:sqlite-handle) sql &optional parameters)
  (with-escaped-parameters (parameters)
    (let* ((query (sqlite:prepare-statement handle sql))
           (fields (mapcar #'make-keyword (sqlite:statement-column-names query))))
      (loop for (pn pv) on parameters by #'cddr
            do (sqlite:bind-parameter query pn pv))
      (loop for row = (sqlite:step-statement query)
            while row
            collect (loop for field in fields
                          for i from 0
                          nconc (list field (sqlite:statement-column-value query i)))))))
