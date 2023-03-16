;;;; postgres.lisp

(defpackage #:jasql.postgres
  (:use :cl #:jasql)
  (:import-from #:postmodern
                #:query
                #:prepare)
  (:export
   #:postgres-handle
   #:with-postmodern-connection
   #:with-prepared-statement))

(in-package #:jasql.postgres)


(defclass postgres-handle ()
  ((database :initarg :database
             :accessor psql-database)
   (username :initarg :username
             :initform ""
             :accessor psql-username)
   (password :initarg :password
             :initform ""
             :accessor psql-password)
   (host :initarg :host
         :initform "localhost"
         :accessor psql-host)
   (port :initarg :port
         :initform 5432
         :accessor psql-port)
   (pooled-p :initarg :pooled-p
             :initform nil
             :accessor psql-pooled-p)
   (use-ssl :initarg :use-ssl
            :initform :try
            :accessor psql-use-ssl)
   (service :initarg :service
            :initform "postgres"
            :accessor psql-service)
   (application-name :initarg :application-name
                     :initform ""
                     :accessor psql-application-name)
   (use-binary :initarg :use-binary
               :initform nil
               :accessor psql-use-binary)))

(defmethod print-object ((handle postgres-handle) stream)
  (with-accessors ((db psql-database) (role psql-username)) handle
    (print-unreadable-object (handle stream :type t)
      (format stream "~A>~A" db role))))


(defmethod spec ((db postgres-handle))
  (with-accessors ((database psql-database)
                   (username psql-username)
                   (password psql-password)
                   (host psql-host)
                   (port psql-port)
                   (pooled-p psql-pooled-p)
                   (use-ssl psql-use-ssl)
                   (service psql-service)
                   (application-name psql-application-name)
                   (use-binary psql-use-binary))
      db
    `(,database ,username ,password ,host
                :port ,port
                :pooled-p ,pooled-p
                :use-ssl ,use-ssl
                :service ,service
                :application-name ,application-name
                :use-binary ,use-binary)))


(defmacro with-postmodern-connection ((db) &body body)
  `(postmodern:call-with-connection (spec ,db) (lambda () ,@body)))


(defvar *terminals* (list #\Space #\Tab #\Newline #\, #\; #\( #\)))


(defun terminalp (char)
  (member char *terminals*))


(defun make-keyword (string)
  (intern (string-upcase string) :keyword))


(defun make-string-buffer ()
  (make-array '(0)
              :element-type 'base-char
              :fill-pointer 0
              :adjustable t))


(defun process-sql (sql)
  (let ((fstr (make-string-buffer)))
    (with-output-to-string (s fstr)
      (loop with mapping = '()
            with placeholderp = nil
            with placeholder = nil
            with num = 1
            for c across sql
            when (eql c #\:)
              do (setf placeholderp t
                       placeholder (make-string-buffer))
            when (and placeholderp (terminalp c))
              do (progn
                   (setf placeholderp nil)
                   (push (cons num (make-keyword placeholder))
                         mapping)
                   (format s "$~D" num)
                   (incf num))
            if placeholderp
              do (unless (eql c #\:)    ; skip :
                   (vector-push-extend c placeholder))
            else
              do (write-char c s)
            finally (return (values fstr (sort mapping #'< :key #'car)))))))


(defmacro with-prepared-statement ((statement args) (sql parameters &optional format) &body body)
  (let ((gstmt (gensym "stmt"))
        (granking (gensym "ranking")))
    `(multiple-value-bind (,gstmt ,granking)
         (process-sql ,sql)
       (let ((,statement (prepare ,gstmt ,(when format `,format)))
             (,args (mapcar (lambda (rank)
                              (getf ,parameters (cdr rank)))
                            ,granking)))
         ,@body))))


;;; protocol implementation

(defmethod insert-returning ((db postgres-handle) sql &optional parameters)
  (with-postmodern-connection (db)
    (with-prepared-statement (func params)
        (sql parameters :row)
      (apply func params))))


(defmethod insert-update-delete ((db postgres-handle) sql &optional parameters)
  (with-postmodern-connection (db)
    (with-prepared-statement (func params)
        (sql parameters :none)
      (apply func params))))


(defmethod insert-update-delete-many ((db postgres-handle) sql &optional parameters-list)
  (with-postmodern-connection (db)
    (loop for parameters in parameters-list
          do (with-prepared-statement (func params)
                 (sql parameters :none)
               (apply func params)))))

(defmethod execute-script ((db postgres-handle) sql)
  (with-postmodern-connection (db)
    (query sql)))


(defmethod select-one-row ((db postgres-handle) sql &optional parameters)
  (with-postmodern-connection (db)
    (with-prepared-statement (func params)
        (sql parameters :row)
      (apply func params))))


(defmethod select ((db postgres-handle) sql &optional parameters)
  (with-postmodern-connection (db)
    (with-prepared-statement (func params)
        (sql parameters :rows)
      (apply func params))))


