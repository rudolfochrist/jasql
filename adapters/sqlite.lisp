;;;; sqlite.lisp

(defpackage #:jasql.sqlite
  (:use :cl #:jasql)
  (:import-from #:sqlite
                #:sqlite-handle))

(in-package #:jasql.sqlite)


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


(defmethod insert-returning ((db sqlite-handle) sql &optional parameters)
  (with-escaped-parameters (parameters)
    (apply #'sqlite:execute-non-query/named db sql parameters))
  (sqlite:last-insert-rowid db))


(defmethod insert-update-delete ((db sqlite-handle) sql &optional parameters)
  (with-escaped-parameters (parameters)
    (apply #'sqlite:execute-non-query/named db sql parameters)))


(defmethod insert-update-delete-many ((db sqlite-handle) sql &optional parameters-list)
  (with-escaped-parameters (parameters-list)
    (sqlite:with-transaction db
      (loop for parameters in parameters-list
            do (apply #'sqlite:execute-non-query/named db sql parameters)))))


(defmethod execute-script ((db sqlite-handle) sql)
  (sqlite:execute-non-query db sql))


(defmethod select-one-row ((db sqlite-handle) sql &optional parameters)
  (with-escaped-parameters (parameters)
    (let ((result (apply #'sqlite:execute-to-list/named db sql parameters)))
      (when (=  1 (length result))
        (first result)))))


(defmethod select ((db sqlite-handle) sql &optional parameters)
  (with-escaped-parameters (parameters)
    (apply #'sqlite:execute-to-list/named db sql parameters)))
