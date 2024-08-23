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
  (sqlite:with-transaction db
    (with-escaped-parameters (parameters)
      (apply #'sqlite:execute-non-query/named db sql parameters))
    (sqlite:last-insert-rowid db)))


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


(defun make-keyword (string)
  (let ((symbol-name (nstring-upcase string)))
    (or (find-symbol symbol-name :keyword)
        (intern symbol-name :keyword))))


(defmethod select-one-row ((db sqlite-handle) sql &optional parameters)
  (with-escaped-parameters (parameters)
    (let* ((query (sqlite:prepare-statement db sql))
           (fields (mapcar #'make-keyword (sqlite:statement-column-names query))))
      (loop for (pn pv) on parameters by #'cddr
            do (sqlite:bind-parameter query pn pv))
      (when (sqlite:step-statement query)
        (loop for field in fields
              for i from 0
              nconc (list field (sqlite:statement-column-value query i)))))))


(defmethod select ((db sqlite-handle) sql &optional parameters)
  (with-escaped-parameters (parameters)
    (let* ((query (sqlite:prepare-statement db sql))
           (fields (mapcar #'make-keyword (sqlite:statement-column-names query))))
      (loop for (pn pv) on parameters by #'cddr
            do (sqlite:bind-parameter query pn pv))
      (loop for row = (sqlite:step-statement query)
            while row
            collect (loop for field in fields
                          for i from 0
                          nconc (list field (sqlite:statement-column-value query i)))))))
