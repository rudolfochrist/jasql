;;;; package.lisp

(defpackage #:jasql
  (:use :cl)
  (:export
   #:load-sql
   #:sql
   #:docstring

   ;; adapter protocol
   #:insert-returning
   #:insert-update-delete
   #:insert-update-delete-many
   #:execute-script
   #:select-one-row
   #:select
   #:paramaters))
