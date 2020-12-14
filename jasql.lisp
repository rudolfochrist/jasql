;;;; jasql.lisp

(in-package #:jasql)


(defvar *query-name-definition-pattern*
  (ppcre:create-scanner "--\\s*name\\s*:\\s*")
  "Match name definition comments.")


(defvar *empty-pattern*
  (ppcre:create-scanner "^\\s*$")
  "Match empty lines.")


(defvar *newline-pattern*
  (ppcre:create-scanner "\\n")
  "Match newlines.")


(defvar *doc-comment-pattern*
  (ppcre:create-scanner "\\s*--\\s*(.*)$")
  "Match SQL comments.")


(defvar *parameter-pattern*
  (ppcre:create-scanner ":(\\w+)")
  "Match parameters starting with a colon, e.g. :id or :user_name.")


(define-condition jasql-error (simple-error)
  ())


(defun read-file (file)
  "Read complete file into memory from STREAM."
  (with-open-file (stream file)
    (with-output-to-string (out)
      (loop for line = (read-line stream nil nil)
            while line
            do (write-line line out)))))


(defun negativep (number)
  "T if NUMBER smaller than 0."
  (< number 0))


(defun slice (string &key (start 0) (end (length string)))
  "Remove length of STRING - END."
  (let ((s (if (negativep start)
               (+ (length string) start)
               start))
        (e (if (negativep end)
               (+ (length string) end)
               end)))
    (subseq string s e)))


(defun extract-parameters (sql)
  "Extract parameters used in SQL."
  (let ((symbols '()))
    (ppcre:do-register-groups (param)
        (*parameter-pattern* sql (nreverse symbols))
      (push (make-symbol (string-upcase param)) symbols))))


(defun create-name-method-pair (name)
  "Map name to adapter method.

Suffix -> Method:
<! -> `insert-returning'
*! -> `insert-update-delete-many'
 ! -> `insert-update-delete'
 # -> `execute-script'
 ? -> `select-one-row'

No suffix resolves to `select'"
  (cond
    ((ends-with-p name "<!")
     (list (slice name :end -2)
           'insert-returning))
    ((ends-with-p name "*!")
     (list (slice name :end -2)
           'insert-update-delete-many))
    ((ends-with-p name "!")
     (list (slice name :end -1)
           'insert-update-delete))
    ((ends-with-p name "#")
     (list (slice name :end -1)
           'execute-script))
    ((ends-with-p name "?")
     (list (slice name :end -1)
           'select-one-row))
    (t
     (list name 'select))))


(defun extract-query-details (query-string)
  "Split the QUERY-STRING into its details.

Details is a triple of name, method, sql, docstring."
  (loop with query = (ppcre:split *newline-pattern* query-string)
        with comments = '()
        with statements = '()
        for line in (rest query)
        do (multiple-value-bind (matchp groups)
               (ppcre:scan-to-strings *doc-comment-pattern* line)
             (if matchp
                 (push (aref groups 0) comments)
                 (push line statements)))
        finally (return (append (create-name-method-pair (first query))
                                (list (format nil "~{~A~^ ~}" (nreverse statements))
                                      (format nil "~{~A~^ ~}" (nreverse comments)))))))


(defun ends-with-p (string suffix)
  "Verify if STRING ends with SUFFIX."
  (string= string suffix
           :start1 (- (length string) (length suffix))))


(defmacro load-sql (path)
  "Load SQL and generate DB access functions.

PATH is either a SQL file or a directory that contains SQL files."
  (setf path (etypecase path
               (symbol (symbol-value path))
               ((or pathname string) path)))
  (unless (probe-file path)
    (error 'jasql-error
           :format-control "File ~A not found."
           :format-arguments (list path)))
  (let ((sql-files (if (uiop:file-pathname-p path)
                       (list path)
                       (uiop:directory-files path "*.sql"))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(loop for sql-file in sql-files
               for raw-sql = (read-file sql-file)
               append (loop with raw-queries = (ppcre:split *query-name-definition-pattern* raw-sql)
                            for raw-query in raw-queries
                            unless (ppcre:scan *empty-pattern* raw-query)
                              collect (destructuring-bind (name method sql doc)
                                          (extract-query-details raw-query)
                                        (let ((params (extract-parameters sql))
                                              (gsym (gensym "sym")))
                                          `(let ((,gsym (intern (string-upcase ,name))))
                                             (setf (symbol-function ,gsym)
                                                   (lambda (db
                                                            ,@(unless (eql method 'execute-script)
                                                                `(&rest parameters))
                                                            ,@(unless (or (eql method 'insert-update-delete-many)
                                                                          (null params))
                                                                (cons '&key params)))
                                                     ,@(when (and params
                                                                  (not (eql method 'insert-update-delete-many)))
                                                         `((declare (ignorable ,@params))))
                                                     (,method db
                                                              (get ',gsym :sql "")
                                                              ,@(unless (eql method 'execute-script)
                                                                  '(parameters)))))
                                             (setf (documentation ,gsym 'function) ,doc)
                                             (setf (get ',gsym :sql) ,sql)
                                             ;; return nothing
                                             (values)))))))))


(defun sql (symbol)
  "Return SQL used by SYMBOL."
  (get symbol :sql))


(defun docstring (symbol)
  "Return docstring of SYMBOL."
  (documentation symbol 'function))

