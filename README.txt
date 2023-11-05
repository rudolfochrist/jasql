1 NAME
======

  jasql --- Simple SQL in Common Lisp. A direct port of Python's anosql.


2 VERSION
=========

  0.5


3 SYNOPSIS
==========

  On the SQL side:
  ,----
  | -- name: get-all-articles
  | -- Retreive all articles.
  | SELECT * FROM articles;
  | 
  | -- name: add-new-article!
  | -- Adds a new articles.
  | INSERT INTO articles
  | (title, author)
  | VALUES
  | (:title, :author);
  `----

  On the Lisp side:
  ,----
  | (jasql:load-sql "sql/articles.sql" :system "blog")
  | 
  | (add-new-article *db* :title "On lisp" :author "paulg")
  | (get-all-articles *db*)
  `----


4 DESCRIPTION
=============

  `jasql' is a direct port of [anosql] from Python to Common Lisp. A
  [Yesql]-style SQL library.

  For more documentation read the Info file.


[anosql] <https://github.com/honza/anosql>

[Yesql] <https://github.com/krisajenkins/yesql/>

4.1 INSTALLATION
~~~~~~~~~~~~~~~~

  `jasql' is not in Quicklisp. Either put it into QL's local projects or
  to any other place where QL/ASDF can find it.

  You can run `make install' to install it to `/usr/local/lisp/jasql' or
  any other `prefix' ASDF knows about.


5 AUTHOR
========

  Sebastian Christ (<mailto:rudolfo.christ@pm.me>)


6 LICENSE
=========

  Released under the MPL-2.0 license.


7 SEE ALSO
==========

  - <https://github.com/honza/anosql>
  - <https://github.com/ruricolist/cl-yesql>
