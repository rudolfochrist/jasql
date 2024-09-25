(asdf:load-systems "sb-texinfo" "jasql" "jasql.sqlite" "jasql.postgres")

(sb-texinfo:document-packages (list :jasql :jasql.sqlite :jasql.postgres) "jasql"
                              :output-file "doc/dict.texi"
                              :standalone nil :write-backmatter nil :write-menu nil :exclude-node t)


