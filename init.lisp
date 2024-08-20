(require 'asdf)
#+sbcl (require 'sb-aclrepl)

(asdf:initialize-source-registry
 '(:source-registry
   (:directory (:here))
   (:tree (:here "vendor/"))
   (:tree (:here "systems/"))
   :inherit-configuration))
