(asdf:defsystem #:cave
  :serial t
  :description "Description of your cave system"
  :author "Your Name"
  :license "Specify your license here"
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:caveman2
               #:clack
               #:cl-dbi
               #:cl-json
               #:cl-ppcre
               #:datafly
               #:djula
               #:envy
               #:hunchentoot
               #:iterate
               #:local-time
               #:parse-number
               #:postmodern
               #:str)
  :components ((:module "src"
                        :serial t
                        :components ((:file "config")
                                     (:file "main")
                                     (:file "db")
                                     (:file "view")
                                     (:file "web")))
               ;    (:module "tests"
               ;             :serial t
               ;             :components ((:file "cave")))
              ))
