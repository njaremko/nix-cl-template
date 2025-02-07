(asdf:defsystem :cave
  :serial t
  :description "Description of your cave system"
  :author "Your Name"
  :license "Specify your license here"
  :depends-on (:alexandria
               :bordeaux-threads
               :caveman2
               :cl-json
               :cl-ppcre
               :cl-yesql
               :clack
               :com.inuoe.jzon
               :dexador
               :djula
               :envy
               :fset
               :iterate
               :ironclad
               :local-time
               :make-hash
               :parse-number
               :postmodern
               :str
               :uuid
               :woo)
  :components ((:module "src"
                        :serial t
                        :components ((:file "config")
                                     (:file "reader")
                                     (:file "main")
                                     (:file "stripe")
                                     (:file "db")
                                     (:file "view")
                                     (:file "auth")
                                     (:file "web"))))
  :in-order-to ((test-op (test-op :cave/tests))))

(asdf:defsystem :cave/tests
  :serial t
  :description "Description of your cave system"
  :author "Your Name"
  :license "Specify your license here"
  :depends-on (:cave :fiveam :check-it)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "cave")
                                     (:module "suites"
                                              :serial nil
                                              :components ((:file "example-suite")
                                                         (:file "reader-suite"))))))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :cave-suite :cave/test))))
