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
                                     (:file "main")
                                     (:file "db")
                                     (:file "view")
                                     (:file "web"))))
  :in-order-to ((test-op (test-op :cave/tests))))

(asdf:defsystem :cave/tests
  :serial t
  :description "Description of your cave system"
  :author "Your Name"
  :license "Specify your license here"
  :depends-on (:cave :rove)
  :components ((:module "tests"
                        :serial t
                        :components ((:file "cave"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
