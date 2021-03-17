(defsystem "listopia"
  :version "0.12.0"
  :author "Ito Dimercel"
  :license "LLGPL"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "listopia"))))
  :description "List manipulation library"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "listopia/tests"))))

(defsystem "listopia/tests"
  :class :package-inferred-system
  :depends-on ("listopia"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "listopia"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
