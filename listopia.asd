#|
  This file is a part of listopia project.
  Copyright (c) 2017 Ito Dimercel (xolcman@gmail.com)
|#

#|
  Author: Ito Dimercel (xolcman@gmail.com)
|#

(defsystem "listopia"
  :version "0.1.0"
  :author "Ito Dimercel"
  :license "LLGPL"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "listopia"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "listopia-test"))))
