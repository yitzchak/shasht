(asdf:defsystem #:shasht
  :description "More JSON encoding/decoding"
  :version "0.1"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on
    (:alexandria)
  :components
    ((:module src
      :serial t
      :components
        ((:file "packages")
         (:file "read")
         (:file "write")))))
