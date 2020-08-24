(ql:quickload :cl-json)
(ql:quickload :jonathan)
(ql:quickload :json-streams)
(ql:quickload :jsown)
(ql:quickload :shasht)
(ql:quickload :st-json)
(ql:quickload :yason)
(ql:quickload :cl-spark)
(ql:quickload :the-cost-of-nothing)

(defparameter +json-string+ "{\"key1\":\"value\",\"key2\":1,\"key3\":[\"Hello\",1.2e-34]}")

(defparameter +json-parsers+
  (list
    'cl-json:decode-json-from-string
    'jojo:parse
    'json-streams:json-parse
    'jsown:parse
    'shasht:from-json
    'st-json:read-json
    'yason:parse))

(let ((benches (mapcar (lambda (fun)
                         (the-cost-of-nothing:benchmark (funcall (symbol-function fun) +json-string+)))
                       +json-parsers+)))
  (write-line (cl-spark:vspark benches
                               :title "JSON Read Times"
                               :min 0
                               :labels (mapcar (lambda (sym) (package-name (symbol-package sym))) +json-parsers+))))


