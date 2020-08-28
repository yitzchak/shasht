(ql:quickload '(:cl-json :jonathan :json-streams :jsown :shasht :st-json :yason :cl-spark :the-cost-of-nothing))

(defparameter +json-string+ "{\"key1\":\"value\\n\",\"key2\":1,\"key3\":[\"Hello \\u2604\",1.2e-34,true,false,null]}")

(defun cl-json/r ()
  "cl-json"
  (cl-json:decode-json-from-string +json-string+))


(defun jonathan/r ()
  "jonathan"
  (jojo:parse +json-string+ :as :alist))


(defun json-streams/r ()
  "json-streams"
  (json-streams:json-parse +json-string+))


(defun jsown/r ()
  "jsown"
  (jsown:parse +json-string+))


(defun shasht/r ()
  "shasht"
  (shasht:from-json +json-string+))


(defun st-json/r ()
  "st-json"
  (st-json:read-json +json-string+))


(defun yason/r ()
  "yason"
  (yason:parse +json-string+))


(defparameter +json-parsers+
  (list #'cl-json/r
        #'jonathan/r
        #'json-streams/r
        #'jsown/r
        #'shasht/r
        #'st-json/r
        #'yason/r))


(write-line
  (cl-spark:vspark
    (mapcar (lambda (fun)
              (the-cost-of-nothing:benchmark (funcall fun)))
            +json-parsers+)
    :title "JSON Read Times"
    :min 0
    :size 80
    :labels (mapcar (lambda (fun) (documentation fun 'function)) +json-parsers+)))


(defparameter +json-alist-list+
  '(("key1" . "value")
    ("key2" . 1)
    ("key3" . ("Hello" 1.2d-34))))


(defparameter +json-hash-list+
  (alexandria:alist-hash-table +json-alist-list+))


(defparameter +jsown-obj+
  (cons :obj +json-alist-list+))


(defparameter +json-streams-obj+
  '(:object
    ("key1" . "value")
    ("key2" . 1)
    ("key3" . (:array "Hello" 1.2d-34))))


(defparameter +st-json-obj+
  (st-json:jso
    "key1"  "value"
    "key2"  1
    "key3" '("Hello" 1.2d-34)))


(defun cl-json/w ()
  "cl-json"
  (cl-json:encode-json-to-string +json-alist-list+))


(defun jonathan/w ()
  "jonathan"
  (jojo:to-json +json-alist-list+ :from :alist))


(defun json-streams/w ()
  "json-streams"
  (json-streams:json-stringify +json-streams-obj+))


(defun jsown/w ()
  "jsown"
  (jsown:to-json +jsown-obj+))


(defun shasht/w ()
  "shasht"
  (let ((*print-pretty* nil))
    (shasht:to-json +json-hash-list+)))


(defun st-json/w ()
  "st-json"
  (st-json:write-json-to-string +st-json-obj+))


(defun yason/w ()
  "yason"
  (with-output-to-string (s)
    (yason:encode-alist +json-alist-list+ s)))


(defparameter +json-writers+
  (list #'cl-json/w
        #'jonathan/w
        #'json-streams/w
        #'jsown/w
        #'shasht/w
        #'st-json/w
        #'yason/w))


(write-line
  (cl-spark:vspark
    (mapcar (lambda (fun)
              (the-cost-of-nothing:benchmark (funcall fun)))
            +json-writers+)
    :title "JSON Write Times"
    :min 0
    :size 80
    :labels (mapcar (lambda (fun) (documentation fun 'function)) +json-writers+)))


(defun cl-json/rw ()
  "cl-json"
  (cl-json:encode-json-to-string (cl-json:decode-json-from-string +json-string+)))


(defun jonathan/rw ()
  "jonathan"
  (jojo:to-json (jojo:parse +json-string+ :as :alist) :from :alist))


(defun json-streams/rw ()
  "json-streams"
  (json-streams:json-stringify (json-streams:json-parse +json-string+)))


(defun jsown/rw ()
  "jsown"
  (jsown:to-json (jsown:parse +json-string+)))


(defun shasht/rw ()
  "shasht"
  (shasht:to-json (shasht:from-json +json-string+)))


(defun st-json/rw ()
  "st-json"
  (st-json:write-json-to-string (st-json:read-json +json-string+)))


(defun yason/rw ()
  "yason"
  (with-output-to-string (s)
    (yason:encode (yason:parse +json-string+) s)))


(defparameter +json-rw+
  (list #'cl-json/rw
        #'jonathan/rw
        #'json-streams/rw
        #'jsown/rw
        #'shasht/rw
        #'st-json/rw
        #'yason/rw))


(write-line
  (cl-spark:vspark
    (mapcar (lambda (fun)
              (the-cost-of-nothing:benchmark (funcall fun)))
            +json-rw+)
    :title "JSON Read/Write Times"
    :min 0
    :size 80
    :labels (mapcar (lambda (fun) (documentation fun 'function)) +json-rw+)))

