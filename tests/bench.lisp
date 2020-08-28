(ql:quickload '(:cl-json :jonathan :json-streams :jsown :shasht :st-json :yason :cl-spark :the-cost-of-nothing))

(defparameter +json-string+ "{\"key1\":\"value\",\"key2\":1,\"key3\":[\"Hello\",1.2e-34]}")

(defun cl-json/from-json ()
  "cl-json"
  (cl-json:decode-json-from-string +json-string+))


(defun jonathan/from-json ()
  "jonathan"
  (jojo:parse +json-string+ :as :alist))


(defun json-streams/from-json ()
  "json-streams"
  (json-streams:json-parse +json-string+))


(defun jsown/from-json ()
  "jsown"
  (jsown:parse +json-string+))


(defun shasht/from-json ()
  "shasht"
  (shasht:from-json +json-string+))


(defun st-json/from-json ()
  "st-json"
  (st-json:read-json +json-string+))


(defun yason/from-json ()
  "yason"
  (yason:parse +json-string+))


(defparameter +json-parsers+
  (list #'cl-json/from-json
        #'jonathan/from-json
        #'json-streams/from-json
        #'jsown/from-json
        #'shasht/from-json
        #'st-json/from-json
        #'yason/from-json))


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


(defun cl-json/to-json ()
  "cl-json"
  (cl-json:encode-json-to-string +json-alist-list+))


(defun jonathan/to-json ()
  "jonathan"
  (jojo:to-json +json-alist-list+ :from :alist))


(defun json-streams/to-json ()
  "json-streams"
  (json-streams:json-stringify +json-streams-obj+))


(defun jsown/to-json ()
  "jsown"
  (jsown:to-json +jsown-obj+))


(defun shasht/to-json ()
  "shasht"
  (let ((*print-pretty* nil))
    (shasht:to-json +json-hash-list+)))


(defun st-json/to-json ()
  "st-json"
  (st-json:write-json-to-string +st-json-obj+))


(defun yason/to-json ()
  "yason"
  (with-output-to-string (s)
    (yason:encode-alist +json-alist-list+ s)))


(defparameter +json-writers+
  (list #'cl-json/to-json
        #'jonathan/to-json
        #'json-streams/to-json
        #'jsown/to-json
        #'shasht/to-json
        #'st-json/to-json
        #'yason/to-json))


(write-line
  (cl-spark:vspark
    (mapcar (lambda (fun)
              (the-cost-of-nothing:benchmark (funcall fun)))
            +json-writers+)
    :title "JSON Write Times"
    :min 0
    :size 80
    :labels (mapcar (lambda (fun) (documentation fun 'function)) +json-writers+)))



