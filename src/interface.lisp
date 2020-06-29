(in-package :shasht)


(defgeneric json-array-begin (instance))


(defgeneric json-array-end (instance))


(defgeneric json-object-begin (instance))


(defgeneric json-object-end (instance))


(defgeneric json-key (instance key))


(defgeneric json-value (instance value))


(defgeneric json-eof (instance))


(defgeneric json-error (instance control &rest args))



