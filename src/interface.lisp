(in-package :shasht)


(defgeneric json-array-begin (instance)
  (:method ((instance (eql nil)))
    (declare (ignore instance))))


(defgeneric json-array-end (instance)
  (:method ((instance (eql nil)))
    (declare (ignore instance))))


(defgeneric json-object-begin (instance)
  (:method ((instance (eql nil)))
    (declare (ignore instance))))


(defgeneric json-object-end (instance)
  (:method ((instance (eql nil)))
    (declare (ignore instance))))


(defgeneric json-key (instance key)
  (:method ((instance (eql nil)) key)
    (declare (ignore instance key))))


(defgeneric json-value (instance value)
  (:method ((instance (eql nil)) value)
    (declare (ignore instance value))))


(defgeneric json-eof (instance)
  (:method ((instance (eql nil)))
    (declare (ignore instance))))


(defgeneric json-error (instance control &rest args)
  (:method ((instance (eql nil)) control &rest args)
    (declare (ignore instance control args))))



