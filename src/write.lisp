(in-package :shasht)


(declaim #+(or)(optimize (speed 3) (safety 0))
         (ftype (function (t t stream) t) print-json-key-value))


(defparameter *delimiter* nil)
(defparameter *next-delimiter* nil)


(defmacro with-json-array (output-stream &body body)
  `(let ((*delimiter* nil)
         (*next-delimiter* ","))
     (declare (type (or null string) *delimiter* *next-delimiter*))
     (write-char #\[ ,output-stream)
     (locally ,@body)
     (write-char #\] ,output-stream)))


(defmacro with-json-object (output-stream &body body)
  `(let ((*delimiter* nil)
         (*next-delimiter* ","))
     (declare (type (or null string) *delimiter* *next-delimiter*))
     (write-char #\{ ,output-stream)
     (locally ,@body)
     (write-char #\} ,output-stream)))


(defgeneric print-json-value (value output-stream))


(defun print-json-key-value (key value output-stream)
  (print-json-value key output-stream)
  (let ((*delimiter* ":"))
    (print-json-value value output-stream)))


(defmethod print-json-value :before (value output-stream)
  (declare (ignore value))
  (if *delimiter*
    (write-string *delimiter* output-stream)
    (setf *delimiter* *next-delimiter*)))


(defmethod print-json-value ((value number) output-stream)
  (format output-stream "~,,,,,,'eE" value)
  value)


(defmethod print-json-value ((value integer) output-stream)
  (prin1 value output-stream))


(defmethod print-json-value ((value string) output-stream)
  (write-char #\" output-stream)
  (do ((index 0 (1+ index)))
      ((>= index (length value)))
    (let* ((ch (char value index))
           (code (char-code ch)))
      (cond
        ((char= ch #\newline)
          (write-string "\\n" output-stream))
        ((char= ch #\return)
          (write-string "\\r" output-stream))
        ((char= ch #\tab)
          (write-string "\\t" output-stream))
        ((char= ch #\page)
          (write-string "\\f" output-stream))
        ((char= ch #\backspace)
          (write-string "\\b" output-stream))
        ((char= ch #\")
          (write-string "\\\"" output-stream))
        ((char= ch #\\)
          (write-string "\\\\" output-stream))
        ((not (graphic-char-p ch))
          (format output-stream "\\u~4,'0x" code))
        ((or (not *write-ascii-encoding*)
             (ascii-printable-p code))
          (write-char ch output-stream))
        ((supplementary-plane-p code)
          (format output-stream "\\u~4,'0x\\u~4,'0x"
                  (+ (ash (- code #x10000) -10)
                     #xd800)
                  (+ (logand (- code #x10000)
                             (- (ash 1 10) 1))
                     #xdc00)))
        (t
          (format output-stream "\\u~4,'0x" code)))))
  (write-char #\" output-stream)
  value)


(defmethod print-json-value ((value hash-table) output-stream)
  (with-json-object output-stream
    (maphash (lambda (key val)
               (print-json-key-value key val output-stream))
             value))
  value)


(defmethod print-json-value ((value list) output-stream)
  (cond
    ((and *write-alist-as-object*
          (alistp value))
      (with-json-object output-stream
        (dolist (pair value)
          (print-json-key-value (car pair) (cdr pair) output-stream))))
    ((and *write-plist-as-object*
          (plistp value))
      (with-json-object output-stream
        (alexandria:doplist (k v value)
          (print-json-key-value k v output-stream))))
    (t
      (with-json-array output-stream
        (dolist (element value)
          (print-json-value element output-stream)))))
  value)


(defmethod print-json-value ((value vector) output-stream)
  (with-json-array output-stream
    (dotimes (position (length value))
      (print-json-value (elt value position) output-stream)))
  value)


(defmethod print-json-value ((value symbol) output-stream)
  (cond
    ((member value *write-true-values* :test #'eql)
      (write-string "true" output-stream))
    ((member value *write-false-values* :test #'eql)
      (write-string "false" output-stream))
    ((member value *write-null-values* :test #'eql)
      (write-string "null" output-stream))
    ((and (null value)
          (or *write-alist-as-object*
              *write-plist-as-object*))
      (write-string "{}" output-stream))
    ((null value)
      (write-string "[]" output-stream))
    (t
      (print-json-value (symbol-name value) output-stream)))
  value)


(defun write-json (value &optional (output-stream t))
  (if (null output-stream)
    (with-output-to-string (output-stream)
      (print-json-value value output-stream))
    (print-json-value value (if (eql t output-stream)
                               *standard-output*
                               output-stream))))


