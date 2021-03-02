(in-package :shasht)


(declaim #+(or)(optimize (speed 3) (safety 0))
         (ftype (function (t t stream) t) print-json-key-value)
         (ftype (function (string stream) string) write-json-string))


(defparameter *delimiter* nil)
(defparameter *next-delimiter* nil)
(defparameter *indent-level* 0)
(defparameter *terminator* nil)
(defparameter *next-terminator* nil)


(defun write-json-string (value output-stream)
  "Write value as a JSON string to stream specified by output-stream."
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


(defmacro with-json-array (output-stream &body body)
  "Enable JSON array writing for body. Array open/close and commas will be automatically
handled when calls to print-json-value are made."
  `(let* ((*terminator* "]")
          (*next-terminator* (if *print-pretty*
                               (concatenate 'string
                                            (string #\newline)
                                            (make-string *indent-level* :initial-element *indent-character*)
                                            "]")
                               "]"))
          (*indent-level* (+ *indent-level* *indent-increment*))
          (*delimiter* (when *print-pretty*
                         (concatenate 'string
                                      (string #\newline)
                                      (make-string *indent-level* :initial-element *indent-character*))))
          (*next-delimiter* (if *print-pretty*
                              (concatenate 'string
                                           ","
                                           (string #\newline)
                                           (make-string *indent-level* :initial-element *indent-character*))
                              ",")))
     (declare (type (or null string) *delimiter* *next-delimiter*))
     (write-char #\[ ,output-stream)
     (locally ,@body)
     (write-string *terminator* ,output-stream)))


(defmacro with-json-object (output-stream &body body)
  "Enable JSON object writing for body. Object open/close and commas will be automatically
handled when calls to print-json-key-value are made."
  `(let* ((*terminator* "}")
          (*next-terminator* (if *print-pretty*
                               (concatenate 'string
                                            (string #\newline)
                                            (make-string *indent-level* :initial-element *indent-character*)
                                            "}")
                               "}"))
          (*indent-level* (+ *indent-level* *indent-increment*))
          (*delimiter* (when *print-pretty*
                         (concatenate 'string
                                      (string #\newline)
                                      (make-string *indent-level* :initial-element *indent-character*))))
          (*next-delimiter* (if *print-pretty*
                              (concatenate 'string
                                           ","
                                           (string #\newline)
                                           (make-string *indent-level* :initial-element *indent-character*))
                              ",")))
     (declare (type (or null string) *delimiter* *next-delimiter*))
     (write-char #\{ ,output-stream)
     (locally ,@body)
     (write-string *terminator* ,output-stream)))


(defgeneric print-json-value (value output-stream)
  (:documentation "Print a JSON value to output-stream. Used by write-json to dispatch based on type."))


(defun print-json-key-value (key value output-stream)
  "Print a JSON key value. Must be used inside of with-json-object."
  (print-json-value key output-stream)
  (let ((*delimiter* (if *print-pretty*
                       ": "
                       ":")))
    (print-json-value value output-stream)))


(defmethod print-json-value :before (value output-stream)
  (declare (ignore value))
  (when *delimiter*
    (write-string *delimiter* output-stream))
  (setf *delimiter* *next-delimiter*)
  (setf *terminator* *next-terminator*))


(defmethod print-json-value ((value number) output-stream)
  (let* ((result (format nil "~,,,,,,'eE" value))
         (decimal-position (position #\. result)))
    (write-string (cond
                    ((not decimal-position)
                      result)
                    ((= (1+ decimal-position) (length result))
                      (subseq result 0 (1- (length result))))
                    ((char= #\e (char result (1+ decimal-position)))
                      (concatenate 'string (subseq result 0 decimal-position)
                                   (subseq result (1+ decimal-position))))
                    (t
                      result))
                  output-stream))
  value)


(defmethod print-json-value ((value integer) output-stream)
  (prin1 value output-stream))


(defmethod print-json-value ((value string) output-stream)
  (write-json-string value output-stream))


(defmethod print-json-value ((value hash-table) output-stream)
  (with-json-object output-stream
    (maphash (lambda (key val)
               (print-json-key-value key val output-stream))
             value))
  value)


(defmethod print-json-value ((value list) output-stream)
  (cond
    ((eql :object (car value))
      (with-json-object output-stream
        (dolist (pair (cdr value))
          (print-json-key-value (car pair) (cdr pair) output-stream))))
    ((eql :array (car value))
      (with-json-array output-stream
        (dolist (element (cdr value))
          (print-json-value element output-stream))))
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
    ((or (and (null value)
              (or *write-alist-as-object*
                  *write-plist-as-object*))
         (member value *write-empty-object-values* :test #'eql))
      (write-string "{}" output-stream))
    ((or (null value)
         (member value *write-empty-array-values* :test #'eql))
      (write-string "[]" output-stream))
    (t
      (write-json-string (symbol-name value) output-stream)))
  value)


(defun write-json (value &optional (output-stream t))
"Read a JSON value. Writing is influenced by the dynamic variables
*write-ascii-encoding*, *write-true-values*,  *write-false-values*,
*write-null-values*, *write-alist-as-object*,  *write-plist-as-object*,
*indent-increment*, *indent-character* and common-lisp:*print-pretty*
which simple indentation of arrays and objects.

The following arguments also control the behavior of the write.

* value - The value to be written.
* output-stream - a stream or nil to return a string or t to use
  *standard-output*."
  (if (null output-stream)
    (with-output-to-string (output-stream)
      (print-json-value value output-stream))
    (print-json-value value (if (eql t output-stream)
                               *standard-output*
                               output-stream))))


(defun write-json* (value &key (output-stream t) ascii-encoding (true-values* '(t :true))
                               (false-values* '(nil :false)) (null-values* '(:null))
                               (empty-array-values* '(:empty-array))
                               (empty-object-values* '(:empty-object)) alist-as-object
                               plist-as-object print-pretty (indent-increment 2)
                               (indent-character* #\space))
"Read a JSON value. Writing is not influenced by the dynamic variables
of write-json.

The following arguments also control the behavior of the write.

* value - The value to be written.
* output-stream - a stream or nil to return a string or t to use
  *standard-output*.
* ascii-encoding - If true then any non ASCII values will be encoded
  using Unicode escape sequences.
* true-values - Values that will be written as a true token.
* false-values - Values that will be written as a false token.
* null-values - Values that will be written as a null token.
* empty-array-values - Values that will be written as an empty array.
* empty-object-values - Values that will be written as an empty object.
* alist-as-object - If true then assocation lists will be written as an object.
* plist-as-object - If true then property lists will be written as an object.
* print-pretty - Use indentation in printing.
* indent-increment - The number of `indent-character` to use at each indention level if
  `print-pretty` is true.
* indent-character - The character to use when indenting objects and arrays.")

