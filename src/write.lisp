(in-package :shasht)


(declaim (inline make-newline-strine)
         (ftype (function (t t t stream) t) print-json-key-value)
         (ftype (function (string stream) string) write-json-string))


(defparameter *delimiter* nil)
(defparameter *next-delimiter* nil)
(defparameter *write-indent-level* 0)
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


(defun make-newline-string ()
  (when *print-pretty*
    (apply 'concatenate 'string (list #\newline)
           (make-list *write-indent-level* :initial-element *write-indent-string*))))


(defmacro with-json-array (output-stream &body body)
  "Enable JSON array writing for body. Array open/close and commas will be automatically
handled when calls to print-json-value are made."
  `(let* ((*terminator* "]")
          (*next-terminator* (concatenate 'string (make-newline-string) "]"))
          (*write-indent-level* (1+ *write-indent-level*))
          (*delimiter* (make-newline-string))
          (*next-delimiter* (concatenate 'string "," *delimiter*)))
     (declare (type (or null string) *delimiter* *next-delimiter*))
     (write-char #\[ ,output-stream)
     (locally ,@body)
     (write-string *terminator* ,output-stream)))


(defmacro with-json-object (output-stream &body body)
  "Enable JSON object writing for body. Object open/close and commas will be automatically
handled when calls to print-json-key-value are made."
  `(let* ((*terminator* "}")
          (*next-terminator* (concatenate 'string (make-newline-string) "}"))
          (*write-indent-level* (1+ *write-indent-level*))
          (*delimiter* (make-newline-string))
          (*next-delimiter* (concatenate 'string "," *delimiter*)))
     (declare (type (or null string) *delimiter* *next-delimiter*))
     (write-char #\{ ,output-stream)
     (locally ,@body)
     (write-string *terminator* ,output-stream)))


(defgeneric print-json-value (value output-stream)
  (:documentation "Print a JSON value to output-stream. Used by write-json to dispatch based on type."))


(defmacro with-json-key ((key output-stream) &body body)
  `(progn
     (print-json-value ,key ,output-stream)
     (let ((*delimiter* (if *print-pretty*
                          ": "
                          ":")))
       ,@body)))


(defgeneric print-json-key-value (object key value output-stream)
  (:documentation "Print a JSON key value. Must be used inside of with-json-object.")
  (:method (object key value output-stream)
    (declare (ignore object))
    (print-json-value key output-stream)
    (let ((*delimiter* (if *print-pretty*
                         ": "
                         ":")))
      (print-json-value value output-stream))))


(defun print-json-delimiter (output-stream)
  (when *delimiter*
    (write-string *delimiter* output-stream))
  (setf *delimiter* *next-delimiter*)
  (setf *terminator* *next-terminator*))


(defmethod print-json-value :before (value output-stream)
  (declare (ignore value))
  (print-json-delimiter output-stream))


(defmethod print-json-value ((value number) output-stream)
  (let* ((result (format nil "~,,,,,,'eE" value))
         (decimal-position (position #\. result))
         (exponent-position (position #\e result))
         (end (length result)))
    (when (and (equal exponent-position (- end 3))
               (char= #\0 (char result (1- end))))
      (decf end 3))
    (when (equal decimal-position (1- end))
      (decf end))
    (when (and decimal-position
               (equal (1+ decimal-position) exponent-position))
      (setf result (concatenate 'string
                                (subseq result 0 decimal-position)
                                (subseq result exponent-position)))
      (decf end))
    (if (zerop end)
        (write-string "0" output-stream)
        (write-string result output-stream :end end)))
  value)


(defmethod print-json-value ((value integer) output-stream)
  (prin1 value output-stream))


(defmethod print-json-value ((value string) output-stream)
  (write-json-string value output-stream))


(defmethod print-json-value ((value hash-table) output-stream)
  (with-json-object output-stream
    (maphash (lambda (key val)
               (print-json-key-value value key val output-stream))
             value))
  value)


(defmethod print-json-value ((value list) output-stream)
  (cond
    ((member (car value) *write-object-alist-tags*)
      (with-json-object output-stream
        (trivial-do:doalist (k v (cdr value))
          (print-json-key-value value k v output-stream))))
    ((member (car value) *write-object-plist-tags*)
      (with-json-object output-stream
        (trivial-do:doplist (k v (cdr value))
          (print-json-key-value value k v output-stream))))
    ((member (car value) *write-array-tags*)
      (with-json-array output-stream
        (dolist (element (cdr value))
          (print-json-value element output-stream))))
    ((and *write-alist-as-object*
          (alistp value))
      (with-json-object output-stream
        (trivial-do:doalist (k v value)
          (print-json-key-value value k v output-stream))))
    ((and *write-plist-as-object*
          (plistp value))
      (with-json-object output-stream
        (trivial-do:doplist (k v value)
          (print-json-key-value value k v output-stream))))
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


(defmethod print-json-value ((value array) output-stream)
  (labels ((print-subarray (index dimensions)
             (with-json-array output-stream
               (loop with dimension = (car dimensions)
                     with remaining-dimensions = (cdr dimensions)
                     for pos below dimension
                     for new-index = (+ pos (* index dimension))
                     if remaining-dimensions
                       do (print-json-delimiter output-stream)
                          (print-subarray new-index remaining-dimensions)
                     else
                       do (print-json-value (row-major-aref value new-index)
                                            output-stream)))))
    (print-subarray 0 (array-dimensions value))))


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
      (write-json-string (funcall *symbol-name-function* value)
                         output-stream)))
  value)


(defmethod print-json-value ((value pathname) output-stream)
  (write-json-string (namestring value) output-stream)
  value)


(defmethod print-json-value ((value character) output-stream)
  (write-json-string (string value) output-stream)
  value)


(defun print-json-mop (value output-stream)
  (with-json-object output-stream
    (dolist (def (closer-mop:class-slots (class-of value)) value)
      (let ((slot-name (closer-mop:slot-definition-name def)))
        (when (slot-boundp value slot-name)
          (let* ((slot-value (slot-value value slot-name))
                 (slot-type (closer-mop:slot-definition-type def))
                 (list-type-p (subtypep 'list slot-type))
                 (null-type-p (subtypep 'null slot-type))
                 (boolean-type-p (subtypep 'boolean slot-type)))
            (print-json-key-value value
                                  slot-name
                                  (cond
                                    (slot-value
                                      slot-value)
                                    ((and null-type-p
                                          (not list-type-p)
                                          (not boolean-type-p))
                                      :null)
                                    ((and list-type-p
                                          (not boolean-type-p))
                                      :empty-array)
                                    (t
                                      :false))
                                  output-stream))))))
  value)


(defmethod print-json-value ((value standard-object) output-stream)
  (print-json-mop value output-stream))


(defmethod print-json-value ((value structure-object) output-stream)
  (print-json-mop value output-stream))


(defun write-json (value &optional (output-stream t))
"Write a JSON value. Writing is influenced by the dynamic variables
*write-ascii-encoding*, *write-true-values*,  *write-false-values*,
*write-null-values*, *write-alist-as-object*,  *write-plist-as-object*,
*write-indent-string* and common-lisp:*print-pretty*.

The following arguments also control the behavior of the write.

* value - The value to be written.
* output-stream - a stream or nil to return a string or t to use
  *standard-output*."
  (cond ((null output-stream)
         (with-output-to-string (output-stream)
           (print-json-value value output-stream)))
        ((eq t output-stream)
         (print-json-value value *standard-output*))
        ((and (streamp output-stream)
              (output-stream-p output-stream))
         (print-json-value value output-stream))
        (t
         (error 'type-error :datum output-stream :expected-type '(or null t stream)))))


(defun write-json* (value
                    &key (stream t)
                         ((:ascii-encoding *write-ascii-encoding*) *write-ascii-encoding*)
                         ((:true-values *write-true-values*) *write-true-values*)
                         ((:false-values *write-false-values*) *write-false-values*)
                         ((:empty-array-values *write-empty-array-values*) *write-empty-array-values*)
                         ((:empty-object-values *write-empty-object-values*) *write-empty-object-values*)
                         ((:array-tags *write-array-tags*) *write-array-tags*)
                         ((:object-alist-tags *write-object-alist-tags*) *write-object-alist-tags*)
                         ((:object-plist-tags *write-object-plist-tags*) *write-object-plist-tags*)
                         ((:alist-as-object *write-alist-as-object*) *write-alist-as-object*)
                         ((:plist-as-object *write-plist-as-object*) *write-plist-as-object*)
                         ((:pretty *print-pretty*) *print-pretty*)
                         ((:indent-string *write-indent-string*) *write-indent-string*))
"Write a JSON value.

The following arguments also control the behavior of the write.

* value - The value to be written.
* stream - a stream or nil to return a string or t to use
  *standard-output*.
* ascii-encoding - If true then any non ASCII values will be encoded
  using Unicode escape sequences.
* true-values - Values that will be written as a true token.
* false-values - Values that will be written as a false token.
* null-values - Values that will be written as a null token.
* empty-array-values - Values that will be written as an empty array.
* empty-object-values - Values that will be written as an empty object.
* array-tags - A list of values whose appearance in the CAR of a list
  indicates the CDR of the list should be written as an array.
* object-alist-tags - A list of values whose appearance in the CAR of
  a list indicates the CDR of the list is an alist and should be written as an
  object.
* object-plist - A list of values whose appearance in the CAR of
  a list indicates the CDR of the list is a plist and should be written as an
  object.
* alist-as-object - If true then assocation lists will be written as an object.
* plist-as-object - If true then property lists will be written as an object.
* pretty - Use indentation in printing.
* indent-string - The string to use when indenting objects and arrays."
  (write-json value stream))

