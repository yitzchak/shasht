(in-package :shasht)


(defun ascii-printable-p (char-code)
  (<= 32 char-code 126))


(defun supplementary-plane-p (char-code)
  (<= #x10000 char-code #x10ffff))


(defstruct writer-state
  delimiter
  terminator
  indent)


(defclass writer ()
  ((states
     :accessor states
     :initform nil)
   (output-stream
     :accessor output-stream
     :initarg :output-stream)
   (pretty
     :accessor pretty
     :initarg :pretty
     :initform nil)))


(defun write-separator (instance)
  (let ((state (first (states instance))))
    (when state
      (write-string (writer-state-delimiter state) (output-stream instance))
      (if (pretty instance)
        (setf (writer-state-delimiter state)
              (concatenate 'string "," (string #\newline) (make-string (* 2 (1+ (writer-state-indent state))) :initial-element #\space))
              (writer-state-terminator state)
              (concatenate 'string (string #\newline) (make-string (* 2 (writer-state-indent state)) :initial-element #\space)))
        (setf (writer-state-delimiter state) ","
              (writer-state-terminator state) "")))))


(defmethod json-array-begin ((instance writer))
  (write-separator instance)
  (with-slots (states output-stream pretty)
              instance
    (let ((indent (if states
                     (1+ (writer-state-indent (first states)))
                     0)))
      (write-char #\[ output-stream)
      (push (make-writer-state :delimiter (if pretty
                                            (concatenate 'string (string #\newline) (make-string (* 2 (1+ indent)) :initial-element #\space))
                                            "")
                               :terminator ""
                               :indent indent)
            states))))


(defmethod json-array-end ((instance writer))
  (write-string (writer-state-terminator (pop (states instance))) (output-stream instance))
  (write-char #\] (output-stream instance)))


(defmethod json-object-begin ((instance writer))
  (write-separator instance)
  (with-slots (states output-stream pretty)
              instance
    (let ((indent (if states
                    (1+ (writer-state-indent (first states)))
                    0)))
      (write-char #\{ output-stream)
      (push (make-writer-state :delimiter (if pretty
                                            (concatenate 'string (string #\newline) (make-string (* 2 (1+ indent)) :initial-element #\space))
                                            "")
                               :terminator ""
                               :indent indent)
            states))))


(defmethod json-object-end ((instance writer))
  (write-string (writer-state-terminator (pop (states instance))) (output-stream instance))
  (write-char #\} (output-stream instance)))


(defmethod json-key ((instance writer) key)
  (json-value instance key)
  (setf (writer-state-delimiter (first (states instance)))
        (if (pretty instance)
            ": "
            ":")))


(defmethod json-value :before ((instance writer) value)
  (declare (ignore value))
  (write-separator instance))
  

(defmethod json-value ((instance writer) (value string))
  (with-slots (output-stream)
              instance
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
    (write-char #\" output-stream)))


(defmethod json-value ((instance writer) (value hash-table))
  (json-object-begin instance)
  (maphash (lambda (key val)
             (json-key instance key)
             (json-value instance val))
           value)
  (json-object-end instance))


(defmethod json-value ((instance writer) (value list))
  (cond
    ((and *write-alist-as-object*
          (alistp value))
      (json-object-begin instance)
      (dolist (pair value)
        (json-key instance (car pair))
        (json-value instance (cdr pair)))
      (json-object-end instance))
    ((and *write-plist-as-object*
          (plistp value))
      (json-object-begin instance)
      (alexandria:doplist (k v value)
        (json-key instance k)
        (json-value instance v))
      (json-object-end instance))
    (t
      (json-array-begin instance)
      (dolist (element value)
        (json-value instance element))
      (json-array-end instance))))


(defmethod json-value ((instance writer) (value vector))
  (json-array-begin instance)
  (dotimes (index (length value))
    (json-value instance (elt value index)))
  (json-array-end instance))


(defmethod json-value ((instance writer) (value integer))
  (prin1 value (output-stream instance)))


(defmethod json-value ((instance writer) (value number))
  (format (output-stream instance) "~,,,,,,'eE" value))


(defmethod json-value ((instance writer) (value symbol))
  (cond
    ((member value *write-true-values* :test #'eql)
      (write-string "true" (output-stream instance)))
    ((member value *write-false-values* :test #'eql)
      (write-string "false" (output-stream instance)))
    ((member value *write-null-values* :test #'eql)
      (write-string "null" (output-stream instance)))
    (t
      (json-value instance (symbol-name value)))))


(defmethod json-eof ((instance writer)))


(defmethod json-error ((instance writer) control &rest args)
  (declare (ignore instance control args)))


(defun write-json (object &optional output-stream)
  (json-value (make-instance 'writer :output-stream (or output-stream *standard-output*))
              object)
  object)


(defun pprint-json (object &optional output-stream)
  (json-value (make-instance 'writer :output-stream (or output-stream *standard-output*) :pretty t)
              object)
  object)


(defun to-json (object)
  (with-output-to-string (output-stream)
    (write-json object output-stream)))


