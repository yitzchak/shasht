(in-package :shasht)


(defun ascii-printable-p (char-code)
  (<= 32 char-code 126))


(defun supplementary-plane-p (char-code)
  (<= #x10000 char-code #x10ffff))


(defgeneric print-json-object (value output-stream))


(defmethod print-json-object ((value number) output-stream)
  (format output-stream "~,,,,,,'eE" value)
  value)


(defmethod print-json-object ((value integer) output-stream)
  (prin1 value output-stream))


(defmethod print-json-object ((value string) output-stream)
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


(defmethod print-json-object ((value hash-table) output-stream)
  (write-char #\{ output-stream)
  (let (delim)
    (maphash (lambda (key val)
               (if delim
                 (write-char delim output-stream)
                 (setf delim #\,))
               (print-json-object key output-stream)
               (write-char #\: output-stream)
               (print-json-object val output-stream))
             value))
  (write-char #\} output-stream)
  value)


(defmethod print-json-object ((value list) output-stream)
  (cond
    ((and *write-alist-as-object*
          (alistp value))
      (write-char #\{ output-stream)
      (let (delim)
        (dolist (pair value)
          (if delim
            (write-char delim output-stream)
            (setf delim #\,))
          (print-json-object (car pair) output-stream)
          (write-char #\: output-stream)
          (print-json-object (cdr pair) output-stream)))
      (write-char #\} output-stream))
    ((and *write-plist-as-object*
          (plistp value))
      (write-char #\{ output-stream)
      (let (delim)
        (alexandria:doplist (k v value)
                            (if delim
                              (write-char delim output-stream)
                              (setf delim #\,))
                            (print-json-object k output-stream)
                            (write-char #\: output-stream)
                            (print-json-object v output-stream)))
      (write-char #\} output-stream))
    (t
      (write-char #\[ output-stream)
      (let (delim)
        (dolist (element value)
          (if delim
            (write-char delim output-stream)
            (setf delim #\,))
          (print-json-object element output-stream)))
      (write-char #\] output-stream)))
  value)


(defmethod print-json-object ((value vector) output-stream)
  (write-char #\[ output-stream)
  (dotimes (position (length value))
    (unless (zerop position)
      (write-char #\, output-stream))
    (print-json-object (elt value position) output-stream))
  (write-char #\] output-stream)
  value)


(defmethod print-json-object ((value symbol) output-stream)
  (cond
    ((member value *write-true-values* :test #'eql)
      (write-string "true" output-stream))
    ((member value *write-false-values* :test #'eql)
      (write-string "false" output-stream))
    ((member value *write-null-values* :test #'eql)
      (write-string "null" output-stream))
    ((null value)
      (write-string "[]" output-stream))
    (t
      (print-json-object (symbol-name value) output-stream)))
  value)


(defun write-json (value &optional (output-stream t))
  (if (null output-stream)
    (with-output-to-string (output-stream)
      (print-json-object value output-stream))
    (print-json-object value output-stream)))


