(in-package :shasht)

(defvar *comma-needed* nil)



(defgeneric write-json (object &optional output-stream))


(defun to-json (object)
  (with-output-to-string (output-stream)
    (write-json object output-stream)))


(defun ascii-printable-p (char-code)
  (<= 32 char-code 126))


(defun supplementary-plane-p (char-code)
  (<= #x10000 char-code #x10ffff))


(defun write-object-key-value (key value)
  (if *comma-needed*
    (write-char #\,)
    (setq *comma-needed* t))
  (write-json key)
  (write-char #\:)
  (write-json value))


(defmacro write-object (output-stream &body body)
  `(let ((*standard-output* (or ,output-stream *standard-output*))
         (*comma-needed* nil))
     (write-char #\{)
     ,@body
     (write-char #\})))

(defmacro with-object (output-stream &body specs)
  `(write-object ,output-stream
     ,@(mapcar (lambda (spec)
                 `(write-object-key-value ,(first spec) (progn ,@(rest spec))))
               specs)))

(defun write-array-element (element)
  (if *comma-needed*
    (write-char #\,)
    (setq *comma-needed* t))
  (write-json element))


(defmacro write-array (output-stream &body body)
  `(let ((*standard-output* (or ,output-stream *standard-output*))
         (*comma-needed* nil))
     (write-char #\[)
     ,@body
     (write-char #\])))


(defmethod write-json ((object hash-table) &optional output-stream)
  (write-object output-stream
    (maphash (lambda (key val)
               (write-object-key-value key val))
             object)))


(defun write-json-alist (alist &optional output-stream)
  (write-object output-stream
    (dolist (pair alist)
      (write-object-key-value (car pair) (cdr pair)))))


(defun write-json-plist (plist &optional output-stream)
  (write-object output-stream
    (alexandria:doplist (key value plist)
      (write-object-key-value key value))))


(defmethod write-json ((object string) &optional output-stream)
  (write-char #\" output-stream)
  (do ((index 0 (1+ index)))
      ((>= index (length object)))
    (let* ((ch (char object index))
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
  (values))
  

(defmethod write-json ((object number) &optional output-stream)
  (format (or output-stream t) "~,,,,,,'eE" object))


(defmethod write-json ((object integer) &optional output-stream)
  (prin1 object output-stream))


(defmethod write-json ((object symbol) &optional output-stream)
  (cond
    ((member object *write-true-values* :test #'eql)
      (write-string "true" output-stream))
    ((member object *write-false-values* :test #'eql)
      (write-string "false" output-stream))
    ((member object *write-null-values* :test #'eql)
      (write-string "null" output-stream))
    (t
      (write-json (symbol-name object) output-stream))))


(defmethod write-json ((object list) &optional output-stream)
  (cond
    ((and *write-alist-as-object*
          (alistp object))
      (write-json-alist object output-stream))
    ((and *write-plist-as-object*
          (plistp object))
      (write-json-plist object output-stream))
    (t
      (write-array output-stream
        (dolist (element object)
          (write-array-element element))))))


(defmethod write-json ((object vector) &optional output-stream)
  (write-array output-stream
    (dotimes (index (length object))
      (write-array-element (elt object index)))))

