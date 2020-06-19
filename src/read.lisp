(in-package :shasht)

(declaim (optimize (speed 0) (safety 3)))

(defvar *true* t)
(defvar *false* nil)
(defvar *null* :null)

(defun read-json-char (input-stream value)
  (unless (char= value (peek-char t input-stream))
    (error "invalid syntax"))
  (read-char input-stream))

(defun read-json-char* (input-stream value)
  (when (char= value (peek-char t input-stream))
    (read-char input-stream)))

(defun read-json-token (input-stream token result)
  (dotimes (pos (length token) result)
    (unless (char= (read-char input-stream) (char token pos))
      (error "invalid syntax"))))

(defun read-digit* (input-stream &optional (radix 10))
  (let ((weight (digit-char-p (peek-char nil input-stream nil) radix)))
    (when weight
      (read-char input-stream)
      weight)))

(defun read-digit (input-stream &optional (radix 10))
  (let ((weight (digit-char-p (read-char input-stream) radix)))
    (if weight
      weight
      (error "invalid syntax"))))

(defun high-surrogate-p (code)
  (<= #xd800 code #xdfff))

(defun read-encoded-char (input-stream)
  (logior (ash (read-digit input-stream 16) 12)
          (ash (read-digit input-stream 16) 8)
          (ash (read-digit input-stream 16) 4)
          (read-digit input-stream 16)))

(defun read-json-escape (input-stream)
  (ecase (read-char input-stream)
    (#\b
      #\backspace)
    (#\f
      #\formfeed)
    (#\n
      #\newline)
    (#\r
      #\return)
    (#\t
      #\tab)
    (#\"
      #\")
    (#\/
      #\/)
    (#\\
      #\\)
    (#\u
      (let ((ch (read-encoded-char input-stream)))
        (code-char
          (if (high-surrogate-p ch)
            (progn
              (read-json-char input-stream #\\)
              (read-json-char input-stream #\u)
              (+ #x10000
                 (- (read-encoded-char input-stream) #xdc00)
                 (* #x400 (- ch #xd800))))
            ch))))))

(defun read-json-string (input-stream)
  (declare (optimize (speed 3) (safety 0))
           (type stream input-stream))
  (read-json-char input-stream #\")
  (do ((ch (read-char input-stream) (read-char input-stream))
       (result (make-array 32 :fill-pointer 0 :adjustable t :element-type 'character)))
      ((char= ch #\") result)
    (declare (type character ch))
    (vector-push
      (if (char= ch #\\)
        (read-json-escape input-stream)
        ch)
      result)))


(defun read-json-object (input-stream)
  (read-json-char input-stream #\{)
  (prog ((result (make-hash-table :test #'equal))
         key)
    (when (read-json-char* input-stream #\})
      (return result))
   read-key-value
    (setf key (read-json-string input-stream))
    (read-json-char input-stream #\:)
    (setf (gethash key result) (read-json input-stream))
    (when (read-json-char* input-stream #\,)
      (go read-key-value))
    (read-json-char input-stream #\})
    (return result)))

(defun read-json-array (input-stream)
  (read-json-char input-stream #\[)
  (prog (result)
    (when (read-json-char* input-stream #\])
      (return result))
   read-item
    (push (read-json input-stream) result)
    (when (read-json-char* input-stream #\,)
      (go read-item))
    (read-json-char input-stream #\])
    (return result)))

(defun read-json-fraction (input-stream value)
  (do ((mult 0.1d0 (* mult 0.1d0))
       (result value)
       (weight (read-digit* input-stream) (read-digit* input-stream)))
      ((not weight) result)
    (setq result (+ result (* mult weight)))))

(defun read-json-integer (input-stream)
  (do* ((negative (read-json-char* input-stream #\-))
        (result 0)
        (weight (read-digit input-stream) (read-digit* input-stream)))
       ((not weight)
         (if negative
           (- result)
           result))
    (setq result (+ (* 10 result) weight))))

(defun read-json-number (input-stream)
  (let ((value (read-json-integer input-stream)))
    (when (eql #\. (peek-char nil input-stream nil))
      (read-char input-stream)
      (setq value (read-json-fraction input-stream value)))
    (if (equalp #\e (peek-char nil input-stream nil))
      (* value (expt 10d0 (read-json-integer input-stream)))
      value)))

(defun read-json (&optional (input-stream *standard-input*))
  (ecase (peek-char t input-stream)
    (#\{
      (read-json-object input-stream))
    (#\[
      (read-json-array input-stream))
    (#\"
      (read-json-string input-stream))
    ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
      (read-json-number input-stream))
    (#\t
      (read-json-token input-stream "true" *true*))
    (#\f
      (read-json-token input-stream "false" *false*))
    (#\n
      (read-json-token input-stream "null" *null*))))

(defun from-json (value)
  (with-input-from-string (input-stream value)
    (read-json input-stream)))


