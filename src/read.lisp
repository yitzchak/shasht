(in-package :shasht)

(defvar *true* t)
(defvar *false* nil)
(defvar *null* :null)
(defvar *array* :vector)
(defvar *object* :hash-table)


(define-condition shasht-parse-error (parse-error)
  ((message
     :initarg :message
     :reader shasht-parse-error-message))
  (:report (lambda (condition stream)
             (write-line (shasht-parse-error-message condition) stream))))


(defun read-json-char (input-stream value &optional skip-whitespace)
  (when skip-whitespace
    (peek-char t input-stream))
  (let ((ch (read-char input-stream nil)))
    (cond
      ((null ch)
        (error 'shasht-parse-error :message (format nil "Expected the next character to be ~A but encountered end of file first." value)))
      ((char= value ch)
        ch)
      (t
        (error 'shasht-parse-error :message (format nil "Expected the next character to be ~A but found ~A instead." value ch))))))


(defun read-json-char* (input-stream value &optional skip-whitespace)
  (when (char= value (peek-char skip-whitespace input-stream))
    (read-char input-stream)))


(defun read-json-token (input-stream token result &optional skip-whitespace)
  (dotimes (pos (length token) result)
    (read-json-char input-stream (char token pos) (and skip-whitespace (zerop pos)))))


(defun read-digit* (input-stream &optional (radix 10))
  (let* ((ch (peek-char nil input-stream nil))
         (weight (when ch
                   (digit-char-p ch radix))))
    (when weight
      (read-char input-stream)
      weight)))


(defun read-digit (input-stream &optional (radix 10))
  (let* ((ch (read-char input-stream nil))
         (weight (when ch (digit-char-p ch radix))))
    (cond
      ((null ch)
        (error 'shasht-parse-error :message (format nil "Expected the next character to be a digit in base ~A but encountered end of file first." radix)))
      (weight
        weight)
      (t
        (error 'shasht-parse-error :message (format nil "Expected the next character to be a digit in base ~A but found ~A instead." radix ch))))))


(defun high-surrogate-p (code)
  (<= #xd800 code #xdfff))


(defun read-encoded-char (input-stream)
  (logior (ash (read-digit input-stream 16) 12)
          (ash (read-digit input-stream 16) 8)
          (ash (read-digit input-stream 16) 4)
          (read-digit input-stream 16)))


(defun read-json-escape (input-stream)
  (declare (type stream input-stream)
           (optimize (debug 0) (safety 0) (speed 3)))
  (let ((ch (read-char input-stream nil :eof)))
    (case ch
      (:eof
        (error 'shasht-parse-error :message (format nil "Expected the next character an escape but encountered end of file first.")))
      (#\b
        #\backspace)
      (#\f
        #\page)
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
        (let ((ech (read-encoded-char input-stream)))
          (code-char
            (if (high-surrogate-p ech)
              (progn
                (read-json-char input-stream #\\)
                (read-json-char input-stream #\u)
                (+ #x10000
                   (- (read-encoded-char input-stream) #xdc00)
                   (* #x400 (- ech #xd800))))
              ech))))
      (otherwise
        (error 'shasht-parse-error :message (format nil "Unknown escape sequence \\~A found." ch))))))


(defun read-json-string (input-stream)
  (read-json-char input-stream #\" t)
  (do ((ch (read-char input-stream nil) (read-char input-stream nil))
       (result (make-array 32 :fill-pointer 0 :adjustable t :element-type 'character)))
      ((equal ch #\") result)
    (cond
      ((null ch)
        (error 'shasht-parse-error :message (format nil "Unexpected end of file in string.")))
      ((graphic-char-p ch) ; Need to check this against unicode, also is delete allowed in JSON strings?
        (vector-push (if (char= ch #\\)
                       (read-json-escape input-stream)
                       ch)
                     result))
      (t
        (error 'shasht-parse-error :message (format nil "Non graphic character found in string."))))))


(defun read-json-object (input-stream)
  (read-json-char input-stream #\{ t)
  (prog ((result (make-hash-table :test #'equal))
         key)
    (when (read-json-char* input-stream #\} t)
      (return result))
   read-key-value
    (setf key (read-json-string input-stream))
    (read-json-char input-stream #\: t)
    (setf (gethash key result) (read-json input-stream))
    (when (read-json-char* input-stream #\, t)
      (go read-key-value))
    (read-json-char input-stream #\} t)
    (return result)))


(defun read-json-array (input-stream)
  (read-json-char input-stream #\[ t)
  (prog (result)
    (when (read-json-char* input-stream #\] t)
      (return (nreverse result)))
   read-item
    (push (read-json input-stream) result)
    (when (read-json-char* input-stream #\, t)
      (go read-item))
    (read-json-char input-stream #\] t)
    (return (nreverse result))))


; (defun read-json-array (input-stream)
;   (read-json-char input-stream #\[)
;   (prog ((result (make-array 32 :adjustable t :fill-pointer 0)))
;     (when (read-json-char* input-stream #\])
;       (return result))
;    read-item
;     (vector-push-extend (read-json input-stream) result)
;     (when (read-json-char* input-stream #\,)
;       (go read-item))
;     (read-json-char input-stream #\])
;     (return result)))


(defun read-json-fraction (input-stream value)
  (do ((mult 1d-1 (* mult 1d-1))
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
    (when (equalp #\e (peek-char nil input-stream nil))
      (read-char input-stream)
      (setq value (* value (expt 10d0 (read-json-integer input-stream)))))
    value))


(defun read-json (&optional (input-stream *standard-input*) (eof-error-p t) eof-value single-value-p)
  (prog1
    (let ((ch (peek-char t input-stream nil :eof)))
      (case ch
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
          (read-json-token input-stream "null" *null*))
        (:eof
          (when eof-error-p
            (error 'end-of-file :stream input-stream))
          eof-value)
        (otherwise
          (error 'shasht-parse-error :message (format nil "Unexpected character ~A found." ch)))))
    (when single-value-p
      (let ((ch (peek-char t input-stream nil)))
        (when ch
          (error 'shasht-parse-error :message (format nil "Unexpected character ~A found at end of file." ch)))))))


(defun from-json (value)
  (with-input-from-string (input-stream value)
    (read-json input-stream)))


