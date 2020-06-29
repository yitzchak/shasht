(in-package :shasht)

(define-condition shasht-parse-error (parse-error)
  ((message
     :initarg :message
     :reader shasht-parse-error-message
     :type string))
  (:report (lambda (condition stream)
             (write-line (shasht-parse-error-message condition) stream))))


(defstruct json-expression
  (type :value)
  value
  (key nil))


(defclass reader-state ()
  ((expression-stack
     :accessor expression-stack
     :initform nil)
   (eof-error-p
     :accessor eof-error-p
     :initarg :eof-error-p)
   (eof-value
     :accessor eof-value
     :initarg :eof-value)
   (input-stream
     :accessor input-stream
     :initarg :input-stream)))


(defmethod json-array-begin ((instance reader-state))
  (push (make-json-expression :type :array
                              :value (unless (eql *read-default-array-format* :list)
                                       (make-array 32 :adjustable t :fill-pointer 0)))
        (expression-stack instance)))


(defmethod json-array-end ((instance reader-state))
  (let ((expr (pop (expression-stack instance))))
    (json-value instance
           (if (eql *read-default-array-format* :list)
             (nreverse (json-expression-value expr))
             (json-expression-value expr)))))


(defmethod json-object-begin ((instance reader-state))
  (push (make-json-expression :type :object
                              :value (unless (eql *read-default-object-format* :alist)
                                       (make-hash-table :test #'equal)))
        (expression-stack instance)))


(defmethod json-object-end ((instance reader-state))
  (let ((expr (pop (expression-stack instance))))
    (json-value instance
           (if (eql *read-default-object-format* :alist)
             (nreverse (json-expression-value expr))
             (json-expression-value expr)))))


(defmethod json-key ((instance reader-state) key)
  (setf (json-expression-key (first (expression-stack instance))) key))


(defmethod json-value ((instance reader-state) value)
  (let ((expr (first (expression-stack instance)))
        (inter-value (case value
                       (:true *read-default-true-value*)
                       (:false *read-default-false-value*)
                       (:null *read-default-null-value*)
                       (otherwise value))))
    (cond
      ((null expr)
        (push (make-json-expression :value inter-value) (expression-stack instance)))
      ((and (eql :array (json-expression-type expr))
            (eql :list *read-default-array-format*))
        (push inter-value (json-expression-value expr)))
      ((eql :array (json-expression-type expr))
        (vector-push-extend inter-value (json-expression-value expr)))
      ((eql :alist *read-default-object-format*)
        (push (cons (json-expression-key expr) inter-value) (json-expression-value expr)))
      (t
        (setf (gethash (json-expression-key expr) (json-expression-value expr)) inter-value)))))


(defmethod json-eof ((instance reader-state))
  (when (eof-error-p instance)
    (error 'end-of-file :stream (input-stream instance)))
  (push (eof-value instance) (expression-stack instance)))


(defmethod json-error ((instance reader-state) control &rest args)
  (error 'shasht-parse-error :message (apply #'format nil control args)))


(defun skip-whitespace (input-stream)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type stream input-stream))
  (tagbody
   read-next
   (when (member (peek-char nil input-stream nil) '(#\space #\newline #\return #\tab) :test #'equal)
     (read-char input-stream)
     (go read-next))))


(defun read-json-char (handler input-stream value &optional skip-whitespace (case-sensitive t))
  (declare (type stream input-stream)
           (type character value)
           (type boolean skip-whitespace case-sensitive))
  (when skip-whitespace
    (skip-whitespace input-stream))
  (let ((ch (read-char input-stream nil)))
    (declare (type (or null character) ch))
    (cond
      ((null ch)
        (json-error handler "Expected the next character to be ~A but encountered end of file first." value))
      ((or (and case-sensitive (char= value ch))
           (and (not case-sensitive) (char-equal value ch)))
        ch)
      (t
        (json-error handler "Expected the next character to be ~A but found ~A instead." value ch)))))


(defun read-json-char* (handler input-stream value &optional skip-whitespace (case-sensitive t))
  (declare (type stream input-stream)
           (type character value)
           (type boolean skip-whitespace case-sensitive))
  (when skip-whitespace
    (skip-whitespace input-stream))
  (let ((ch (peek-char nil input-stream nil)))
    (declare (type (or null character) ch))
    (when (and ch
               (or (and case-sensitive (char= value ch))
                   (and (not case-sensitive) (char-equal value ch))))
      (read-char input-stream))))


(defun read-json-token-into-handler (handler input-stream token result &optional skip-whitespace)
  (declare (type stream input-stream)
           (type simple-string token)
           (type boolean skip-whitespace))
  (dotimes (pos (length token) result)
    (read-json-char handler input-stream (char token pos) (and skip-whitespace (zerop pos))))
  (json-value handler result))


(defun read-digit* (handler input-stream &optional (radix 10))
  (declare (type stream input-stream)
           (type fixnum radix))
  (let* ((ch (peek-char nil input-stream nil))
         (weight (when ch
                   (digit-char-p ch radix))))
    (declare (type (or null character) ch)
             (type (or null fixnum) weight))
    (when weight
      (read-char input-stream)
      weight)))


(defun read-digit (handler input-stream &optional (radix 10))
  (declare (type stream input-stream)
           (type fixnum radix))
  (let* ((ch (read-char input-stream nil))
         (weight (when ch (digit-char-p ch radix))))
    (declare (type (or null character) ch)
             (type (or null fixnum) weight))
    (cond
      ((null ch)
        (json-error handler "Expected the next character to be a digit in base ~A but encountered end of file first." radix))
      (weight
        weight)
      (t
        (json-error handler "Expected the next character to be a digit in base ~A but found ~A instead." radix ch)))))


(defun high-surrogate-p (code)
  (declare (type fixnum code))
  (<= #xd800 code #xdfff))


(defun read-encoded-char (handler input-stream)
  (declare (type stream input-stream))
  (logior (ash (read-digit handler input-stream 16) 12)
          (ash (read-digit handler input-stream 16) 8)
          (ash (read-digit handler input-stream 16) 4)
          (read-digit handler input-stream 16)))


(defun read-json-escape (handler input-stream)
  (declare (type stream input-stream))
  (let ((ch (read-char input-stream nil :eof)))
    (case ch
      (:eof
        (json-error handler "Expected the next character an escape but encountered end of file first."))
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
        (let ((ech (read-encoded-char handler input-stream)))
          (code-char
            (if (high-surrogate-p ech)
              (progn
                (read-json-char handler input-stream #\\)
                (read-json-char handler input-stream #\u)
                (+ #x10000
                   (- (read-encoded-char handler input-stream) #xdc00)
                   (* #x400 (- ech #xd800))))
              ech))))
      (otherwise
        (json-error handler "Unknown escape sequence \\~A found." ch)))))


(defun read-json-string-into-handler (handler input-stream &optional key)
  (prog (ch (result (make-array 32 :fill-pointer 0 :adjustable t :element-type 'character)))
    (read-json-char handler input-stream #\" t)
   read-next
    (setq ch (read-char input-stream nil))
    (cond
      ((null ch)
        (json-error handler "Unexpected end of file in string."))
      ((char= ch #\\)
        (vector-push-extend (read-json-escape handler input-stream) result)
        (go read-next))
      ((and key (char= #\" ch))
        (json-key handler result))
      ((char= #\" ch)
        (json-value handler result))
      ((control-char-p ch)
        (json-error handler "Control character found in string."))
      (t
        (vector-push-extend ch result)
        (go read-next)))))


(defun read-json-object-into-handler (handler input-stream)
  (read-json-char handler input-stream #\{ t)
  (json-object-begin handler)
  (if (read-json-char* handler input-stream #\} t)
    (json-object-end handler)
    (tagbody
     read-key-value
      (read-json-string-into-handler handler input-stream t)
      (read-json-char handler input-stream #\: t)
      (read-json-into-handler handler input-stream)
      (when (read-json-char* handler input-stream #\, t)
        (go read-key-value))
      (read-json-char handler input-stream #\} t)
      (json-object-end handler))))


(defun read-json-array-into-handler (handler input-stream)
  (read-json-char handler input-stream #\[ t)
  (json-array-begin handler)
  (if (read-json-char* handler input-stream #\] t)
    (json-array-end handler)
    (tagbody
     read-item
      (read-json-into-handler handler input-stream)
      (when (read-json-char* handler input-stream #\, t)
        (go read-item))
      (read-json-char handler input-stream #\] t)
      (json-array-end handler))))


(defun read-json-fraction (handler input-stream value)
  (do ((exponent -1 (1- exponent))
       (result value)
       (weight (read-digit handler input-stream) (read-digit* handler input-stream)))
      ((not weight) result)
    (setq result (+ result (* (expt (coerce 10 *read-default-float-format*) exponent) weight)))))


(defun read-json-integer (handler input-stream)
  (do* ((negative (read-json-char* handler input-stream #\-))
        (result 0)
        (weight (read-digit handler input-stream) (read-digit* handler input-stream)))
       ((not weight)
         (if negative
           (- result)
           result))
    (when (and (zerop result) (zerop weight))
      (return 0))
    (setq result (+ (* 10 result) weight))))


(defun read-json-exponent (handler input-stream)
  (do* ((negative (if (read-json-char* handler input-stream #\+)
                    nil
                    (read-json-char* handler input-stream #\-)))
        (result 0)
        (weight (read-digit handler input-stream) (read-digit* handler input-stream)))
       ((not weight)
         (if negative
           (- result)
           result))
    (setq result (+ (* 10 result) weight))))


(defun read-json-number-into-handler (handler input-stream)
  (let ((value (read-json-integer handler input-stream)))
    (when (read-json-char* handler input-stream #\.)
      (setq value (read-json-fraction handler input-stream value)))
    (when (read-json-char* handler input-stream #\e nil nil)
      (setq value (* value (expt (coerce 10 *read-default-float-format*) (read-json-exponent handler input-stream)))))
    (json-value handler value)))


(defun read-json-into-handler (handler input-stream)
  (skip-whitespace input-stream)
  (let ((ch (peek-char nil input-stream nil :eof)))
    (case ch
      (#\{
        (read-json-object-into-handler handler input-stream))
      (#\[
        (read-json-array-into-handler handler input-stream))
      (#\"
        (read-json-string-into-handler handler input-stream))
      ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
        (read-json-number-into-handler handler input-stream))
      (#\t
        (read-json-token-into-handler handler input-stream "true" :true))
      (#\f
        (read-json-token-into-handler handler input-stream "false" :false))
      (#\n
        (read-json-token-into-handler handler input-stream "null" :null))
      (:eof
        (json-eof handler))
      (otherwise
        (json-error handler "Unexpected character ~A found." ch)))))


(defun read-json (&optional (input-stream *standard-input*) (eof-error-p t) eof-value single-value-p)
  (let ((handler (make-instance 'reader-state
                   :eof-error-p eof-error-p :eof-value eof-value :input-stream input-stream)))
    (read-json-into-handler handler input-stream)
    (when single-value-p
      (skip-whitespace input-stream)
      (let ((ch (peek-char nil input-stream nil)))
        (when ch
          (json-error handler "Unexpected character ~A found at end of file." ch))))
    (json-expression-value (first (expression-stack handler)))))


(defun from-json (value)
  (with-input-from-string (input-stream value)
    (read-json input-stream)))


