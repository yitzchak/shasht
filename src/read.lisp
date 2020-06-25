(in-package :shasht)


(define-condition shasht-parse-error (parse-error)
  ((message
     :initarg :message
     :reader shasht-parse-error-message))
  (:report (lambda (condition stream)
             (write-line (shasht-parse-error-message condition) stream))))


(defun skip-whitespace (input-stream)
  (do ((ch (peek-char nil input-stream nil) (peek-char nil input-stream nil)))
      ((not (member ch '(#\space #\newline #\return #\tab) :test #'equal)))
    (read-char input-stream)))


(defun read-json-char (input-stream value &optional skip-whitespace (case-sensitive t))
  (when skip-whitespace
    (skip-whitespace input-stream))
  (let ((ch (read-char input-stream nil)))
    (cond
      ((null ch)
        (error 'shasht-parse-error :message (format nil "Expected the next character to be ~A but encountered end of file first." value)))
      ((or (and case-sensitive (char= value ch))
           (and (not case-sensitive) (char-equal value ch)))
        ch)
      (t
        (error 'shasht-parse-error :message (format nil "Expected the next character to be ~A but found ~A instead." value ch))))))


(defun read-json-char* (input-stream value &optional skip-whitespace (case-sensitive t))
  (when skip-whitespace
    (skip-whitespace input-stream))
  (let ((ch (peek-char nil input-stream nil)))
    (when (and ch
               (or (and case-sensitive (char= value ch))
                   (and (not case-sensitive) (char-equal value ch))))
      (read-char input-stream))))


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
      ((control-char-p ch)
        (error 'shasht-parse-error :message (format nil "Control character found in string.")))
      (t
        (vector-push (if (char= ch #\\)
                       (read-json-escape input-stream)
                       ch)
                     result)))))


(defmacro read-json-object-with-handlers (initial extend clone)
  (alexandria:with-gensyms (result-var key-var)
    `(prog ((,result-var ,initial) ,key-var)
       (read-json-char *standard-input* #\{ t)
       (when (read-json-char* *standard-input* #\} t)
         (return (,clone ,result-var)))
      read-key-value
       (setq ,key-var (read-json-string *standard-input*))
       (read-json-char *standard-input* #\: t)
       (,extend ,key-var (read-json *standard-input*) ,result-var)
       (when (read-json-char* *standard-input* #\, t)
         (go read-key-value))
       (read-json-char *standard-input* #\} t)
       (return (,clone ,result-var)))))


(defmacro alist-push (key value alist)
  `(setf ,alist (acons ,key ,value ,alist)))


(defun hash-table-extend (key value hash-table)
  (setf (gethash key hash-table) value))


(defun read-json-object (input-stream)
  (let ((*standard-input* (or input-stream *standard-input*)))
    (ecase *read-default-object-format*
      (:hash-table
        (read-json-object-with-handlers (make-hash-table :test #'equal) hash-table-extend identity))
      (:alist
        (read-json-object-with-handlers nil alist-push nreverse))
      (otherwise
        (error 'shasht-parse-error :message (format nil "Unknown array format specified in *read-default-object-format* of ~A." *read-default-object-format*))))))


; (defun read-json-object (input-stream)
;   (read-json-char input-stream #\{ t)
;   (prog ((result (make-hash-table :test #'equal))
;          key)
;     (when (read-json-char* input-stream #\} t)
;       (return result))
;    read-key-value
;     (setf key (read-json-string input-stream))
;     (read-json-char input-stream #\: t)
;     (setf (gethash key result) (read-json input-stream))
;     (when (read-json-char* input-stream #\, t)
;       (go read-key-value))
;     (read-json-char input-stream #\} t)
;     (return result)))

()


(defmacro read-json-array-with-handlers (initial extend clone)
  (alexandria:with-gensyms (result-var)
    `(prog ((,result-var ,initial))
       (read-json-char *standard-input* #\[ t)
       (when (read-json-char* *standard-input* #\] t)
         (return (,clone ,result-var)))
      read-item
       (,extend (read-json *standard-input*) ,result-var)
       (when (read-json-char* *standard-input* #\, t)
         (go read-item))
       (read-json-char *standard-input* #\] t)
       (return (,clone ,result-var)))))


(defun read-json-array (input-stream)
  (let ((*standard-input* (or input-stream *standard-input*)))
    (case *read-default-array-format*
      (:list
        (read-json-array-with-handlers nil push nreverse))
      (:vector
        (read-json-array-with-handlers (make-array 32 :adjustable t :fill-pointer 0) vector-push-extend identity))
      (otherwise
        (error 'shasht-parse-error :message (format nil "Unknown array format specified in *read-default-array-format* of ~A." *read-default-array-format*))))))

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
  (do ((exponent -1 (1- exponent))
       (result value)
       (weight (read-digit input-stream) (read-digit* input-stream)))
      ((not weight) result)
    (setq result (+ result (* (expt (coerce 10 *read-default-float-format*) exponent) weight)))))


(defun read-json-integer (input-stream)
  (do* ((negative (read-json-char* input-stream #\-))
        (result 0)
        (weight (read-digit input-stream) (read-digit* input-stream)))
       ((not weight)
         (if negative
           (- result)
           result))
    (when (and (zerop result) (zerop weight))
      (return 0))
    (setq result (+ (* 10 result) weight))))


(defun read-json-exponent (input-stream)
  (do* ((negative (if (read-json-char* input-stream #\+)
                    nil
                    (read-json-char* input-stream #\-)))
        (result 0)
        (weight (read-digit input-stream) (read-digit* input-stream)))
       ((not weight)
         (if negative
           (- result)
           result))
    (setq result (+ (* 10 result) weight))))


(defun read-json-number (input-stream)
  (let ((value (read-json-integer input-stream)))
    (when (read-json-char* input-stream #\.)
      (setq value (read-json-fraction input-stream value)))
    (when (read-json-char* input-stream #\e nil nil)
      (setq value (* value (expt (coerce 10 *read-default-float-format*) (read-json-exponent input-stream)))))
    value))


(defun read-json (&optional (input-stream *standard-input*) (eof-error-p t) eof-value single-value-p)
  (prog2
    (skip-whitespace input-stream)
    (let ((ch (peek-char nil input-stream nil :eof)))
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
          (read-json-token input-stream "true" *read-default-true-value*))
        (#\f
          (read-json-token input-stream "false" *read-default-false-value*))
        (#\n
          (read-json-token input-stream "null" *read-default-null-value*))
        (:eof
          (when eof-error-p
            (error 'end-of-file :stream input-stream))
          eof-value)
        (otherwise
          (error 'shasht-parse-error :message (format nil "Unexpected character ~A found." ch)))))
    (when single-value-p
      (skip-whitespace input-stream)
      (let ((ch (peek-char nil input-stream nil)))
        (when ch
          (error 'shasht-parse-error :message (format nil "Unexpected character ~A found at end of file." ch)))))))


(defun from-json (value)
  (with-input-from-string (input-stream value)
    (read-json input-stream)))


