(in-package :shasht)


(declaim (inline skip-whitespace expect-char test-char expect-string))


(define-condition shasht-parse-error (parse-error)
  ((expected
     :reader shasht-parse-error-expected
     :initarg :expected
     :initform nil)
   (char
     :reader shasht-parse-error-char
     :initarg :char
     :type character))
  (:report (lambda (condition stream)
             (format stream "Unexpected character ~A found during parsing."
                            (shasht-parse-error-char condition)))))


(defstruct reader-state
  (type :value)
  value
  (key nil))


(defclass reader ()
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


(defmethod json-array-begin ((instance reader))
  (push (make-reader-state :type :array
                           :value (unless (eql *read-default-array-format* :list)
                                    (make-array 32 :adjustable t :fill-pointer 0)))
        (expression-stack instance)))


(defmethod json-array-end ((instance reader))
  (let ((expr (pop (expression-stack instance))))
    (json-value instance
                (if (eql *read-default-array-format* :list)
                  (nreverse (reader-state-value expr))
                  (reader-state-value expr)))))


(defmethod json-object-begin ((instance reader))
  (push (make-reader-state :type :object
                           :value (unless (eql *read-default-object-format* :alist)
                                    (make-hash-table :test #'equal)))
        (expression-stack instance)))


(defmethod json-object-end ((instance reader))
  (let ((expr (pop (expression-stack instance))))
    (json-value instance
                (if (eql *read-default-object-format* :alist)
                  (nreverse (reader-state-value expr))
                  (reader-state-value expr)))))


(defmethod json-key ((instance reader) key)
  (setf (reader-state-key (first (expression-stack instance))) key))


(defmethod json-value ((instance reader) value)
  (let ((expr (first (expression-stack instance)))
        (inter-value (case value
                       (:true *read-default-true-value*)
                       (:false *read-default-false-value*)
                       (:null *read-default-null-value*)
                       (otherwise value))))
    (cond
      ((null expr)
        (push (make-reader-state :value inter-value) (expression-stack instance)))
      ((and (eql :array (reader-state-type expr))
            (eql :list *read-default-array-format*))
        (push inter-value (reader-state-value expr)))
      ((eql :array (reader-state-type expr))
        (vector-push-extend inter-value (reader-state-value expr)))
      ((eql :alist *read-default-object-format*)
        (push (cons (reader-state-key expr) inter-value) (reader-state-value expr)))
      (t
        (setf (gethash (reader-state-key expr) (reader-state-value expr)) inter-value)))))


(defmethod json-eof ((instance reader))
  (when (eof-error-p instance)
    (error 'end-of-file :stream (input-stream instance)))
  (push (eof-value instance) (expression-stack instance)))


(defun skip-whitespace (input-stream)
  (declare (type stream input-stream))
  (tagbody
   read-next
   (when (member (peek-char nil input-stream nil) '(#\space #\newline #\return #\tab) :test #'equal)
     (read-char input-stream)
     (go read-next))))


(defun expect-char (input-stream value &key (test #'equal))
  (declare (type stream input-stream)
           (type character value))
  (let ((ch (read-char input-stream nil)))
    (declare (type (or null character) ch))
    (unless (funcall test value ch)
      (error 'shasht-parse-error :char ch :expected (list value)))))


(defun test-char (input-stream value &key (test #'equal))
  (declare (type stream input-stream)
           (type character value))
  (and (funcall test value (peek-char nil input-stream nil))
       (read-char input-stream)
       t))


(defun expect-string (input-stream token)
  (declare (type stream input-stream)
           (type simple-string token))
  (dotimes (pos (length token))
    (expect-char input-stream (char token pos))))


(defun read-digit* (input-stream &optional hexadecimal)
  (declare (type stream input-stream)
           (type boolean hexadecimal))
  (let* ((ch (peek-char nil input-stream nil))
         (weight (when ch
                   (digit-char-p ch (if hexadecimal 16 10)))))
    (declare (type (or null character) ch)
             (type (or null fixnum) weight))
    (when weight
      (read-char input-stream)
      weight)))


(defun read-digit (input-stream &optional hexadecimal)
  (declare (type stream input-stream)
           (type boolean hexadecimal))
  (let* ((ch (read-char input-stream nil))
         (weight (when ch (digit-char-p ch (if hexadecimal 16 10)))))
    (declare (type (or null character) ch)
             (type (or null fixnum) weight))
    (unless weight
      (error 'shasht-parse-error :char ch :expected (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
    weight))


(defun high-surrogate-p (code)
  (declare (type fixnum code))
  (<= #xd800 code #xdfff))


(defun read-encoded-char (input-stream)
  (declare (type stream input-stream))
  (logior (ash (read-digit input-stream t) 12)
          (ash (read-digit input-stream t) 8)
          (ash (read-digit input-stream t) 4)
          (read-digit input-stream t)))


(defun read-json-string (input-stream)
  (declare (optimize (speed 3) (safety 0))
           (type stream input-stream))
  (skip-whitespace input-stream)
  (expect-char input-stream #\")
  (prog ((result (make-array 32 :fill-pointer 0 :adjustable t :element-type 'character))
         ch hi lo)
   read-next
    (setq ch (read-char input-stream nil))
    (cond
      ((or (null ch)
           (control-char-p ch))
        (error 'shasht-parse-error :char ch))
      ((char= #\" ch)
        (return result))
      ((char/= #\\ ch)
        (vector-push-extend ch result)
        (go read-next)))

    (setq ch (read-char input-stream nil))
    (cond
      ((equal #\b ch)
        (vector-push-extend #\backspace)
        (go read-next))
      ((equal #\f ch)
        (vector-push-extend #\page)
        (go read-next))
      ((equal #\n ch)
        (vector-push-extend #\newline)
        (go read-next))
      ((equal #\r ch)
        (vector-push-extend #\return)
        (go read-next))
      ((equal #\t ch)
        (vector-push-extend #\tab)
        (go read-next))
      ((equal #\" ch)
        (vector-push-extend #\")
        (go read-next))
      ((equal #\/ ch)
        (vector-push-extend #\/)
        (go read-next))
      ((equal #\\ ch)
        (vector-push-extend #\\)
        (go read-next))
      ((not (equal #\u ch))
        (error 'shasht-parse-error :char ch :expected (list #\b #\f #\n #\r #\t #\" #\/ #\\ #\u))))

    (setq hi (read-encoded-char input-stream))

    (cond
      ((high-surrogate-p hi)
        #+cmucl (vector-push-extend (code-char hi) results) ; CMUCL is UTF-16
        (expect-char input-stream #\\)
        (expect-char input-stream #\u)
        (setq lo (read-encoded-char input-stream))
        (vector-push-extend
          (code-char #+cmucl lo
                     #-cmucl (+ #x10000
                                (- lo #xdc00)
                                (* #x400 (- hi #xd800))))
          results))
      (t
        (vector-push-extend (code-char hi) results)))

    (go read-next)))


(defun read-json-fraction (input-stream value)
  (do ((exponent -1 (1- exponent))
       (result value)
       (weight (read-digit input-stream) (read-digit* input-stream)))
      ((not weight) result)
    (setq result (+ result (* (expt (coerce 10 *read-default-float-format*) exponent) weight)))))


(defun read-json-integer (input-stream)
  (do* ((negative (test-char input-stream #\-))
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
  (do* ((negative (if (test-char input-stream #\+)
                    nil
                    (test-char input-stream #\-)))
        (result 0)
        (weight (read-digit input-stream) (read-digit* input-stream)))
       ((not weight)
         (if negative
           (- result)
           result))
    (setq result (+ (* 10 result) weight))))


(defun read-json-number (input-stream)
  (let ((value (read-json-integer input-stream)))
    (when (test-char input-stream #\.)
      (setq value (read-json-fraction input-stream value)))
    (when (test-char input-stream #\e :test #'equalp)
      (setq value (* value (expt (coerce 10 *read-default-float-format*) (read-json-exponent input-stream)))))
    value))


(defun read-json-into-handler (handler input-stream)
  (prog (ch objects-p)
    (declare (type (or null character) ch))
   read-next
    ; If we are in an object then read the key first.
    (when (first objects-p)
      (json-key handler (read-json-string input-stream))
      (skip-whitespace input-stream)
      (expect-char input-stream #\:))

    (skip-whitespace input-stream)
    (setq ch (peek-char nil input-stream nil))

    (cond
      ((null ch)
        (json-eof handler))
      ((char= #\{ ch)
        (read-char input-stream)
        (skip-whitespace input-stream)
        (json-object-begin handler)
        (cond
          ((test-char input-stream #\})
            (json-object-end handler))
          (t
            (push t objects-p)
            (go read-next))))
      ((char= #\[ ch)
        (read-char input-stream)
        (skip-whitespace input-stream)
        (json-array-begin handler)
        (cond
          ((test-char input-stream #\])
            (json-array-end handler))
          (t
            (push nil objects-p)
            (go read-next))))
      ((char= #\" ch)
        (json-value handler (read-json-string input-stream)))
      ((position ch "-0123456789")
        (json-value handler (read-json-number input-stream)))
      ((char= #\t ch)
        (read-char input-stream)
        (expect-char input-stream #\r)
        (expect-char input-stream #\u)
        (expect-char input-stream #\e)
        (json-value handler :true))
      ((char= #\f ch)
        (read-char input-stream)
        (expect-char input-stream #\a)
        (expect-char input-stream #\l)
        (expect-char input-stream #\s)
        (expect-char input-stream #\e)
        (json-value handler :false))
      ((char= #\n ch)
        (read-char input-stream)
        (expect-char input-stream #\u)
        (expect-char input-stream #\l)
        (expect-char input-stream #\l)
        (json-value handler :null))
      (t
        (error 'shasht-parse-error :char ch)))

   read-separator
    (skip-whitespace input-stream)
    (cond
      ((null objects-p)) ; We aren't in an object or an array so exit.
      ((test-char input-stream #\,) ; If there is a comma then there is another value or key/value.
        (go read-next))
      ((first objects-p) ; We are at the end of an object so finish it and look for more separators.
        (expect-char input-stream #\})
        (json-object-end handler)
        (pop objects-p)
        (go read-separator))
      (t ; We are at the end of an array so finish it and look for more separators.
        (expect-char input-stream #\])
        (json-array-end handler)
        (pop objects-p)
        (go read-separator)))))


(defun read-json (&optional (input-stream *standard-input*) (eof-error-p t) eof-value single-value-p)
  (declare (type boolean eof-error-p single-value-p))
  (let ((handler (make-instance 'reader
                                :eof-error-p eof-error-p :eof-value eof-value :input-stream input-stream)))
    (read-json-into-handler handler input-stream)
    (when single-value-p
      (skip-whitespace input-stream)
      (let ((ch (peek-char nil input-stream nil)))
        (when ch
          (error 'shasht-parse-error :char ch))))
    (reader-state-value (first (expression-stack handler)))))


(defun from-json (value)
  (with-input-from-string (input-stream value)
    (read-json input-stream)))


