(in-package :shasht)


;(declaim (inline skip-whitespace expect-char test-char expect-string))

(defparameter +whitespace-haystack+
              (coerce '(#\space #\newline #\return #\tab) 'string ))


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


(defmacro array-begin (expression-stack)
  `(push (make-reader-state :type :array
                            :value (unless (eql *read-default-array-format* :list)
                                     (make-array 32 :adjustable t :fill-pointer 0)))
         ,expression-stack))


(defmacro array-end (expression-stack)
  `(value ,expression-stack
          (if (eql *read-default-array-format* :list)
            (nreverse (reader-state-value (pop ,expression-stack)))
            (reader-state-value (pop ,expression-stack)))))


(defmacro object-begin (expression-stack)
  `(push (make-reader-state :type :object
                            :value (unless (eql *read-default-object-format* :alist)
                                     (make-hash-table :test #'equal)))
         ,expression-stack))


(defmacro object-end (expression-stack)
  `(value ,expression-stack
          (if (eql *read-default-object-format* :alist)
            (nreverse (reader-state-value (pop ,expression-stack)))
            (reader-state-value (pop ,expression-stack)))))


(defmacro object-key (expression-stack key)
  `(setf (reader-state-key (first ,expression-stack)) ,key))


(defmacro value (expression-stack value-form)
  (let ((value (gensym)))
    `(let ((,value ,value-form))
       (cond
         ((null ,expression-stack)
           (push (make-reader-state :value ,value) ,expression-stack))
         ((and (eql :array (reader-state-type (first ,expression-stack)))
               (eql :list *read-default-array-format*))
           (push ,value (reader-state-value (first ,expression-stack))))
         ((eql :array (reader-state-type (first ,expression-stack)))
           (vector-push-extend ,value (reader-state-value (first ,expression-stack))))
         ((eql :alist *read-default-object-format*)
           (push (cons (reader-state-key (first ,expression-stack)) ,value) (reader-state-value (first ,expression-stack))))
         (t
           (setf (gethash (reader-state-key (first ,expression-stack)) (reader-state-value (first ,expression-stack))) ,value))))))


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
  (let* ((ch (read-char input-stream nil))
         (ev (funcall test value ch)))
    (unless ev
      (unread-char ch input-stream))
    ev))


(defun test-char-string (input-stream haystack)
  (declare (type stream input-stream)
           (type string haystack))
  (let ((ch (read-char input-stream nil)))
    (cond
      ((null ch)
        nil)
      ((position ch haystack :test #'char=)
        t)
      (t
        (unread-char ch input-stream)
        nil))))


(defun skip-whitespace (input-stream)
  (declare (type stream input-stream))
  (prog (ch)
   read-next
    (when (member (setf ch (read-char input-stream nil))
                  '(#\space #\newline #\return #\tab)
                  :test #'equal)
      (go read-next))
    (when ch
      (unread-char ch input-stream))))


(defun expect-string (input-stream token)
  (declare (type stream input-stream)
           (type simple-string token))
  (dotimes (pos (length token))
    (expect-char input-stream (char token pos))))


(defun read-digit* (input-stream &optional hexadecimal)
  (declare (type stream input-stream)
           (type boolean hexadecimal))
  (let* ((ch (read-char input-stream nil))
         (weight (when ch
                   (digit-char-p ch (if hexadecimal 16 10)))))
    (declare (type (or null character) ch)
             (type (or null fixnum) weight))
    (when (and ch (not weight))
      (unread-char ch input-stream))
    weight))


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
  (declare (type stream input-stream))
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
        (vector-push-extend #\backspace result)
        (go read-next))
      ((equal #\f ch)
        (vector-push-extend #\page result)
        (go read-next))
      ((equal #\n ch)
        (vector-push-extend #\newline result)
        (go read-next))
      ((equal #\r ch)
        (vector-push-extend #\return result)
        (go read-next))
      ((equal #\t ch)
        (vector-push-extend #\tab result)
        (go read-next))
      ((equal #\" ch)
        (vector-push-extend #\" result)
        (go read-next))
      ((equal #\/ ch)
        (vector-push-extend #\/ result)
        (go read-next))
      ((equal #\\ ch)
        (vector-push-extend #\\ result)
        (go read-next))
      ((not (equal #\u ch))
        (error 'shasht-parse-error :char ch :expected (list #\b #\f #\n #\r #\t #\" #\/ #\\ #\u))))

    (setq hi (read-encoded-char input-stream))

    (cond
      ((high-surrogate-p hi)
        #+cmucl (vector-push-extend (code-char hi) result) ; CMUCL is UTF-16
        (expect-char input-stream #\\)
        (expect-char input-stream #\u)
        (setq lo (read-encoded-char input-stream))
        (vector-push-extend
          (code-char #+cmucl lo
                     #-cmucl (+ #x10000
                                (- lo #xdc00)
                                (* #x400 (- hi #xd800))))
          result))
      (t
        (vector-push-extend (code-char hi) result)))

    (go read-next)))


(defun read-json-number (input-stream)
  (declare (type stream input-stream))
  (prog ((mantissa 0)
         (exponent 0)
         (frac-exponent 0)
         (mantissa-accum #'+)
         (exponent-accum #'+)
         digit
         ch)
    (declare (type integer mantissa exponent frac-exponent)
             (type function manitissa-accum exponent-accum)
             (type (or null integer) digit)
             (type (or null character) ch))
    (cond
      ((null (setf ch (read-char input-stream nil)))
        (error 'shasht-parse-error :expected '(#\- #\. #\e #\E #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
      ((char= #\- ch)
        (setf mantissa-accum #'-)
        (setf ch (read-char input-stream nil))))
    (cond
      ((null ch)
        (error 'shasht-parse-error :expected '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
      ((char= #\0 ch)
        (cond
          ((char= #\. (setf ch (read-char input-stream nil)))
            (go read-frac))
          ((or (char= #\e ch)
               (char= #\E ch))
            (go read-exp))
          (t
            (unread-char ch input-stream)
            (return 0))))
      ((setf digit (digit-char-p ch))
        (setf mantissa (funcall mantissa-accum digit)))
      (t
        (error 'shasht-parse-error :char ch :expected '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))
   read-int-digit
    (cond
      ((null (setf ch (read-char input-stream nil)))
        (return mantissa))
      ((char= #\. ch))
      ((or (char= #\e ch)
           (char= #\E ch))
        (go read-exp))
      ((setf digit (digit-char-p ch))
        (setf mantissa (funcall mantissa-accum (* 10 mantissa) digit))
        (go read-int-digit))
      (t
        (unread-char ch input-stream)
        (return mantissa)))
   read-frac
    (cond
      ((null (setf ch (read-char input-stream nil)))
        (error 'shasht-parse-error :expected '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
      ((setf digit (digit-char-p ch))
        (decf frac-exponent)
        (setf mantissa (funcall mantissa-accum (* 10 mantissa) digit)))
      (t
        (error 'shasht-parse-error :char ch :expected '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))
   read-frac-digit
    (cond
      ((null (setf ch (read-char input-stream nil)))
        (go finish))
      ((or (char= #\e ch)
           (char= #\E ch)))
      ((setf digit (digit-char-p ch))
        (decf frac-exponent)
        (setf mantissa (funcall mantissa-accum (* 10 mantissa) digit))
        (go read-frac-digit))
      (t
        (unread-char ch input-stream)
        (go finish)))
   read-exp
    (cond
      ((null (setf ch (read-char input-stream nil)))
        (error 'shasht-parse-error :char ch :expected '(#\+ #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
      ((char= #\+ ch)
        (setf ch (read-char input-stream nil)))
      ((char= #\- ch)
        (setf ch (read-char input-stream nil))
        (setf exponent-accum #'-)))
    (cond
      ((and ch
            (setf digit (digit-char-p ch)))
        (setf exponent (funcall exponent-accum (* 10 exponent) digit)))
      (t
        (error 'shasht-parse-error :char ch :expected '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))
   read-exp-digit
    (cond
      ((null (setf ch (read-char input-stream nil))))
      ((setf digit (digit-char-p ch))
        (setf exponent (funcall exponent-accum (* 10 exponent) digit))
        (go read-exp-digit))
      (t
        (unread-char ch input-stream)))
   finish
    (return (* mantissa (expt (coerce 10 *read-default-float-format*) (+ frac-exponent exponent))))))


(defun read-json (&optional (input-stream *standard-input*) (eof-error-p t) eof-value single-value-p)
  (declare (type boolean eof-error-p single-value-p))
  (prog (ch objects-p expression-stack)
    (declare (type (or null character) ch))
   read-next
    ; If we are in an object then read the key first.
    (when (first objects-p)
      (object-key expression-stack (read-json-string input-stream))
      (skip-whitespace input-stream)
      (expect-char input-stream #\:))

    (skip-whitespace input-stream)
    (setq ch (read-char input-stream nil))

    (cond
      ((null ch)
        (when eof-error-p
          (error 'end-of-file :stream input-stream))
        (push eof-value expression-stack))
      ((char= #\{ ch)
        (skip-whitespace input-stream)
        (object-begin expression-stack)
        (cond
          ((test-char input-stream #\})
            (object-end expression-stack))
          (t
            (push t objects-p)
            (go read-next))))
      ((char= #\[ ch)
        (skip-whitespace input-stream)
        (array-begin expression-stack)
        (cond
          ((test-char input-stream #\])
            (array-end expression-stack))
          (t
            (push nil objects-p)
            (go read-next))))
      ((char= #\" ch)
        (unread-char ch input-stream)
        (value expression-stack (read-json-string input-stream)))
      ((integer-char-p ch)
        (unread-char ch input-stream)
        (value expression-stack (read-json-number input-stream)))
      ((char= #\t ch)
        (expect-char input-stream #\r)
        (expect-char input-stream #\u)
        (expect-char input-stream #\e)
        (value expression-stack *read-default-true-value*))
      ((char= #\f ch)
        (expect-char input-stream #\a)
        (expect-char input-stream #\l)
        (expect-char input-stream #\s)
        (expect-char input-stream #\e)
        (value expression-stack *read-default-false-value*))
      ((char= #\n ch)
        (expect-char input-stream #\u)
        (expect-char input-stream #\l)
        (expect-char input-stream #\l)
        (value expression-stack *read-default-null-value*))
      (t
        (error 'shasht-parse-error :char ch)))

   read-separator
    (skip-whitespace input-stream)
    (cond
      ((null objects-p) ; We aren't in an object or an array so exit.
        (when single-value-p
          (let ((ch (read-char input-stream nil)))
            (when ch
              (error 'shasht-parse-error :char ch))))
        (return (reader-state-value (first expression-stack))))
      ((test-char input-stream #\,) ; If there is a comma then there is another value or key/value.
        (go read-next))
      ((first objects-p) ; We are at the end of an object.
        (expect-char input-stream #\})
        (object-end expression-stack))
      (t ; We are at the end of an array.
        (expect-char input-stream #\])
        (array-end expression-stack)))
    ; We just finished an object or an array so look for more separators.
    (pop objects-p)
    (go read-separator)))


(defun from-json (value)
  ;(with-input-from-string (input-stream value)
;    (read-json input-stream)))
  (read-json (make-string-input-stream value)))

