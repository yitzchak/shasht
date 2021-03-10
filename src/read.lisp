(in-package :shasht)


(declaim (inline skip-whitespace)
         #+(or)(optimize (speed 3) (safety 0))
         (ftype (function (stream) fixnum) read-encoded-char)
         (ftype (function (stream boolean) string) read-json-string)
         (ftype (function (stream) number) read-json-number)
         (ftype (function (stream character boolean boolean) boolean) expect-char))


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


(defun expect-char (input-stream value skip-whitespace error-p)
  (declare (type stream input-stream)
           (type character value))
  (prog (ch)
    (declare (type (or null character) ch))
   repeat
    (cond
      ((and (null (setf ch (read-char input-stream nil)))
            error-p)
        (error 'shasht-parse-error :char ch :expected (list value)))
      ((null ch)
        (return nil))
      ((char= value ch)
        (return t))
      ((and skip-whitespace
            (member ch '(#\space #\newline #\return #\tab) :test #'char=))
        (go repeat))
      (error-p
        (error 'shasht-parse-error :char ch :expected (list value)))
      (t
        (unread-char ch input-stream)
        (return nil)))))


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


(defun read-encoded-char (input-stream)
  (declare (type stream input-stream))
  (prog (ch digit (result 0) (count 16))
    (declare (type (or null fixnum) digit result count)
             (type (or null character) ch))
   repeat
    (when (or (null (setf ch (read-char input-stream nil)))
              (null (setf digit (digit-char-p ch 16))))
      (error 'shasht-parse-error :char ch :expected (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
    (setf result (logior result (ash digit (decf count 4))))
    (when (zerop count)
      (return result))
    (go repeat)))


(defun read-json-string (input-stream skip-quote)
  (declare (type stream input-stream)
           (type boolean skip-quote))
  (unless skip-quote
    (expect-char input-stream #\" t t))
  (prog ((result (make-array 32 :fill-pointer 0 :adjustable t :element-type 'character))
         (codepoint 0)
         ch)
    (declare (type (or null character) ch)
             (type fixnum codepoint))
   read-next
    (cond
      ((or (null (setf ch (read-char input-stream nil)))
           (control-char-p ch))
        (error 'shasht-parse-error :char ch))
      ((char= #\" ch)
        (return result))
      ((char/= #\\ ch)
        (vector-push-extend ch result)
        (go read-next)))

    (cond
      ((null (setf ch (read-char input-stream nil)))
        (error 'shasht-parse-error :char ch :expected (list #\b #\f #\n #\r #\t #\" #\/ #\\ #\u)))
      ((char= #\b ch)
        (vector-push-extend #\backspace result)
        (go read-next))
      ((char= #\f ch)
        (vector-push-extend #\page result)
        (go read-next))
      ((char= #\n ch)
        (vector-push-extend #\newline result)
        (go read-next))
      ((char= #\r ch)
        (vector-push-extend #\return result)
        (go read-next))
      ((char= #\t ch)
        (vector-push-extend #\tab result)
        (go read-next))
      ((char= #\" ch)
        (vector-push-extend #\" result)
        (go read-next))
      ((char= #\/ ch)
        (vector-push-extend #\/ result)
        (go read-next))
      ((char= #\\ ch)
        (vector-push-extend #\\ result)
        (go read-next))
      ((char/= #\u ch)
        (error 'shasht-parse-error :char ch :expected (list #\b #\f #\n #\r #\t #\" #\/ #\\ #\u))))

    (cond
      ((typep (setf codepoint (read-encoded-char input-stream)) 'high-surrogate)
        #+cmucl (vector-push-extend (code-char codepoint) result) ; CMUCL is UTF-16
        (expect-char input-stream #\\ nil t)
        (expect-char input-stream #\u nil t)
        (vector-push-extend (code-char #+cmucl (read-encoded-char input-stream)
                                       #-cmucl (surrogates-to-codepoint codepoint (read-encoded-char input-stream)))
                            result))
      (t
        (vector-push-extend (code-char codepoint) result)))

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
             (type function mantissa-accum exponent-accum)
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


(defun read-json (&optional input-stream-or-string (eof-error-p t) eof-value single-value-p)
  "Read a JSON value. Reading is influenced by the dynamic variables
*read-default-true-value*, *read-default-false-value*, *read-default-null-value*,
*read-default-array-format*, *read-default-object-format* and
common-lisp:*read-default-float-format* which each determine the default values
and formats used. The following arguments also control the behavior of the read.

* input-stream-or-string - a stream, a string or t. If t is passed then
  *standard-input* is used.
* eof-error-p - if true signal eof with error, otherwise return eof-value.
* eof-value - value used if eof-error-p is nil.
* single-value-p - Check for trailing junk after read is complete."
  (declare (type boolean eof-error-p single-value-p))
  (prog (ch objects-p expression-stack
         (input-stream (cond
                          ((null input-stream-or-string)
                             *standard-input*)
                          ((stringp input-stream-or-string)
                            (make-string-input-stream input-stream-or-string))
                          (t
                            input-stream-or-string))))
    (declare (type (or null character) ch))
   read-next
    ; If we are in an object then read the key first.
    (when (first objects-p)
      (object-key expression-stack (read-json-string input-stream nil))
      (expect-char input-stream #\: t t))

    (skip-whitespace input-stream)
    (setq ch (read-char input-stream nil))

    (cond
      ((null ch)
        (when eof-error-p
          (error 'end-of-file :stream input-stream))
        (push eof-value expression-stack))
      ((char= #\{ ch)
        (object-begin expression-stack)
        (cond
          ((expect-char input-stream #\} t nil)
            (object-end expression-stack))
          (t
            (push t objects-p)
            (go read-next))))
      ((char= #\[ ch)
        (array-begin expression-stack)
        (cond
          ((expect-char input-stream #\] t nil)
            (array-end expression-stack))
          (t
            (push nil objects-p)
            (go read-next))))
      ((char= #\" ch)
        (value expression-stack (read-json-string input-stream t)))
      ((integer-char-p ch)
        (unread-char ch input-stream)
        (value expression-stack (read-json-number input-stream)))
      ((char= #\t ch)
        (expect-char input-stream #\r nil t)
        (expect-char input-stream #\u nil t)
        (expect-char input-stream #\e nil t)
        (value expression-stack *read-default-true-value*))
      ((char= #\f ch)
        (expect-char input-stream #\a nil t)
        (expect-char input-stream #\l nil t)
        (expect-char input-stream #\s nil t)
        (expect-char input-stream #\e nil t)
        (value expression-stack *read-default-false-value*))
      ((char= #\n ch)
        (expect-char input-stream #\u nil t)
        (expect-char input-stream #\l nil t)
        (expect-char input-stream #\l nil t)
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
      ((expect-char input-stream #\, nil nil) ; If there is a comma then there is another value or key/value.
        (go read-next))
      ((first objects-p) ; We are at the end of an object.
        (expect-char input-stream #\} nil t)
        (object-end expression-stack))
      (t ; We are at the end of an array.
        (expect-char input-stream #\] nil t)
        (array-end expression-stack)))
    ; We just finished an object or an array so look for more separators.
    (pop objects-p)
    (go read-separator)))


(defun read-json* (&key stream (eof-error t) eof-value single-value
                        (true-value t) false-value (null-value :null) (array-format :vector)
                        (object-format :hash-table) (float-format 'single-float))
  "Read a JSON value. Reading is influenced by the keyword arguments and not by the dynamic
variables of `read-json`.

* stream - a stream, a string or t. If t is passed then *standard-input* is used.
* eof-error - if true signal eof with error, otherwise return eof-value.
* eof-value - value used if eof-error-p is nil.
* single-value - Check for trailing junk after read is complete.
* true-value - The value to return when reading a true token.
* false-value - The value to return when reading a false token.
* null-value - The value to return when reading a null token.
* array-format - The format to use when reading an array. Current supported formats are
  :vector or :list.
* object-format - The format to use when reading an object. Current supported formats are
  :hash-table, :alist or :plist.
* float-format - The format of floating point values."
  (let ((*read-default-true-value* true-value)
        (*read-default-false-value* false-value)
        (*read-default-null-value* null-value)
        (*read-default-array-format* array-format)
        (*read-default-object-format* object-format)
        (*read-default-float-format* float-format))
    (read-json stream eof-error eof-value single-value)))

