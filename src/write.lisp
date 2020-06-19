(in-package :shasht)

(defvar *ascii-encoding* nil)
(defvar *comma-needed* nil)

(defgeneric write-json (object &optional output-stream))

(defun to-json (object)
  (with-output-to-string (output-stream)
    (write-json object output-stream)))

(defun control-char-p (char-code)
  (< char-code 32))

(defun ascii-printable-p (char-code)
  (<= 32 char-code 126))

(defun supplementary-plane-p (char-code)
  (<= #x10000 char-code #x10ffff))

(defun write-object-key-value (key value &optional output-stream)
  (if *comma-needed*
    (write-char #\, output-stream)
    (setq *comma-needed* t))
  (write-json key output-stream)
  (write-char #\: output-stream)
  (write-char value output-stream))

(defmacro write-object (output-stream &body body)
  (alexandria:with-gensyms (os)
    `(let ((,os ,output-stream)
           (*comma-needed* nil))
       (write-char #\{ ,os)
       ,@body
       (write-char #\} ,os))))

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
        ((char= ch #\formfeed)
          (write-string "\\f" output-stream))
        ((char= ch #\backspace)
          (write-string "\\b" output-stream))
        ((char= ch #\")
          (write-string "\\\"" output-stream))
        ((char= ch #\\)
          (write-string "\\\\" output-stream))
        ((control-char-p code)
          (format output-stream "\\u~4,'0x" code))
        ((or (not *ascii-encoding*)
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
  (write-char #\" output-stream))
  
