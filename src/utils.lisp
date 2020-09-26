(in-package :shasht)


(declaim (inline plistp alistp control-char-p integer-char-p ascii-printable-p
                 supplementary-plane-p high-surrogate-p)
         (optimize (speed 3) (safety 0))
         (ftype (function (fixnum) boolean)
                ascii-printable-p
                supplementary-plane-p
                high-surrogate-p)
         (ftype (function (character) boolean)
                control-char-p
                integer-char-p)
         (ftype (function (t) boolean) plistp alistp))


(defun plistp (plist)
  (when (listp plist)
    (do ((tail plist (cddr tail)))
        ((null tail) t)
      (unless (and (keywordp (car tail))
                   (cdr tail))
        (return nil)))))


(defun alistp (alist)
  (and (listp alist)
       (every #'consp alist)))


(defun control-char-p (ch)
  (< (char-code ch) 32))


(defun integer-char-p (char)
  (declare (type character char))
  (or (char<= #\0 char #\9)
      (char= char #\-)))


(defun ascii-printable-p (char-code)
  (declare (type fixnum char-code))
  (<= 32 char-code 126))


(defun supplementary-plane-p (char-code)
  (<= #x10000 char-code #x10ffff))


(defun high-surrogate-p (code)
  (declare (type fixnum code))
  (<= #xd800 code #xdfff))



