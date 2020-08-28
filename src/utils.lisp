(in-package :shasht)


(defun control-char-p (ch)
  (< (char-code ch) 32))


(defun plistp (plist)
  (when (listp plist)
    (do ((tail plist (cddr tail)))
        ((null tail) t)
      (unless (and (keywordp (car tail))
                   (cdr tail))
        (return nil)))))


;(defun alistp (alist)
;  (and (listp alist)
;    (every #'consp alist)))


(defun alistp (alist)
  (when (listp alist)
    (dolist (item alist t)
      (unless (listp item)
        (return nil)))))


(declaim (inline integer-char-p))
(defun integer-char-p (char)
  (declare (type character char))
  (or (char<= #\0 char #\9)
      (char= char #\-)))
