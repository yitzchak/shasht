(in-package :shasht)


(defun control-char-p (ch)
  (< (char-code ch) 32))


(defun plistp (plist)
  (do ((tail plist (cddr tail)))
      ((null tail) t)
    (unless (and (keywordp (car tail))
                 (cdr tail))
      (return nil))))


(defun alistp (alist)
  (every #'consp alist))
