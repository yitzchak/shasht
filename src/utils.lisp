(in-package :shasht)

(defun control-char-p (ch)
  (< (char-code ch) 32))

