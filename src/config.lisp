(in-package :shasht)

(defvar *read-default-true-value* t)
(defvar *read-default-false-value* nil)
(defvar *read-default-null-value* :null)
(defvar *read-default-array-format* :vector)
(defvar *read-default-object-format* :hash-table)

(defvar *write-ascii-encoding* nil)
(defvar *write-true-values* '(t :true))
(defvar *write-false-values* '(nil :false))
(defvar *write-null-values* '(:null))
(defvar *write-alist-as-object* nil)
(defvar *write-plist-as-object* nil)

