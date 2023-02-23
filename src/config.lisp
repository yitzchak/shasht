(in-package :shasht)

(defvar *read-default-true-value* t
  "The default value to return when reading a true token.")

(defvar *read-default-false-value* nil
  "The default value to return when reading a false token.")

(defvar *read-default-null-value* :null
  "The default value to return when reading a null token.")

(defvar *read-default-array-format* :vector
  "The default format to use when reading an array. Current supported formats
are :vector or :list.")

(defvar *read-default-object-format* :hash-table
  "The default format to use when reading an object. Current supported formats
are :hash-table, :alist or :plist.")

(defvar *read-length* nil
  "The maximum number of values in an array or an object.")

(defvar *read-level* nil
  "The maximum number of levels to allow during reading.")

(defvar *read-hash-table-test* 'equal
  "The :test designator for make-hash-table (used when reading objects into hash-tables).")

(defvar *write-ascii-encoding* nil
  "If true then any non ASCII values will be encoded using Unicode escape sequences.")

(defvar *write-true-values* '(t :true)
  "Values that will be written as a true token.")

(defvar *write-false-values* '(nil :false)
  "Values that will be written as a false token.")

(defvar *write-null-values* '(:null)
  "Values that will be written as a null token.")

(defvar *write-empty-array-values* '(:empty-array)
  "Values that will be written as an empty array.")

(defvar *write-empty-object-values* '(:empty-object)
  "Values that will be written as an empty object.")

(defvar *write-alist-as-object* nil
  "If true then assocation lists will be written as an object.")

(defvar *write-plist-as-object* nil
  "If true then property lists will be written as an object.")

(defvar *write-indent-string* "  "
  "The string to use when indenting objects and arrays.")

(defvar *write-array-tags* '(:array)
  "Indicators in the CAR of a list that indicate that the CDR or the list
should be written as an array.")

(defvar *write-object-alist-tags* '(:object-alist)
  "Indicators in the CAR of a list that indicate that the CDR of the list
is an alist and should be written as object.")

(defvar *write-object-plist-tags* '(:object-plist)
  "Indicators in the CAR of a list that indicate that the CDR of the list
is an plist and should be written as object.")

(defvar *symbol-name-function* #'symbol-name)

