(in-package #:shasht/test)


(defun read-json (name)
  (with-open-file (stream (asdf:component-pathname (asdf:find-component :shasht/test (list "src" "test" "fixtures" name))))
    (let ((*read-default-float-format* 'long-float))
      (shasht:read-json stream t nil t))))


(defmacro create-tests ()
  `(progn ,@(mapcan (lambda (component)
                      (when (typep component 'asdf:static-file)
                        (let ((name (asdf:component-name component)))
                          (case (char name 0)
                            (#\y
                              (list `(define-test ,(format nil "Will parse ~A" name)
                                                  :time-limit 1
                                                  (finish (read-json ,name)))))
                            (#\n
                              (list `(define-test ,(format nil "Will not parse ~A" name)
                                                  :time-limit 1
                                                  (fail (read-json ,name)))))))))
                    (asdf:component-children (asdf:find-component :shasht/test '("src" "test" "fixtures"))))))


(eval-when (:execute :load-toplevel :compile-toplevel)
  (create-tests))
