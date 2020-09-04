(ql:quickload '(:shasht :flamegraph))


(defun fu (count)
  (dotimes (i count)
    (shasht:read-json "{\"key1\": \"value\\n\",
\"key2\":1,\"key3\" :[\"Hello \\u2604\",  1.2e-34 ,true,
  false,null]}")))


(flamegraph:save-flame-graph ("fu.stack")
  (fu 10000000))
