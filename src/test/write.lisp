(in-package :shasht/test)


(defmacro define-write-test (name value expected &key pretty ascii alist plist false)
  `(define-test ,name
     (finish
       (is equal
         ,expected
         (let ((*print-pretty* ,pretty)
               (shasht:*write-alist-as-object* ,alist)
               (shasht:*write-plist-as-object* ,plist)
               (shasht:*write-ascii-encoding* ,ascii)
               (shasht:*write-false-values* ,(or false '(quote (nil :false)))))
           (shasht:to-json ,value))))))


(define-write-test "Write :null as null"
  :null
  "null")


(define-write-test "Write :true as true"
  :true
  "true")


(define-write-test "Write t as true"
  t
  "true")


(define-write-test "Write nil as false"
  nil
  "false")


(define-write-test "Write :false as false"
  :false
  "false")


(define-write-test "Write nil as empty array"
  nil
  "[]"
  :false '(:false))


(define-write-test "Write integer"
  743
  "743")


(define-write-test "Write string"
  "fu bar"
  "\"fu bar\"")


(define-write-test "Write NUL"
  (string (code-char #x0))
  "\"\\\u0000\"")


(define-write-test "Write SOH"
  (string (code-char #x1))
  "\"\\\u0001\"")


(define-write-test "Write STX"
  (string (code-char #x2))
  "\"\\\u0002\"")


(define-write-test "Write ETX"
  (string (code-char #x3))
  "\"\\\u0003\"")


(define-write-test "Write EOT"
  (string (code-char #x4))
  "\"\\\u0004\"")


(define-write-test "Write ENQ"
  (string (code-char #x5))
  "\"\\\u0005\"")


(define-write-test "Write ACK"
  (string (code-char #x6))
  "\"\\\u0006\"")


(define-write-test "Write BEL"
  (string (code-char #x7))
  "\"\\\u0007\"")


(define-write-test "Write BS"
  (string (code-char #x8))
  "\"\\\b\"")


(define-write-test "Write HT"
  (string (code-char #x9))
  "\"\\\t\"")


(define-write-test "Write LF"
  (string (code-char #xa))
  "\"\\\n\"")


(define-write-test "Write VT"
  (string (code-char #xb))
  "\"\\\u000B\"")


(define-write-test "Write FF"
  (string (code-char #xc))
  "\"\\\f\"")


(define-write-test "Write CR"
  (string (code-char #xd))
  "\"\\\r\"")


(define-write-test "Write SO"
  (string (code-char #xe))
  "\"\\\u000E\"")


(define-write-test "Write SI"
  (string (code-char #xf))
  "\"\\\u000F\"")


(define-write-test "Write DLE"
  (string (code-char #x10))
  "\"\\\u0010\"")


(define-write-test "Write DC1"
  (string (code-char #x11))
  "\"\\\u0011\"")


(define-write-test "Write DC2"
  (string (code-char #x12))
  "\"\\\u0012\"")


(define-write-test "Write DC3"
  (string (code-char #x13))
  "\"\\\u0013\"")


(define-write-test "Write DC4"
  (string (code-char #x14))
  "\"\\\u0014\"")


(define-write-test "Write NAK"
  (string (code-char #x15))
  "\"\\\u0015\"")


(define-write-test "Write SYN"
  (string (code-char #x16))
  "\"\\\u0016\"")


(define-write-test "Write ETB"
  (string (code-char #x17))
  "\"\\\u0017\"")


(define-write-test "Write CAN"
  (string (code-char #x18))
  "\"\\\u0018\"")

(define-write-test "Write EM"
  (string (code-char #x19))
  "\"\\\u0019\"")


(define-write-test "Write SUB"
  (string (code-char #x1a))
  "\"\\\u001A\"")


(define-write-test "Write ESC"
  (string (code-char #x1b))
  "\"\\\u001B\"")


(define-write-test "Write FS"
  (string (code-char #x1c))
  "\"\\\u001C\"")


(define-write-test "Write GS"
  (string (code-char #x1d))
  "\"\\\u001D\"")


(define-write-test "Write RS"
  (string (code-char #x1e))
  "\"\\\u001E\"")


(define-write-test "Write US"
  (string (code-char #x1f))
  "\"\\\u001F\"")


(define-write-test "Write quote"
  (string #\")
  "\"\\\"\"")


(define-write-test "Write backslash"
  (string #\\)
  "\"\\\\\"")


(define-write-test "Write simple numerical list"
  '(1 2 3)
  "[1,2,3]")


(define-write-test "Write simple numerical vector"
  #(1 2 3)
  "[1,2,3]")


(define-write-test "Write complex numerical vector"
  #(1 #(2 3) 4 #(6 7 8) #(9 10 #(11 12 13)))
  "[1,[2,3],4,[6,7,8],[9,10,[11,12,13]]]")


(define-write-test "Write complex numerical list"
  '(1 (2 3) 4 (6 7 8) (9 10 (11 12 13)))
  "[1,[2,3],4,[6,7,8],[9,10,[11,12,13]]]")


(define-write-test "Write simple string vector"
  #("quux" "wibble" "gronk")
  "[\"quux\",\"wibble\",\"gronk\"]")


(define-write-test "Write simple string list"
  '("quux" "wibble" "gronk")
  "[\"quux\",\"wibble\",\"gronk\"]")


(define-write-test "Write simple symbol vector"
  #(:quux :wibble :gronk)
  "[\"QUUX\",\"WIBBLE\",\"GRONK\"]")


(define-write-test "Write simple symbol list"
  '(:quux :wibble :gronk)
  "[\"QUUX\",\"WIBBLE\",\"GRONK\"]")


(define-write-test "Write simple hash table"
  (alexandria:alist-hash-table '(("fu" . 1) ("bar" . 2) ("wibble" . 3)))
  "{\"fu\":1,\"bar\":2,\"wibble\":3}")


(define-write-test "Write simple alist"
  '(("fu" . 1) ("bar" . 2) ("wibble" . 3))
  "{\"fu\":1,\"bar\":2,\"wibble\":3}"
  :alist t)


(define-write-test "Write simple plist"
  '(:fu 1 :bar 2 :wibble 3)
  "{\"FU\":1,\"BAR\":2,\"WIBBLE\":3}"
  :plist t)


(define-write-test "Write plist like list"
  '(:fu 1 :bar 2 :wibble 3)
  "[\"FU\",1,\"BAR\",2,\"WIBBLE\",3]")

