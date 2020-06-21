(ql:quickload :shasht)

(sb-profile:profile
shasht:read-json shasht::read-json-number shasht::read-json-object shasht::read-json-array
shasht::read-json-string)

(dotimes (p 10000) (shasht:from-json "{\"key1\":\"value\",\"key2\":1,\"key3\":[\"Hello\",1.2]}"))

(sb-profile:report)
