# shasht

[![Build Status](https://travis-ci.com/yitzchak/shasht.svg?branch=master)](https://travis-ci.com/yitzchak/shasht)

Common Lisp JSON reading and writing for the Kzin.

## Reading


## Writing


## Compliance

Although concise the JSON specification is very vague on a number of points and
accurate compliance by implementations is often substandard. Without a test
suite compliance is difficult to test. The 
[JSONTestSuite](https://github.com/nst/JSONTestSuite) includes over 300 reading
tests including those left ambiguous by the specification. The test suite of
shasht includes all of these tests in addition to various write test. For a
comparision of the compliance of the Common Lisp implementations of JSON see
[Compliance Comparision](https://yitzchak.github.io/shasht/).

## Benchmarks

A simple benchmark can be done with `tests/bench.lisp`. For SBCL on a Ryzen 
4800H system the following results are typical.

```
                                JSON Read Times                                 
             0                     6.8727204e-6                     1.3745441e-5
             ˫--------------------------------+--------------------------------˧
     cl-json ███████████████████████████████████▋
    jonathan ████████▍
json-streams ███████████████████████████████████████████████████████████████████
       jsown ████████▉
      shasht ██████████████▎
     st-json ██████████████████████████████████▎
       yason ████████████████████████████████████████▌


                                JSON Write Times                                
             0                     6.0235893e-6                     1.2047179e-5
             ˫--------------------------------+--------------------------------˧
     cl-json ███████████████████████████████████████████████████████████████████
    jonathan █████████████████████████████████▊
json-streams ██████████████████████████████▎
       jsown ███████████████████████████████████████▏
      shasht ██████████▊
     st-json ███████▋
       yason ██████████████████████████████████▎


                             JSON Read/Write Times                              
             0                    1.11709105e-5                     2.2341821e-5
             ˫--------------------------------+--------------------------------˧
     cl-json ███████████████████████████████████████████████████████████████████
    jonathan ██████████████████████████████████▉
json-streams ███████████████████████████████████████████████████████████████▉
       jsown ████████████████████████████▋
      shasht ███████████████████████▍
     st-json ███████████████████████████▊
       yason ████████████████████████████████████████████████████████▌
```
