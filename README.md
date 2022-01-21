# shasht

[![Build Status][ci-badge]][ci]

Common Lisp JSON reading and writing for the Kzinti.

## Reading

The primary interface to parsing and reading JSON is the `read-json` function.

```lisp
(read-json &optional input-stream-or-string (eof-error-p t) eof-value single-value-p)
```

The argument `input-stream-or-string` can be an stream, a string to read from,
or `nil` to use `*standard-input*`. The arguments `eof-error-p` and `eof-value`
have the same affect as they do in the Common Lisp function `read`. If the
`single-value-p` argument is true then the input to `read-json` is assumed to
be a single value, which means that extra tokens at the end will cause an
error to be generated.

There are a number of dynamic variables that will influence the parsing of JSON
data.

- `common-lisp:*read-default-float-format*` — Controls the floating-point format 
   that is to be used when reading a floating-point number.
- `*read-default-true-value*` — The default value to return when reading a true 
  token. Initially set to `t`.
- `*read-default-false-value*` — The default value to return when reading a 
  false token. Initially set to `nil`.
- `*read-default-null-value*` — The default value to return when reading a null
  token. Initially set to `:null`.
- `*read-default-array-format*` — The default format to use when reading an
  array. Current supported formats are `:vector` or `:list`. Initially set to
  `:vector`.
- `*read-default-object-format*` — The default format to use when reading an
  object. Current supported formats are `:hash-table`, `:alist` or `:plist`.
  Initially set to `:hash-table`.
  
There is also a keyword variant `read-json*` which will set the various dynamic
variables from supplied keywords.

```lisp
(read-json* :stream nil
            :eof-error t
            :eof-value nil 
            :single-value nil
            :true-value t 
            :false-value nil 
            :null-value :null
            :array-format :vector 
            :object-format :hash-table
            :float-format 'single-float)
```

## Writing

The primary interface to serializing and writing JSON is the `write-json` 
function.

```lisp
(write-json value &optional (output-stream t))
```

The `output-stream` argument can be a stream, `t` for `*standard-output*`, or
`nil` for output to a string. If the output is to a string then this string will
be returned, otherwise the original value will be returned.

There are a number of dynamic variables that will influence the serialization of
JSON data.

- `common-lisp:*print-pretty*` — If true then a simple indentation algorithm
  will be used.
- `*write-indent-string*` — The string to use when indenting objects and arrays.
   Initially set to `#\space`.
- `*write-ascii-encoding*` — If true then any non ASCII values will be encoded 
  using Unicode escape sequences. Initially set to `nil`.
- `*write-true-values*` — Values that will be written as a true token. Initially 
  set to `'(t :true)`.
- `*write-false-values*` — Values that will be written as a false token. 
  Initially set to `'(nil :false)`.
- `*write-null-values*` — Values that will be written as a null token. Initially 
  set to `(:null)`.
- `*write-alist-as-object*` — If true then assocation lists will be written as 
  an object. Initially set to `nil`. 
- `*write-plist-as-object*` — If true then property lists will be written as an 
  object. Initially set to `nil`.
- `*write-array-tags*` — A list of values whose appearance in the CAR of a list 
  indicates the CDR of the list should be written as an array. Initially set to 
  `'(:array)`.
- `*write-object-alist-tags*` — A list of values whose appearance in the CAR of 
  a list indicates the CDR of the list is an alist and should be written as an 
  object. Initially set to `'(:object-alist)`.
- `*write-object-plist-tags*` — A list of values whose appearance in the CAR of 
  a list indicates the CDR of the list is a plist and should be written as an 
  object. Initially set to `'(:object-plist)`.
  
The actual serialization of JSON data is done by the generic function
`print-json-value` which can be specialized for additional value types.

```lisp
(print-json-value value output-stream)
```

There is also a keyword variant `write-json*` which will set the various dynamic
variables from supplied keywords and will default to the current dynamic value 
of each keyword.

```lisp
(write-json* value :stream t 
                   :ascii-encoding nil 
                   :true-values '(t :true)
                   :false-values '(nil :false) 
                   :null-values '(:null)
                   :empty-array-values '(:empty-array)
                   :empty-object-values '(:empty-object)
                   :array-tags '(:array)
                   :object-alist-tags '(:object-alist) 
                   :object-plist-tags '(:object-plist) 
                   :alist-as-object nil 
                   :plist-as-object nil
                   :pretty nil 
                   :indent-string "  ")
```
### Serialization Helper Functions

In order to facilitate extending the serialization facilities of shasht there
are a number of helper functions available. To aid in the printing of JSON
strings there is the following.

```lisp
(write-json-string value output-stream)
```

In order to ease the serialization of objects and arrays there is 
`with-json-object` and `with-json-array`. Both of these macros take an
output stream as the first argument then enable indentation and automatic
handling of all delimiter tokens. Inside the body of `with-json-object`
the function `(print-json-key-value key value output-stream)` should be used
to output a key value pair. Inside the body of `with-json-array` the function
`(print-json-value value output-stream)` should be used to output a single
value. Example usage can be seen in the source code.

## Mapping of Data Types

The mapping of types between Common Lisp and JSON is not one-to-one nor is it
without ambiguity due to issues such as Common Lisp's treatment of `nil` as
an empty list and as a false value. JSON also includes `null` which does not
have an obvious representation in Common Lisp. Because of this, shasht makes
certain choices in the default mapping of data types between Common Lisp and
JSON that may not be ideal for all applications. shasht was primarily designed
to be a good round-trip encoder/decoder for [common-lisp-jupyter][] in the
network protocol of Jupyter.

A brief explanation of the default mapping is given in the table below. For 
more detail regarding the mapping of individual types and how to configure
that mapping see the sections following this table.

| Common Lisp                            |     | JSON                               |
|----------------------------------------|----:|------------------------------------|
| integer                                | <-> | number without decimal or exponent |
| float                                  | <-> | number with decimal or exponent    |
| ratio                                  |  -> | number with decimal or exponent    |
| rational                               |  -> | number with decimal or exponent    |
| string                                 | <-> | string                             |
| character                              |  -> | string                             |
| pathname                               |  -> | string                             |
| symbol not matching other mapping      |  -> | string                             |
| vector                                 | <-> | array                              |
| multi-dimensional array                |  -> | nested array                       |
| non-`nil` list                         |  -> | array                              |
| hash table                             | <-> | object                             |
| standard object                        |  -> | object                             |
| structure object                       |  -> | object                             |
| `t`                                    | <-> | `true`                             |
| `:true`                                |  -> | `true`                             |
| `nil`                                  | <-> | `false`                            |
| `:false`                               |  -> | `false`                            |
| `:null`                                | <-> | `null`                             |
| `:empty-array`                         |  -> | `[]`                               |
| `:empty-object`                        |  -> | `{}`                               |
| `'(:array 1 2 3)`                      |  -> | `[1,2,3]`                          |
| `'(:object-alist ("a" . 1) ("b" . 2))` |  -> | `{"a":1,"b":2}`                    |
| `'(:object-plist "a" 1 "b" 2)`         |  -> | `{"a":1,"b":2}`                    |

### Mapping of Number Types

The format of a number read from JSON when a decimal or an exponent is present
in the number literal can be influenced with `cl:*read-default-float-format*`.
This is the same behavior of `cl:read`. In order to read JSON numbers with large
exponents one would need do something like the following.

```common-lisp
(shasht:read-json "[2.232e75]" :float-format 'double-float)
```
### Mapping of Array Types

The dynamic variables `*read-default-array-format*`, 
`*write-empty-array-values*`, and `*write-array-tags*` all influence the mapping
of JSON arrays to Common Lisp vectors and lists. Common Lisp vectors and 
multi-dimensional arrays are always writen as JSON arrays. By default JSON
arrays are read as Common Lisp vectors. With the default settings only non-`nil`
lists that don't satisfy some other mapping rule are written as JSON arrays.

If one wants to use lists as the default JSON array format then 
`*read-default-false-value*`, `*read-default-array-format*`, and 
`*write-false-value*` will need to need to be set to appropriate values since
in the default mapping `nil` maps to `false`. For example, the following 
could be done.

```common-lisp
(let ((shasht:*read-default-false-value* :false)
      (shasht:*read-default-array-format* :list)
      (shasht:*write-false-values* '(:false)))
 (shasht:read-json ...)
 (shasht:write-json ...))
```

Lists with a CAR `eql` to a value in `*write-array-tags*`, 
`*write-object-alist-tags*`, `*write-object-plist-tags*` will still be written
as an array or object as appropriate. To completely disable this behavior the
variables would need to be bound to `nil`. Or one could do the following.

```common-lisp
(shasht:write-json '(1 2 3) :false-value '(:false) :array-tags nil
                            :object-alist-tags nil :object-plist-tags nil)
```

In this case the mapping for array types would become:

| Common Lisp                            |     | JSON                               |
|----------------------------------------|----:|------------------------------------|
| vector                                 |  -> | array                              |
| multi-dimensional array                |  -> | nested array                       |
| list                                   | <-> | array                              |

### Mapping of Object Types

The dynamic variables `*read-default-object-format*`, `*write-alist-as-object*`,
`*write-plist-as-object*`, `*write-empty-object-values*`, 
`*write-object-alist-tags*`, and `*write-object-plist-tags*` all influence the 
mapping of JSON objects to Common Lisp hash tables, alists, and plists. Common 
Lisp hash tables are always written as JSON objects. By default JSON objects are 
read as Common Lisp hash tables.

In order to use alists as the default JSON object format the dynamic variables
`*read-default-object-format*`, `*write-alist-as-object*`, 
`*read-default-false-value*`, and `*write-false-values*` will need to be set
to appropriate values. For example, the following would use alists as the
default JSON object format and `:false` as the JSON `false` value.

```common-lisp
(let ((shasht:*read-default-object-format* :alist)
      (shasht:*write-alist-as-object* t)
      (shasht:*read-default-false-value* :false)
      (shasht:*write-false-values* '(:false)))
 (shasht:read-json ...)
 (shasht:write-json ...))
```

In this case the mapping for object types would become:

| Common Lisp                            |     | JSON                               |
|----------------------------------------|----:|------------------------------------|
| hash table                             |  -> | object                             |
| alist                                  | <-> | object                             |
| standard object                        |  -> | object                             |
| structure object                       |  -> | object                             |

The same could be accomplished for plists by doing the following.

```common-lisp
(let ((shasht:*read-default-object-format* :plist)
      (shasht:*write-plist-as-object* t)
      (shasht:*read-default-false-value* :false)
      (shasht:*write-false-values* '(:false)))
 (shasht:read-json ...)
 (shasht:write-json ...))
```

## Compliance

Although concise the JSON specification is very vague on a number of points and
thus accurate compliance by implementations is often substandard. Without 
comprehensive tests compliance is difficult to ascertain. The 
[JSONTestSuite](https://github.com/nst/JSONTestSuite) includes over 300 reading
tests including those left ambiguous by the specification. The test suite of
shasht includes all of these tests in addition to various write tests. For a
comparision of the compliance of the Common Lisp implementations of JSON see
[Compliance Comparision](https://yitzchak.github.io/shasht/parsing.html).

## Benchmarks

A simple benchmark can be done with `tests/bench.lisp`. For SBCL the following 
results are typical.

```
                                JSON Read Times                                 
             0                     7.002169e-6                      1.4004338e-5
             ˫--------------------------------+--------------------------------˧
     cl-json ███████████████████████████████████▉
    jonathan ████████▊
json-streams ███████████████████████████████████████████████████████████████████
       jsown █████████▋
      shasht █████████████▏
     st-json █████████████████████████████████▊
       yason ████████████████████████████████████████▉


                                JSON Write Times                                
             0                    6.2018808e-6                     1.24037615e-5
             ˫--------------------------------+--------------------------------˧
     cl-json ███████████████████████████████████████████████████████████████████
    jonathan ████████████████████████████████████▍
json-streams ███████████████████████████████▎
       jsown ████████████████████████████████████████▉
      shasht ███████████▍
     st-json ███████▋
       yason ████████████████████████████████████▏


                             JSON Read/Write Times                              
             0                     1.1348141e-5                     2.2696282e-5
             ˫--------------------------------+--------------------------------˧
     cl-json ███████████████████████████████████████████████████████████████████
    jonathan █████████████████████████████████████▍
json-streams ██████████████████████████████████████████████████████████████▉
       jsown ██████████████████████████████▎
      shasht ██████████████████████▏
     st-json ███████████████████████████▋
       yason ███████████████████████████████████████████████████████████▉

```

<!--refs-->

[ci]: https://github.com/yitzchak/shasht/actions/
[ci-badge]: https://github.com/yitzchak/shasht/workflows/ci/badge.svg
[common-lisp-jupyter]: https://github.com/yitzchak/common-lisp-jupyter/

