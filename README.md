# recursive-regex

This library integrates with CL-PPCRE to make named regex groups 
dispatch to custom matcher functions, which can recursively call
another regex in the context of the original regex.  It can then
return a (parse) tree of results.

## Some advantages of this approach

 * Modularize common regexp patterns so more reuse can be had

 * Allow non-regular expression matches by defering to custom 
   match functions

 * Named groups matched in * expressions return all their results
   rather than the last match only (full parse tree of results, 
   based on the named dispatchers)

## Some disadvantages of this approach

 * Currently doesnt support a streaming interface

## Minor Example:
```
 (regex-recursive-groups #?r"function\s*(?<parens>)"
     "looking for function(call(), 1 ,2) type things")
 >> #<RESULT-NODE ROOT s:12 e:34 kids:1 "function(call(), 1 ,2)"  {1003341731}>
    (:ROOT "function(call(), 1 ,2)"
      (:MATCHED-PARENS "(call(), 1 ,2)"))
```
NB: Currently returns clos object trees and lisp-trees, because I
want to play with it in the repl and see results but in 
practicality the CLOS result is probably what I want to work 
with.


## Pre-Defined Dispatchers

  body: 
    can be used in a definition to designate where the body 
    matcher should start matching. You can also provide a default
    body by specifying a regex inside the body in a definition.

  parens, brackets, braces, angels:
    these all match a single matched pair 
    eg, for parens:
      not in the match (in (the) match) not in the match

  single-quotes, double-quotes:
    single-quotes and double-quotes respect a single backslash as escape

  comma-list: 
    [\t ]*(?:(?<body>[^,]*)[\t ]*,)*[\t ]*(?<body>[^,]*)[\t ]*
    the comma-list definition is (?<body>) separated by commas with
    arbitrary whitespace surrounding them. The default value of the
    body regex os [^,]*

  csv-row:
    (?<comma-list>((?<double-quotes>)|[^\n,]*))(?:\n|$)
    calls the comma-list matcher passing in the body
    double-quotes-matcher or not new line or comma repeating

```
;; Copyright (c) 2011 Russ Tyndall, 
;;   Acceleration.net http://www.acceleration.net
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```