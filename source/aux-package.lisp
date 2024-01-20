#|
Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1) Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

2) Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

(cl:defpackage #:pantalea.fancy
  (:use)
  (:export #:defpackage))

(cl:defmacro pantalea.fancy:defpackage (package cl:&rest options)
  (cl:let ((reexports cl:nil))
    (cl:dolist (option options)
      (cl:unless (cl:consp option)
        (cl:error "bogus FANCY-PACKAGE option: ~S" option))
      (cl:destructuring-bind (optname . optval)
          option
        (cl:case optname
          (:reexport
           (cl:push optval reexports)))))
    `(cl:eval-when (:compile-toplevel :load-toplevel :execute)
       (cl:defpackage ,package
         (:use)
         ,@(cl:remove :reexport options :key #'cl:car)
         ,@(cl:loop for rexport-spec in reexports
                 append
                 (cl:destructuring-bind (rpackage . rsymbols)
                     rexport-spec
                   `((:import-from ,rpackage ,@rsymbols)
                     (:export ,@rsymbols))))))))

(pantalea.fancy:defpackage pantalea.aux-package
  (:reexport #:alexandria
             #:xor
             #:if-let
             #:when-let
             #:when-let*
             #:cswitch
             #:eswitch
             #:switch
             #:define-constant
             #:alist-hash-table
             #:copy-hash-table
             #:hash-table-alist
             #:hash-table-keys
             #:hash-table-plist
             #:hash-table-values
             #:maphash-keys
             #:maphash-values
             #:plist-hash-table
             #:compose
             #:curry
             #:ensure-function
             #:ensure-functionf
             #:multiple-value-compose
             #:rcurry
             #:alist-plist
             #:appendf
             #:nconcf
             #:reversef
             #:nreversef
             #:circular-list
             #:circular-list-p
             #:circular-tree-p
             #:doplist
             #:ensure-car
             #:ensure-cons
             #:ensure-list
             #:flatten
             #:lastcar
             #:make-circular-list
             #:map-product
             #:mappend
             #:nunionf
             #:plist-alist
             #:proper-list
             #:proper-list-length
             #:proper-list-p
             #:remove-from-plist
             #:remove-from-plistf
             #:delete-from-plist
             #:delete-from-plistf
             #:set-equal
             #:setp
             #:unionf
             #:clamp
             #:iota
             #:lerp
             #:map-iota
             #:maxf
             #:mean
             #:median
             #:minf
             #:subfactorial
             #:array-index
             #:array-length
             #:copy-array
             #:copy-sequence
             #:deletef
             #:emptyp
             #:ends-with
             #:ends-with-subseq
             #:extremum
             #:first-elt
             #:last-elt
             #:length=
             #:map-combinations
             #:map-derangements
             #:map-permutations
             #:proper-sequence
             #:random-elt
             #:removef
             #:rotate
             #:sequence-of-length-p
             #:shuffle
             #:starts-with
             #:starts-with-subseq
             #:once-only
             #:parse-body
             #:parse-ordinary-lambda-list
             #:with-gensyms
             #:with-unique-names
             #:ensure-symbol
             #:format-symbol
             #:make-gensym
             #:make-gensym-list
             #:make-keyword
             #:string-designator
             #:negative-double-float
             #:negative-fixnum-p
             #:negative-float
             #:negative-float-p
             #:negative-long-float
             #:negative-long-float-p
             #:negative-rational
             #:negative-rational-p
             #:negative-real
             #:negative-single-float-p
             #:non-negative-double-float
             #:non-negative-double-float-p
             #:non-negative-fixnum
             #:non-negative-fixnum-p
             #:non-negative-float
             #:non-negative-float-p
             #:non-negative-integer-p
             #:non-negative-long-float
             #:non-negative-rational
             #:non-negative-real-p
             #:non-negative-short-float-p
             #:non-negative-single-float
             #:non-negative-single-float-p
             #:non-positive-double-float
             #:non-positive-double-float-p
             #:non-positive-fixnum
             #:non-positive-fixnum-p
             #:non-positive-float
             #:non-positive-float-p
             #:non-positive-integer
             #:non-positive-rational
             #:non-positive-real
             #:non-positive-real-p
             #:non-positive-short-float
             #:non-positive-short-float-p
             #:non-positive-single-float-p
             #:positive-double-float
             #:positive-double-float-p
             #:positive-fixnum
             #:positive-fixnum-p
             #:positive-float
             #:positive-float-p
             #:positive-integer
             #:positive-rational
             #:positive-real
             #:positive-real-p
             #:positive-short-float
             #:positive-short-float-p
             #:positive-single-float
             #:positive-single-float-p
             #:coercef
             #:negative-double-float-p
             #:negative-fixnum
             #:negative-integer
             #:negative-integer-p
             #:negative-real-p
             #:negative-short-float
             #:negative-short-float-p
             #:negative-single-float
             #:non-negative-integer
             #:non-negative-long-float-p
             #:non-negative-rational-p
             #:non-negative-real
             #:non-negative-short-float
             #:non-positive-integer-p
             #:non-positive-long-float
             #:non-positive-long-float-p
             #:non-positive-rational-p
             #:non-positive-single-float
             #:of-type
             #:positive-integer-p
             #:positive-long-float
             #:positive-long-float-p
             #:positive-rational-p
             #:type=
             #:required-argument
             #:ignore-some-conditions
             #:simple-style-warning
             #:simple-reader-error
             #:simple-parse-error
             #:simple-program-error
             #:unwind-protect-case
             #:featurep
             #:with-input-from-file
             #:with-output-to-file
             #:read-stream-content-into-string
             #:read-file-into-string
             #:write-string-into-file
             #:read-stream-content-into-byte-vector
             #:read-file-into-byte-vector
             #:write-byte-vector-into-file
             #:copy-stream
             #:copy-file
             #:symbolicate
             #:assoc-value
             #:rassoc-value
             #:destructuring-case
             #:destructuring-ccase
             #:destructuring-ecase)
  (:reexport #:serapeum
             #:defalias
             #:reshuffle
             #:~>
             #:->
             #:~>>
             #:batches
             #:supertypep
             #:tuple
             #:runs
             #:lret
             #:lret*
             #:no
             #:nor
             #:nand
             #:econd
             #:ordering
             #:nlet
             #:flip
             #:fork
             #:fork2
             #:hook
             #:hook2
             #:juxt
             #:dict
             #:dict*
             #:box
             #:unbox
             #:vect
             #:parse-number
             #:parse-float
             #:ensure
             #:random-in-range
             #:finc
             #:make
             #:vector=
             #:fbind
             #:take
             #:drop
             #:take-while
             #:drop-while
             #:extrema
             #:eval-always
             #:plist-keys
             #:plist-values
             #:def
             #:round-to
             #:assure
             #:trim-whitespace)
  (:reexport #:metabang.bind
             #:bind)
  (:reexport #:iterate
             #:iterate
             #:iter
             #:display-iterate-clauses
             #:defsynonym
             #:dsetq
             #:declare-variables
             #:defmacro-clause
             #:defmacro-driver
             #:defclause-sequence
             #:initially
             #:after-each
             #:finally
             #:finally-protected
             #:else
             #:if-first-time
             #:first-iteration-p
             #:first-time-p
             #:finish
             #:leave
             #:next-iteration
             #:next
             #:terminate
             #:repeat
             #:for
             #:counting
             #:as
             #:generate
             #:generating
             #:in
             #:sum
             #:summing
             #:multiply
             #:multiplying
             #:maximize
             #:minimize
             #:maximizing
             #:minimizing
             #:always
             #:never
             #:thereis
             #:finding
             #:collect
             #:collecting
             #:with
             #:while
             #:until
             #:adjoining
             #:nconcing
             #:appending
             #:nunioning
             #:unioning
             #:reducing
             #:accumulate
             #:accumulating))
