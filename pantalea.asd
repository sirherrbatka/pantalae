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
(asdf:defsystem pantalea
  :name "pantalea"
  :license "BSD simplified"
  :description "P2P network layer for Common Lisp"
  :serial T
  :pathname "source"
  :depends-on ( #:iterate  #:metabang-bind
                #:serapeum #:bordeaux-threads
                #:usocket  #:alexandria
                #:ironclad #:cl-conspack
                #:nibbles  #:local-time
                #:log4cl   #:cl-custom-hash-table
                #:mockery)
  :components ((:file "aux-package")
               (:module "utils"
                :components ((:file "packages")
                             (:file "hashing")
                             (:file "bloom-filter")
                             (:file "skip-list")
                             (:file "queue")
                             (:file "hyperloglog")
                             (:file "conditions")
                             (:file "dep")
                             (:file "promise")
                             (:file "timing-wheel")))
               (:module "cryptography"
                :components ((:file "package")
                             (:file "generics")
                             (:file "utils")
                             (:file "types")
                             (:file "functions")
                             (:file "methods")))
               (:module "transport"
                :components ((:file "packages")
                             (:file "protocol-macros")
                             (:file "protocol-conditions")
                             (:file "protocol-variables")
                             (:file "protocol-generics")
                             (:file "protocol-types")
                             (:file "protocol-utils")
                             (:file "protocol-serialization")
                             (:file "protocol-methods")
                             (:file "protocol-functions")
                             (:file "impl-intra")
                             (:file "impl-tcp")))))
