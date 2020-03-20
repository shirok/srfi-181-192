
(define-library (srfi 181)
  (export make-custom-binary-input-port
          make-custom-textual-input-port
          make-custom-binary-output-port
          make-custom-textual-output-port
          make-custom-binary-input/output-port
          make-custom-textual-input/output-port)
  (cond-expand
   (gauche
    (import (scheme base)
            (gauche base)
            (gauche uvector)
            (gauche vport))
    (include "181.gauche.scm"))
   (else   
    ;; redefine those procedures
    (export call-with-port
            input-port?
            output-port?
            textual-port?
            binary-port?
            port?
            close-port
            close-input-port
            close-output-port
            read
            read-char
            peek-char
            read-line
            char-ready?
            read-string
            read-u8
            peek-u8
            u8-ready?
            read-bytevector
            read-bytevector!
            write
            write-shared
            write-simple
            display
            write-string
            write-u8
            write-bytevetor
            flush-output-port
            )
   (import (except (scheme base)
                   call-with-port
                   input-port?
                   output-port?
                   textual-port?
                   binary-port?
                   port?
                   close-port
                   close-input-port
                   close-output-port
                   read-char
                   peek-char
                   read-line
                   char-ready?
                   read-string
                   read-u8
                   peek-u8
                   u8-ready?
                   read-bytevector
                   read-bytevector!
                   write-string
                   write-u8
                   write-bytevetor
                   flush-output-port)
           (prefix (only (scheme base)
                         call-with-port
                         input-port?
                         output-port?
                         textual-port?
                         binary-port?
                         port?
                         close-port
                         close-input-port
                         close-output-port
                         read-char
                         peek-char
                         read-line
                         char-ready?
                         read-string
                         read-u8
                         peek-u8
                         u8-ready?
                         read-bytevector
                         read-bytevector!
                         write-string
                         write-u8
                         write-bytevetor
                         flush-output-port) r7rs:)
           (prefix (scheme read) r7rs:)
           (prefix (scheme write) r7rs:))
   (include "181.impl.scm"))))


;; Local variables:
;; mode: scheme
;; end:


  
