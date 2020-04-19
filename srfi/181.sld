
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
    (import (scheme base)
            (scheme read)
            (scheme write))
    (include "181.impl.scm"))))

;; Local variables:
;; mode: scheme
;; end:


  
