(import (scheme base)
        (srfi 1)
        (srfi 64)
        (srfi 181))

(test-begin "srfi-181-test")

(let ((data (apply bytevector
                   (list-tabulate 1000 (lambda (i) (modulo i 256)))))
      (pos  0)
      (closed #f))
  (let ((p (make-custom-binary-input-port 
            "binary-input"
            (lambda (buf start count)   ; read!
              (let ((size (min count (- (bytevector-length data) pos))))
                (bytevector-copy! buf start data pos size)
                size))
            (lambda () pos)             ; get-position
            (lambda (k) (set! pos k))   ; set-position
            (lambda () (set! closed #t))) ; close
           ))
    (test-assert (port? p))
    (test-assert (input-port? p))
    
    (test-eqv (read-u8 p) 0)
    (test-eqv (read-u8 p) 1)
    (test-eqv (read-u8 p) 2)

    (test-equal (read-bytevector 997 p)
                (bytevector-copy data 3))
    (test-assert (begin (close-port p)
                        closed))
    ))

(test-end  "srfi-181-test")




