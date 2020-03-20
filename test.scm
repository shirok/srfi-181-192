;; -*- coding:utf-8 -*-

(import (scheme base)
        (srfi 1)
        (srfi 13)
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
                (bytevector-copy! buf start data pos (+ pos size))
                (set! pos (+ pos size))
                size))
            (lambda () pos)             ; get-position
            (lambda (k) (set! pos k))   ; set-position
            (lambda () (set! closed #t))) ; close
           ))
    (test-assert (port? p))
    (test-assert (input-port? p))
    
    (test-eqv 0 (read-u8 p))
    (test-eqv 1 (read-u8 p))
    (test-eqv 2 (read-u8 p))

    (test-equal (bytevector-copy data 3)
                (read-bytevector 997 p))
    (test-equal (eof-object) (read-u8 p))
    (test-assert (begin (close-port p)
                        closed))
    ))

(let ((data (string-tabulate (lambda (i) 
                               (integer->char
                                (cond-expand
                                 (full-unicode (+ #x3000 i))
                                 (else (modulo i 256)))))
                             1000))
      (pos  0)
      (closed #f))
  (let ((p (make-custom-textual-input-port 
            "textual-input"
            (lambda (buf start count)   ; read!
              (let ((size (min count (- (string-length data) pos))))
                (unless (zero? size)
                  (if (string? buf)
                    (string-copy! buf start data pos size)
                    (do ((i 0 (+ i 1))
                         (j pos (+ j 1)))
                        ((= i size) (set! pos j))
                      (vector-set! buf (+ start i) (string-ref data j)))))
                size))
            (lambda () pos)             ; get-position
            (lambda (k) (set! pos k))   ; set-position
            (lambda () (set! closed #t))) ; close
           ))
    (test-assert (port? p))
    (test-assert (input-port? p))
    
    (test-eqv (string-ref data 0) (read-char p))
    (test-eqv (string-ref data 1) (read-char p))
    (test-eqv (string-ref data 2) (read-char p))

    (test-equal (string-copy data 3)
                (read-string 997 p))
    (test-equal (eof-object) (read-char p))

    (test-assert (begin (close-port p)
                        closed))
    ))

(test-end  "srfi-181-test")




