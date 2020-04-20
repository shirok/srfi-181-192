;; -*- coding:utf-8 -*-

(cond-expand
 (gauche (import (scheme base)
                 (srfi 1)
                 (srfi 13)
                 (srfi 64)))
 (chibi  (import (except (scheme base)
                         input-port? output-port? textual-port? binary-port?
                         port? close-port close-input-port close-output-port
                         read-char peek-char read-line char-ready?
                         read-string read-u8 peek-u8 u8-ready?
                         read-bytevector read-bytevector!
                         write-char write-string write-u8
                         write-bytevector flush-output-port)
                 (srfi 1)
                 (srfi 130)
                 (except (chibi test) test-equal)
                 (rename (chibi test) (test-equal chibi:test-equal))
                 (srfi 181 adapter)))
 )

(import (srfi 181))
(import (srfi 192))

(cond-expand
 (chibi 
  ;; adapt srfi-64 macros to (chibi test)
  (define-syntax test-equal
    (syntax-rules ()
      ((_ expect expr) (chibi:test-equal equal? expect expr))))
  (define-syntax test-eqv
    (syntax-rules ()
      ((_ expect expr) (chibi:test-equal eqv? expect expr)))))
 (else))

(test-begin "srfi-181-192-test")

;; Binary input, no port positioning
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
            #f                          ; get-position
            #f                          ; set-position
            (lambda () (set! closed #t))) ; close
           ))
    (test-assert (port? p))
    (test-assert (input-port? p))
    (test-assert (not (output-port? p)))
    (test-assert (not (port-has-port-position? p)))
    (test-assert (not (port-has-set-port-position!? p)))
    
    (test-eqv 0 (read-u8 p))
    (test-eqv 1 (read-u8 p))
    (test-eqv 2 (peek-u8 p))
    (test-eqv 2 (read-u8 p))

    (test-equal (bytevector-copy data 3)
                (read-bytevector 997 p))
    (test-equal (eof-object) (read-u8 p))
    
    (test-assert (begin (close-port p)
                        closed))
    ))

;; Binary input, port positioning
(let ((data (apply bytevector
                   (list-tabulate 1000 (lambda (i) (modulo i 256)))))
      (pos  0)
      (saved-pos #f)
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
    (test-assert (not (output-port? p)))
    (test-assert (port-has-port-position? p))
    (test-assert (port-has-set-port-position!? p))
    
    (test-eqv 0 (read-u8 p))
    (test-eqv 1 (read-u8 p))
    (test-eqv 2 (peek-u8 p))
    (set! saved-pos (port-position p))
    (test-eqv 2 (read-u8 p))

    (test-equal (bytevector-copy data 3)
                (read-bytevector 997 p))
    (test-equal (eof-object) (read-u8 p))
    
    (set-port-position! p saved-pos)
    (test-eqv 2 (read-u8 p))

    (test-assert (begin (close-port p)
                        closed))
    ))

;; Textual input, no port positioning
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
            #f                          ; get-position
            #f                          ; set-position
            (lambda () (set! closed #t))) ; close
           ))
    (test-assert (port? p))
    (test-assert (input-port? p))
    (test-assert (not (output-port? p)))
    (test-assert (not (port-has-port-position? p)))
    (test-assert (not (port-has-set-port-position!? p)))
    
    (test-eqv (string-ref data 0) (read-char p))
    (test-eqv (string-ref data 1) (read-char p))
    (test-eqv (string-ref data 2) (peek-char p))
    (test-eqv (string-ref data 2) (read-char p))

    (test-equal (string-copy data 3)
                (read-string 997 p))
    (test-equal (eof-object) (read-char p))

    (test-assert (begin (close-port p)
                        closed))
    ))

;; Textual input, port positioning
(let ((data (string-tabulate (lambda (i) 
                               (integer->char
                                (cond-expand
                                 (full-unicode (+ #x3000 i))
                                 (else (modulo i 256)))))
                             1000))
      (pos  0)
      (saved-pos #f)
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
    (test-assert (not (output-port? p)))
    (test-assert (port-has-port-position? p))
    (test-assert (port-has-set-port-position!? p))
    
    (test-eqv (string-ref data 0) (read-char p))
    (test-eqv (string-ref data 1) (read-char p))
    (test-eqv (string-ref data 2) (peek-char p))
    (set! saved-pos (port-position p))
    (test-eqv (string-ref data 2) (read-char p))
    (test-eqv (string-ref data 3) (peek-char p))

    (test-equal (string-copy data 3)
                (read-string 997 p))
    (test-equal (eof-object) (read-char p))

    (set-port-position! p saved-pos)
    (test-eqv (string-ref data 2) (peek-char p))
    
    (test-assert (begin (close-port p)
                        closed))
    ))

;; (let ((data (apply bytevector
;;                    (list-tabulate 1000 (lambda (i) (modulo i 256)))))
;;       (sink '())
;;       (pos 0)
;;       (closed #f))
;;   (let ((p (make-custom-binary-output-port
;;             "binary-output"
;;             (lambda (buf start count)   ;write!
;;               (do ([k 

(test-end  "srfi-181-192-test")
