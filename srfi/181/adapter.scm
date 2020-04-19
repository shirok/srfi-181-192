;;;
;;; This module overrides R7RS basic port procedures to handle
;;; custom ports.
;;;
;;; Note: read is not included, for it will be a lots of work to
;;; implement its full functionality.  This is mostly for the
;;; demonstration purpose.
;;;
(define-library (srfi 181 adapter)
  (import (scheme base)
          (srfi 181 generic))
  (export (rename cp:input-port? input-port?)
          (rename cp:output-port? output-port?)
          (rename cp:textual-port? textual-port?)
          (rename cp:binary-port? binary-port/)
          (rename cp:port? port?)
          (rename cp:close-port close-port)
          (rename cp:close-input-port close-input-port)
          (rename cp:close-output-port close-output-port)
          ;; (rename cp:read read)
          (rename cp:read-char read-char)
          (rename cp:peek-char peek-char)
          (rename cp:read-line read-line)
          (rename cp:char-ready? char-ready?)
          (rename cp:read-string read-string)
          (rename cp:read-u8 read-u8)
          (rename cp:peek-u8 peek-u8)
          (rename cp:u8-ready? u8-ready?)
          (rename cp:read-bytevector read-bytevector)
          (rename cp:read-bytevector! read-bytevector!)
          ;; (rename cp:write write)
          ;; (rename cp:write-shared write-shared)
          ;; (rename cp:write-simple write-simple)
          ;; (rename cp:display display)
          (rename cp:write-char write-char)
          (rename cp:write-string write-string)
          (rename cp:write-u8 write-u8)
          (rename cp:write-bytevetor write-bytevector)
          (rename cp:flush-output-port flush-output-port)
          )
  )

;;; Primitive operations

(define (cp:input-port? obj)
  (or (custom-input-port? obj)
      (input-port? obj)))

(define (cp:output-port? obj)
  (or (custom-output-port? obj)
      (output-port? obj)))

(define (cp:textual-port? obj)
  (or (custom-port-textual? obj)
      (textual-port? obj)))

(define (cp:binary-port? obj)
  (or (custom-port-binary? obj)
      (binary-port? obj)))

(define (cp:port? obj)
  (or (custom-port? obj)
      (port? obj)))

(define (cp:close-port obj)
  (if (custom-port? obj)
    (custom-port-close obj)
    (close-port obj)))

(define (cp:close-input-port obj)
  (cond ((custom-input-port? obj) (custom-port-close obj))
        ((input-port? obj) (close-port obj))
        (else (error "input port expected, got" obj))))

(define (cp:close-output-port obj)
  (cond ((custom-output-port? obj) (custom-port-close obj))
        ((output-port? obj) (close-port obj))
        (else (error "output port expected, got" obj))))

(define (cp:flush-output-port obj)
  (cond ((custom-output-port? obj) (custom-port-flush obj))
        ((output-port? obj) (flush-output-port obj))
        (else (error "output port expected, got" obj))))

(define cp:read-char 
  (case-lambda
    (() (cp:read-char (current-input-port)))
    ((port) (cond ((custom-input-port? port) (custom-port-read-char port))
                  ((input-port? port) (read-char port))
                  (else (error "input port expected, got" port))))))

(define cp:peek-char 
  (case-lambda
    (() (cp:peek-char (current-input-port)))
    ((port) (cond ((custom-input-port? port) (custom-port-peek-char port))
                  ((input-port? port) (peek-char port))
                  (else (error "input port expected, got" port))))))

(define cp:read-u8
  (case-lambda
    (() (cp:read-u8 (current-input-port)))
    ((port) (cond ((custom-input-port? port) (custom-port-read-u8 port))
                  ((input-port? port) (read-u8 port))
                  (else (error "input port expected, got" port))))))

(define cp:peek-u8
  (case-lambda
    (() (cp:peek-u8 (current-input-port)))
    ((port) (cond ((custom-input-port? port) (custom-port-peek-u8 port))
                  ((input-port? port) (peek-u8 port))
                  (else (error "input port expected, got" port))))))

(define cp:write-char
  (case-lambda
    ((char) (cp:write-char char (current-output-port)))
    ((char port)
     (cond ((custom-output-port? port) (custom-port-write-char char port))
           ((output-port? port) (write-char char port))
           (else (error "output port expected, got" port))))))

(define cp:write-u8
  (case-lambda
    ((u8) (cp:write-u8 u8 (current-output-port)))
    ((u8 port)
     (cond ((custom-output-port? port) (custom-port-write-u8 u8 port))
           ((output-port? port) (write-u8 u8 port))
           (else (error "output port expected, got" port))))))

;; These two are debatable.
(define cp:char-ready?
  (case-lambda
    (() (cp:char-ready? (current-input-port)))
    ((port)
     (cond ((custom-input-port? port) #t)
           ((input-port? port) (char-ready? port))
           (else (error "input port expected, got" port))))))

(define cp:u8-ready?
  (case-lambda
    (() (cp:u8-ready? (current-input-port)))
    ((port)
     (cond ((custom-input-port? port) #t)
           ((input-port? port) (u8-ready? port))
           (else (error "input port expected, got" port))))))

;;; Derived operations

   
