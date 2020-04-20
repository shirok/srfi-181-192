;;;
;;; Primitive operations
;;;

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

;;;
;;; Derived operations
;;;

