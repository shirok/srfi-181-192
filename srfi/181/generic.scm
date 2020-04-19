;;;
;;; Generic implementation of srfi-181 and srfi-192
;;;

(define-library (srfi 181 generic)
  (import (scheme base)
          (srfi 145))                   ; assume
  (export custom-port?
          custom-input-port?
          custom-output-port?
          custom-port-binary?
          custom-port-textual?
          custom-port-has-port-position?
          custom-port-position
          custom-port-has-set-port-position!?
          custom-port-set-port-position!
          custom-port-read-u8
          custom-port-peek-u8
          custom-port-read-char
          custom-port-peek-char
          custom-port-write-u8
          custom-port-write-char
          custom-port-close
          custom-port-flush

          make-custom-binary-input-port
          make-custom-textual-input-port
          make-custom-binary-output-port
          make-custom-textual-output-port
          make-custom-binary-input/output-port
          make-custom-textual-input/output-port)
  (begin


    (define-record-type custom-port
      %make-custom-port  ; constructor
      custom-port?       ; predicate

      id                 ; port name
      binary?            ; boolean flag
      (prefetch)         ; prefeched byte / char
      (prefetch-pos)     ; save the current pos before peek-char
                                        ;  can be (pos) or #f
      buf                ; one-element bytevector or vector
      read!
      write!
      get-position
      set-position!
      closer
      flusher)

    (define (boolean x) (not (not x)))

    (define (custom-port-has-port-position? port)
      (assume (custom-port? port))
      (boolean (custom-port-get-position port)))

    (define (custom-port-position port)
      (assume (and (custom-port? port)
                   (custom-port-get-position port)))
      (if (custom-port-binary? port)
        (if (custom-port-prefetch port)
          (- ((custom-port-get-position port)) 1)
          ((custom-port-get-position port)))
        (or (custom-port-prefetch-pos port)
            ((custon-port-get-position port)))))

    (define (custom-port-has-set-port-position!? port)
      (assume (custom-port? port))
      (boolean (custom-port-set-position! port)))

    (define (custon-port-set-port-position! port pos)
      (assume (and (custom-port? port)
                   (custom-port-set-position! port)))
      (custom-port-prefetch-set! port #f)
      (custom-port-prefetch-pos-set! port #f)
      ((custom-port-set-position! port) pos))

    (define (custom-input-port? port)
      (boolean (custom-port-read! port)))

    (define (custom-output-port? port)
      (boolean (custom-port-write! port)))

    (define (custom-port-textual? port)
      (not (custom-port-binary? port)))

    (define (custom-port-read-u8 port)
      (assume (and (custom-input-port? port)
                   (custom-port-binary? port)))
      (let ((pre (custom-port-prefetch port)))
        (if pre
          (begin (custom-port-prefetch-set! port #f)
                 pre)
          (let ((buf (custom-port-buf port)))
            ((custom-port-read! port) buf 0 1)
            (bytevector-ref buf 0)))))

    (define (custom-port-peek-u8 port)
      (or (custom-port-prefetch port)
          (let ((buf (custom-port-buf port)))
            ((custom-port-read! port) buf 0 1)
            (custom-port-prefetch-set! (bytevector-ref buf 0))
            (bytevector-ref buf 0))))

    (define (custom-port-read-char port)
      (assume (and (custom-input-port? port)
                   (custom-port-textual? port)))
      (let ((pre (custom-port-prefetch port)))
        (if pre
          (begin (custom-port-prefetch-set! port #f)
                 pre)
          (let ((buf (custom-port-buf port)))
            ((custom-port-read! port) buf 0 1)
            (vector-ref buf 0)))))

    (define (custom-port-peek-char port)
      (or (custom-port-prefetch port)
          (let ((buf (custom-port-buf port)))
            ((custom-port-read! port) buf 0 1)
            (custom-port-prefetch-set! (vector-ref buf 0))
            (vector-ref buf 0))))

    (define (custom-port-write-u8 port byte)
      (let ((buf (custom-port-buf port)))
        (bytevector-set! buf 0 byte)
        ((custom-port-write! port) buf 0 1)))

    (define (custom-port-write-char port ch)
      (let ((buf (custom-port-buf port)))
        (vector-set! buf 0 byte)
        ((custom-port-write! port) buf 0 1)))

    (define (custom-port-close port)
      (let ((closer (custom-port-closer port)))
        (and closer (closer))))

    (define (custom-port-flush port)
      (let ((flusher (custom-port-flusher port)))
        (and flusher (flusher))))

    (define (make-custom-binary-input-port id read!
                                           get-position set-position! 
                                           close)
      (%make-custom-port id #t #f #f (make-bytevector 1)
                         read! #f
                         get-position
                         set-position!
                         close
                         #f))

    (define (make-custom-textual-input-port id read!
                                            get-position set-position! 
                                            close)
      (%make-custom-port id #f #f #f (make-vector 1)
                         read! #f
                         get-position 
                         set-position!
                         close
                         #f))

    (define (make-custom-binary-output-port id write!
                                           get-position set-position! 
                                           close . opts)
      (let ((flush (if (pair? opts) (car opts) #f)))
        (%make-custom-port id #t #f #f (make-bytevector 1)
                           #f write!
                           get-position
                           set-position!
                           close
                           flush)))

    (define (make-custom-textual-output-port id write!
                                             get-position set-position! 
                                             close . opts)
      (let ((flush (if (pair? opts) (car opts) #f)))
        (%make-custom-port id #f #f #f (make-bytevector 1)
                           #f write!
                           get-position
                           set-position!
                           close
                           flush)))

    (define (make-custom-binary-input/output-port id read! write!
                                                  get-position set-position! 
                                                  close . opts)
      (let ((flush (if (pair? opts) (car opts) #f)))
        (%make-custom-port id #t #f #f (make-bytevector 1)
                           read! write!
                           get-position
                           set-position!
                           close
                           flush)))

    (define (make-custom-textual-input/output-port id read! write!
                                                   get-position set-position!
                                                   close . opts)
      (let ((flush (if (pair? opts) (car opts) #f)))
        (%make-custom-port id #f #f #f (make-bytevector 1)
                           read! write!
                           get-position
                           set-position!
                           close
                           flush)))

    )) ;; define-library
