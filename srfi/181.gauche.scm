;;
;; Included from 181.sld
;;

(define-constant *buffer-size* 1024)

(define (make-seeker get-position set-position)
  (^[offset whence]
    (if (and (= offset 0) (eq? whence 'SEEK_CUR))
      (and get-position (get-position))
      (and set-position
           (ecase whence
             [(SEEK_SET) (set-position offset)]
             [(SEEK_CUR) (and get-position
                              (set-position (+ offset (get-position))))]
             [(SEEK_END) #f]            ; unsupportable
             )))))

(define (make-custom-binary-input-port id read! get-position set-position close)
  (define (filler buf)
    (read! buf 0 (u8vector-length buf)))
  (make <buffered-input-port>
    :name id
    :fill filler
    :seek (make-seeker get-position set-position)
    :close close))

(define (make-custom-textual-input-port id read! get-position set-position close)
  ;; <buffered-input-port> uses u8vector for the buffer, so we have
  ;; to convert it.
  (define cbuf #f)                      ;vector, allocated on demand
  (define (filler buf)
    (unless cbuf
      (set! cbuf (make-vector (quotient (u8vector-length buf) 4))))
    (rlet1 n (read! cbuf 0 (vector-length cbuf))
      (unless (zero? n)
        (let* ([s (vector->string cbuf 0 n)]
               [size (string-size s)])
          (assume (<= size (u8vector-length buf)))
          (string->u8vector! buf 0 s)))))
  (make <buffered-input-port>
    :name id
    :fill filler
    :seek (make-seeker get-position set-position)
    :close close))
    
  
