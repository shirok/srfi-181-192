;;
;; Included from 181.sld
;;

(use gauche.uvector)
(use gauche.vport)

(define-constant *buffer-size* 1024)

(define (make-custom-binary-input-port id read! get-position set-position close)
  (define (filler buf)
    (read! buf 0 (u8vector-length buf)))
  (define (seeker offset whence)
    (if (and (= offset 0) (eq? whence 'SEEK_CUR))
      (and get-position (get-position))
      (and set-position
           (ecase whence
             [(SEEK_SET) (set-position offset)]
             [(SEEK_CUR) (and get-position
                              (set-position (+ offset (get-position))))]
             [(SEEK_END) #f]            ; unsupportable
             ))))
  (make <buffered-input-port>
    :name id
    :fill filler
    :seek seeker
    :close close))

    

