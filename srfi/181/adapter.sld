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
          (scheme case-lambda)
          (scheme write)
          (srfi 181 generic))
  (export (rename cp:input-port? input-port?)
          (rename cp:output-port? output-port?)
          (rename cp:textual-port? textual-port?)
          (rename cp:binary-port? binary-port?)
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
          (rename cp:write write)
          (rename cp:write-shared write-shared)
          (rename cp:write-simple write-simple)
          (rename cp:display display)
          (rename cp:write-char write-char)
          (rename cp:write-string write-string)
          (rename cp:write-u8 write-u8)
          (rename cp:write-bytevector write-bytevector)
          (rename cp:flush-output-port flush-output-port)
          )
  (include "adapter.scm")
  )
