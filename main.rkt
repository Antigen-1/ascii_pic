#lang racket
(require racket/draw)
(provide (all-from-out racket/draw) make-ascii-pic ascii-table simply-make-ascii-pic)

(define/contract ascii-table (parameter/c bytes?) (make-parameter #"abcdefghijklmnopqrstuvwxyz!@#$%^&*()-_ "))

(define get-pixels
  (lambda (x y w h bitmap)
    (let ((color (make-bytes (* 4 w h))))
      (send bitmap get-argb-pixels x y w h color)
      color)))

(define get-grey-value
  (lambda (bytes)
    (let ((l (bytes->list bytes)))
      (+ (* (car l) 0.3) (* (cadr l) 0.59) (* (caddr l) 0.11)))))

(define create-handler
  (lambda ()
    (let ((t (ascii-table)))
      (define gap (/ 256 (bytes-length t)))
      (lambda (bytes)
        (bytes-ref t (exact-floor (/ (get-grey-value bytes) gap)))))))

(define/contract make-ascii-pic
  (-> exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? (is-a?/c bitmap%) bytes?)
  (lambda (start-x start-y width height bitmap)
    (let ((proc (create-handler))
          (pixels (get-pixels start-x start-y width height bitmap)))
      (let loop ((pixels pixels) (x 0) (byte-list null))
        (cond
          ((bytes=? pixels #"") (list->bytes (reverse byte-list)))
          ((> x width) (loop pixels 0 (cons 10 byte-list)))
          (else (loop (subbytes pixels 4) (add1 x) (cons (proc (subbytes pixels 1 4)) byte-list))))))))

(define/contract simply-make-ascii-pic
  (-> (is-a?/c bitmap%) bytes?)
  (lambda (bitmap)
    (make-ascii-pic 0 0 (send bitmap get-width) (send bitmap get-height) bitmap)))