#lang racket
(require racket/draw)
(provide (all-from-out racket/draw) make-ascii-pic ascii-table)

(define/contract ascii-table (parameter/c bytes?) (make-parameter #"abcdefghijklmnopqrstuvwxyz!@#$%^&*()-_ "))

(define get-pixel
  (lambda (x y w h bitmap)
    (let ((color (make-bytes)))
      (send bitmap get-argb-pixels x y w h color)
      ;;ignore the alpha value
      (subbytes color 1))))

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
  (-> (is-a? bitmap%) bytes?)
  (lambda (bitmap)
    (let ((w (send bitmap get-width))
          (h (send bitmap get-height))
          (p (create-handler)))
      (let loop ((x 0) (y 0) (byte-list null))
        (cond
          ((> y h) (list->bytes (reverse byte-list)))
          ((> x w) (loop 0 (add1 y) (cons 10 byte-list)))
          (else (loop (add1 x) y (cons (p (get-pixel x y w h bitmap)) byte-list))))))))