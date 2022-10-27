#lang racket
(require racket/draw sugar/cache)
(provide (all-from-out racket/draw) make-ascii-pic ascii-table simply-make-ascii-pic factor binarize)

(define/contract ascii-table (parameter/c bytes?) (make-parameter #"abcdefghijklmnopqrstuvwxyz!@#$%^&*()-_ "))
(define/contract factor (parameter/c exact-positive-integer?) (make-parameter 2))

(define get-pixels
  (lambda (x y w h bitmap)
    (let ((color (make-bytes (* 4 w h))))
      (send bitmap get-argb-pixels x y w h color)
      color)))

(define/caching get-grey-value
  (lambda (bytes)
    (let ((ref (curry bytes-ref bytes)))
      (+ (* (ref 0) 0.3) (* (ref 1) 0.59) (* (ref 2) 0.11)))))

(define create-handler
  (lambda ()
    (let ((t (ascii-table)))
      (define gap (/ 256 (bytes-length t)))
      (make-caching-proc
       (lambda (bytes)
         (bytes-ref t (exact-floor (/ (get-grey-value bytes) gap))))))))

(define/contract make-ascii-pic
  (-> exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? (is-a?/c bitmap%) bytes?)
  (lambda (start-x start-y width height bitmap)
    (let ((proc (create-handler))
          (pixels (get-pixels start-x start-y width height bitmap))
          (factor (factor)))
      (let loop ((pixels pixels) (x 0) (byte-list null))
        (cond
          ((bytes=? pixels #"") (list->bytes (reverse byte-list)))
          ((= x width) (loop pixels 0 (cons 10 byte-list)))
          (else (loop (subbytes pixels 4) (add1 x) (append (make-list factor (proc (subbytes pixels 1 4))) byte-list))))))))

(define/contract simply-make-ascii-pic
  (-> (is-a?/c bitmap%) bytes?)
  (lambda (bitmap)
    (make-ascii-pic 0 0 (send bitmap get-width) (send bitmap get-height) bitmap)))

(define average-and-split (lambda (pixels)
                            (let loop ((pixels pixels) (amount 0) (counter 0) (grey-values null) (alpha null))
                              (cond ((bytes=? pixels #"") (values (/ amount counter) grey-values alpha))
                                    (else
                                     (let ((grey-value (get-grey-value (subbytes pixels 1 4))))
                                       (loop (subbytes pixels 4) (+ amount grey-value) (add1 counter)
                                             (cons grey-value grey-values) (cons (bytes-ref pixels 0) alpha))))))))

(define/contract binarize
  (-> (is-a?/c bitmap%) void?)
  (lambda (bitmap)
    (let ((width (send bitmap get-width))
          (height (send bitmap get-height)))
      (let ((pixels (get-pixels 0 0 width height bitmap)))
        (define-values (avg lis aph) (average-and-split pixels))
        (let loop ((lis lis) (aph aph) (res null))
          (cond ((null? lis) (send bitmap set-argb-pixels 0 0 width height (bytes-append* res)))
                (else (loop (cdr lis) (cdr aph) (cons (if (>= (car lis) avg)
                                                          (bytes (car aph) 255 255 255)
                                                          (bytes (car aph) 0 0 0))
                                                      res)))))))))