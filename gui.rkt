#lang racket
(require racket/gui
         syntax/parse/define
         (for-syntax racket/syntax)
         "base.rkt")

(provide rbutton% rtext-field% rcanvas%)

(define rbutton%
  (class button%
    (init-field [callback void])
    (define clicks (new event-stream%))
    (super-new [callback (λ (c e) (send clicks call e)
                           (callback c e))])
    (define/public (get-clicks)
      clicks)))

(define rtext-field%
  (class text-field%
    (init-field [callback void])
    (define vals (new event-stream%))
    (super-new [callback (λ (c e)
                           (send vals call (send this get-value))
                           (callback c e))])
    (define/public (get-values)
      vals)))
(define (interval t)
  (define ret (new event-stream%))
  (new timer% [notify-callback (thunk (send ret call (current-milliseconds)))]
       [interval t])
  ret)
(define rcanvas%
  (class canvas%
    (super-new)
    (define mouse-pos (new behavior% [init-value (cons 0 0)]))
    (define-simple-macro (define-public-es name)
      #:with get-name (format-id #'define-public-es "get-~a" #'name)
      (begin (define name (new event-stream%))
             (define/public (get-name) name)))
    (define-public-es key-strokes)
    (define-public-es left-clicks)
    (define-public-es right-clicks)
    (define-public-es left-releases)
    (define-public-es right-releases)
    (define/override (on-event me)
      (super on-event me)
      (match (send me get-event-type)
        ['left-down (send left-clicks call me)]
        ['left-up (send left-releases call me)]
        ['right-down (send right-clicks call me)]
        ['right-up (send right-releases call me)]
        [_ (void)]
        )
      )
    (define/override (on-char ke)
      (super on-char ke)
      (send key-strokes call (send ke get-key-code)))
    ))


                          
