#lang racket
(require racket/gui
         syntax/parse/define
         (for-syntax racket/syntax)
         "base.rkt")

(provide rbutton% rtext-field%)

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

                          
    