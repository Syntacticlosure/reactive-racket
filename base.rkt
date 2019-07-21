#lang racket
(require syntax/parse/define)
(provide (except-out (all-defined-out) after-schedule))
(define after-schedule (make-parameter #f))
(define-simple-macro (run-behavior (behaviors-id:id ...) expr ...)
  #:with calc #'(let ([behaviors-id (send behaviors-id value-now)] ...)
                  expr ...)
  (let ([ret (new behavior% [init-value calc])])
    (send behaviors-id add-listener (λ (v)
                                      (if (after-schedule)
                                          (after-schedule (cons (thunk 
                                                                 (send ret call calc))
                                                                (after-schedule))
                                                                 )
                                          (send ret call calc)))) ... 
    ret))
    
(define (hold ev init)
  (foldl-b (λ (v old) v) init ev))

(define (map-e f evs)
  (define ret (new event-stream%))
  (send evs add-listener
        (λ (v) (send ret call (f v))))
  ret)

(define (filter-e p evs)
  (define ret (new event-stream%))
  (send evs add-listener
        (λ (v) (when (p v)
                 (send ret call v))))
  ret)

(define (foldl-e op init evs)
  (define ret (new event-stream%))
  (define acc init)
  (send evs add-listener
        (λ (v) (set! acc (op v acc))
          (send ret call acc)))
  ret
  )

(define (foldl-b op init evs)
  (define ret (new behavior% [init-value init]))
  (send evs add-listener
        (λ (v) (define old (send ret value-now))
          (send ret call (op v old))))
  ret
  )

(define (merge-e . args)
  (define ret (new event-stream%))
  (for-each (λ (e)
              (send e add-listener (λ (v) (send ret call v))))
            args)
  ret)


(define event-stream%
  (class object%
    (super-new)
    (field [listeners '()])
    (define/public (add-listener lis)
      (set! listeners (cons lis listeners)))
    (define/public (call value)
      (define after-do '())
      (if (after-schedule)
          (for-each (λ (f) (f value)) listeners)
          (parameterize ([after-schedule '()])
            (for-each (λ (f) (f value)) listeners)
            (set! after-do (after-schedule))
            ))
      (for-each (λ (f) (f)) after-do)
      )
    ))

(define behavior%
  (class event-stream%
    (init-field init-value)
    (super-new)
    (define current-value init-value)
    (define/public (value-now)
      current-value)
    (define/override (add-listener lis)
      (super add-listener lis)
      (lis current-value))
    (define/override (call value)
      (super call value)
      (set! current-value value)
      )
    ))
