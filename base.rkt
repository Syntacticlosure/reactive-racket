#lang racket
(require syntax/parse/define)
(provide (except-out (all-defined-out) this-level-computations))


(define this-level-computations (make-parameter '()))
(define next-level-computations (make-parameter '()))


(define-simple-macro (snapshot (behaviors-id:id ...) expr ...)
  (let ([behaviors-id (send behaviors-id value-now)] ...)
    expr ...))


(define-simple-macro (run-behavior (behaviors-id:id ...) expr ...)
  #:with calc #'(let ([behaviors-id (send behaviors-id value-now)] ...)
                  expr ...)
  (let* ([ret (new behavior% [init-value calc])]
         [calc-thunk (thunk (send ret call calc))])
    (send behaviors-id add-listener (λ (v)(send ret call calc))) ... 
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
      (parameterize ([next-level-computations (append
                                               (map (λ (f) (thunk (f value))) listeners)
                                               (next-level-computations))])
        (if (null? (this-level-computations))
            (if (null? (next-level-computations))
                (void)
                (let ([comp (car (next-level-computations))])
                  (parameterize ([this-level-computations (cdr (next-level-computations))]
                                 [next-level-computations '()])
                    (comp))))
            (let ([comp (car (this-level-computations))])
              (parameterize ([this-level-computations (cdr (this-level-computations))])
                (comp)))))
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
      (set! current-value value)
      (super call value)
      )
    ))
(define a (new behavior% [init-value 1]))
(define b (run-behavior (a) a))
(define c (run-behavior (a) (+ a 1)))
(define d (run-behavior (b c) (+ b c)))
(send d add-listener displayln)
