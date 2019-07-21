#lang racket
(require "../base.rkt" "../gui.rkt" racket/gui)

(define frame (new frame% [label "Sum Calculator"][width 400][height 300]))
(define tf1 (new rtext-field% [label "num1:"][parent frame][init-value "0"]))
(define tf2 (new rtext-field% [label "num2:"][parent frame][init-value "0"]))
(define result (new message% [label "            "][parent frame]))

(define num1-value (hold (filter-e values (map-e string->number
                                           (send tf1 get-values))) 0))
(define num2-value (hold (filter-e values (map-e string->number
                                           (send tf2 get-values))) 0))

(define result-value (run-behavior (num1-value num2-value)
                                   (+ num1-value num2-value)))
(send result-value add-listener (λ (v) (send result set-label (~a v))))

(send frame show #t)