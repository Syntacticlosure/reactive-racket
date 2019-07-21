#lang racket
(require "../base.rkt" "../gui.rkt" racket/gui)

(define frame (new frame% [label "count clicks"][width 400][height 300]))
(define bt (new rbutton% [label "click me"] [parent frame]))
(define msg (new message% [label "                              "][parent frame]))
(define times (foldl-b + 0 (map-e (λ (_) 1) (send bt get-clicks))))
(send times add-listener
      (λ (v) (send msg set-label (format "you clicked ~a times!" v))))
(send frame show #t)