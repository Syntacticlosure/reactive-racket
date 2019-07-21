#lang racket
(require "../base.rkt" "../gui.rkt" racket/gui)

(define frame (new frame% [label "spreadsheet"]
                   [width 500][height 400]))

(define canvas (new rcanvas% [parent frame]))

(send frame show #t)