#lang racket
(require "../base.rkt" "../gui.rkt" racket/gui)

(define frame (new frame% [label "spreadsheet"]
                   [width 700][height 400]))
(define block-width 60)
(define block-height 25)
(define pen (new pen% [color "black"][width 1]))
(define selected-brush (new brush% [color "silver"]))
(define no-border (new pen% [style 'transparent]))


(define (drawer cvs dc)
  (define (draw-num x y text selected?)
    (when selected?
      (define old-pen (send dc get-pen))
      (send dc set-pen no-border)
      (send dc set-brush selected-brush)
      (send dc draw-rectangle (* x block-width)
            (* y block-height) block-width block-height)
      (send dc set-pen old-pen))
    (send dc draw-text (~a text) (+ 3 (* x block-width))
          (+ 2 (* y block-height)) ))
  (snapshot (lines cols datas current-selected)
            (define total-width (* block-width cols))
            (define total-height (* block-height lines))
            (send dc set-pen pen)
            (for ([i (range 0 (+ total-height 1) block-height)])
              (send dc draw-line 0 i total-width i))
            (for ([i (range 0 (+ total-width 1) block-width)])
              (send dc draw-line i 0 i total-height))
            (for* ([i cols]
                   [j lines])
              (draw-num i j (hash-ref datas (cons i j) "")
                        (and (= i (car current-selected))
                             (= j (cdr current-selected)))))
            ))
(define canvas (new rcanvas% [parent frame]
                    [paint-callback drawer]))
(define more-lines-button (new rbutton% [label "more lines"][parent frame]))
(define more-cols-button (new rbutton% [label "more cols"][parent frame]))
(define lines (foldl-b
               (λ (_ acc)
                 (+ acc 1)) 3 (send more-lines-button get-clicks)))
(define cols (foldl-b
              (λ (_ acc)
                (+ acc 1)) 3 (send more-cols-button get-clicks)))
(define datas (foldl-b
               (λ (key hs)
                 (snapshot (current-selected)
                           (define nhs (if (hash-ref hs current-selected #f) hs
                                           (hash-set hs current-selected "")))
                           (match key
                             [(or #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)
                              (hash-update nhs current-selected
                                           (λ (v) (string-append v (~a key))))]
                             [#\backspace
                              (hash-update nhs current-selected
                                           (λ (v) (if (string=? v "")
                                                      v
                                                      (substring v 0 (- (string-length v) 1)))))]
                             [_ nhs])))
               (hash)
               (send canvas get-key-strokes)))
                          
(define current-selected (hold (filter-e (λ (x) (snapshot (lines cols)
                                                          (and (<= 0 (car x)(- cols 1))
                                                               (<= 0 (cdr x) (- lines 1)))))
                                         (map-e (λ (ev)
                                                  (cons (quotient (send ev get-x) block-width)
                                                        (quotient (send ev get-y) block-height)
                                                        ))
                                                (send canvas get-left-clicks))) (cons 0 0)))
(define all-states (run-behavior (lines cols current-selected datas)
                                 (list lines cols current-selected datas)))
(send all-states add-listener (λ (v) (send canvas refresh)))
(send frame show #t)