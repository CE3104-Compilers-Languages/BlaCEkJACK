#lang racket

(require racket/gui)
(#%require (only racket/base random))
(require "logica.rkt")


(define palos '("corazones"
                "diamantes"
                "plicas"
                "treboles"))

(define nombres '("As" "2" "3" "4"
                "5" "6" "7" "8"
                "9" "10" "J" "Q" "K"))

(define valores '((1 11) 2 3 4
                5 6 7 8
                9 10 10 10 10))


(define mazo (combinar palos nombres valores))

(define crupier '("Crupier" '()))
(define jugadores '())
(define conteo 52)

(define CantidadJugadores 4)

;Método que actualiza los mazos de la partida al pedir una nueva carta
(define (pedir Njugador) (cond (#t
                                (let ([rnd (random conteo)])
                                  (set! jugadores (pedir-aux Njugador jugadores rnd mazo))
                                  (set! conteo (- conteo 1))
                                  (set! mazo (delete mazo rnd)))
                                )
                               ))

;Método que pide un número N de cartas que se distribuyen entre todos los jugadores
(define (iniciar Ncartas) (
                           cond ((zero? Ncartas)
                                 )
                                (else
                                 (pedir (remainder Ncartas CantidadJugadores))
                                 (iniciar (- Ncartas 1)))
                                ))


(define (suma Njugador) 1)

(define (pedirCrupier) (
                        cond (< (suma 0) 17
                              (pedir 0)
                              (pedirCrupier))
                             ))


; Variables globales
(define current_turn 1)
(define cards '())
(define players '())
(define game_spacing 10)

; Total game scene
(define game_frame (new frame%
                   [label "Blackjack"]
                   [spacing game_spacing]))

;(define background_img (make-object bitmap% "imgs/background.jpg"))

;(define canvas
;  (instantiate canvas%
;    (game_frame)
;    (paint-callback
;     (lambda (canvas dc)
;       (send dc draw-bitmap background_img 0 0)))
;    (min-width (send background_img get-width))
;    (min-height (send background_img get-height))))

; WIP IMAGE PRODUCTION
(define (get_img img_name)
  (bitmap% img_name))

; Crupier scene (inside total game scene)
(define crupier-panel (new vertical-panel%
                           [parent game_frame]
                           [spacing game_spacing]))

(new message% [parent crupier-panel]
     [label "Crupier"])

(define msg (new message% [parent crupier-panel]
                 [label "Aqui van cartas de crupier"]))
(define msg2 (new message% [parent crupier-panel]
                 [label "Aqui va info de crupier"]))

(define crupier-stance-panel (new horizontal-panel%
                                  [parent crupier-panel]
                                  [spacing (/ game_spacing 2)]
                                  [alignment '(center bottom)]))

; Scene shared by all players (inside Total game scene)
(define players-panel (new horizontal-panel%
                           [parent game_frame]
                           [spacing (* game_spacing 2)]))

; Player 1 scene (inside scene shared by all players)
(define player1-panel (new vertical-panel%
                           [parent players-panel]
                           [enabled #f]
                           [spacing game_spacing]))

(define player1_name (new message% [parent player1-panel]
                          [label ""]))

(define msg3 (new message% [parent player1-panel]
                 [label "Aqui van cartas del jugador 1"]))
(define msg4 (new message% [parent player1-panel]
                 [label "Aqui va info jugador 1"]))

(define player1-stance-panel (new horizontal-panel%
                                  [parent player1-panel]
                                  [spacing (/ game_spacing 2)]
                                  [alignment '(center bottom)]))

(new button% [parent player1-stance-panel]
     [label "Hit"])
(new button% [parent player1-stance-panel]
     [label "Stand"])

; Player 2 scene (inside scene shared by all players)
(define player2-panel (new vertical-panel%
                           [parent players-panel]
                           [enabled #f]
                           [spacing game_spacing]))

(define player2_name (new message% [parent player2-panel]
                          [label ""]))

(define msg5 (new message% [parent player2-panel]
                 [label "Aqui van cartas de jugador 2"]))
(define msg6 (new message% [parent player2-panel]
                 [label "Aqui va info jugador 2"]))

(define player2-stance-panel (new horizontal-panel%
                                  [parent player2-panel]
                                  [spacing (/ game_spacing 2)]
                                  [alignment '(center bottom)]))

(new button% [parent player2-stance-panel]
     [label "Hit"])
(new button% [parent player2-stance-panel]
     [label "Stand"])

; Player 3 scene (inside scene shared by all players)
(define player3-panel (new vertical-panel%
                           [parent players-panel]
                           [enabled #f]
                           [spacing game_spacing]))

(define player3_name (new message% [parent player3-panel]
                          [label ""]))

(define player3-cards-panel (new horizontal-panel%
                                 [parent player3-panel]))

(define msg7 (new message% [parent player3-panel]
                 [label (read-bitmap "imgs/cards/cardClubs2.png")]))


(define msg8 (new message% [parent player3-panel]
                 [label "Aqui va info jugador 3"]))

(define player3-stance-panel (new horizontal-panel%
                                  [parent player3-panel]
                                  [spacing (/ game_spacing 2)]
                                  [alignment '(center bottom)]))

(new button% [parent player3-stance-panel]
     [label "Hit"])
(new button% [parent player3-stance-panel]
     [label "Stand"])

; Functions utilized to initialize game
(define (list_length arr length)
  (cond ((equal? arr '()) length)
        (else
         (list_length (cdr arr) (+ length 1)))))

; Names players in the interface (numbers are inverted because they are compared to "jugadores" list length
(define (set_player_name name num)
  (cond ((equal? num 1) (begin (send player1-panel enable #t) (send player1_name set-label name)))
        ((equal? num 2) (begin (send player2-panel enable #t) (send player2_name set-label name)))
        ((equal? num 3) (begin (send player3-panel enable #t) (send player3_name set-label name)))))

(define (bCEj X)
  (cond ((and (>= (list_length X 0) 1) (<= (list_length X 0) 3)) (begin (set! jugadores (append jugadores (list (list (car X) '()))))
                                                                   (set_player_name (car X) (list_length jugadores 0))
                                                                   (bCEj (cdr X))))
        ((and (equal? X '()) (not (equal? jugadores '()))) (send game_frame show #t))
        (else
         (write "Cant start game with number of players given"))))






