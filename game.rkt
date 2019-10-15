#lang racket


(require racket/gui 2htdp/image)

; Variables globales
(define current_turn 1)
(define cards '())
(define players '())
(define game_spacing 10)

; Total game scene
(define game_frame (new frame%
                   [label "Blackjack"]
                   [spacing game_spacing]))

; Crupier scene (inside total game scene)
(define crupier-panel (new vertical-panel%
                           [parent game_frame]
                           [spacing game_spacing]))

(define msg (new message% [parent crupier-panel]
                 [label "Aqui van cartas de crupier"]))
(define msg2 (new message% [parent crupier-panel]
                 [label "Aqui va info de crupier"]))

(define crupier-stance-panel (new horizontal-panel%
                                  [parent crupier-panel]
                                  [spacing (/ game_spacing 2)]
                                  [alignment '(center bottom)]))

(new button% [parent crupier-stance-panel]
     [label "Hit"])
(new button% [parent crupier-stance-panel]
     [label "Stand"])

; Scene shared by all players (inside Total game scene)
(define players-panel (new horizontal-panel%
                           [parent game_frame]
                           [spacing (* game_spacing 2)]))

; Player 1 scene (inside scene shared by all players)
(define player1-panel (new vertical-panel%
                           [parent players-panel]))

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
                           [spacing game_spacing]))

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
                           [spacing game_spacing]))

(define msg7 (new message% [parent player3-panel]
                 [label "Aqui van cartas de jugador 3"]))

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