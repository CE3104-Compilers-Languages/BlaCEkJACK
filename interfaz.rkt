#lang racket

(require racket/gui pict)
(#%require (only racket/base random))
(require "logica.rkt")


(define palos '("corazones"
                "diamantes"
                "plicas"
                "treboles"))

(define nombres '("As" "2" "3" "4"
                "5" "6" "7" "8"
                "9" "10" "J" "Q" "K"))

(define valores '(11 2 3 4
                5 6 7 8
                9 10 10 10 10))


(define mazo (combinar palos nombres valores))
(define jugadores '())
(define conteo 52)
(define current_turn 0)
(define turno 1)
(define CantidadJugadores 4)

(define (GenerarJugadores nombres)
  (set! jugadores (AsignarNombres nombres))
  (set! CantidadJugadores (+ 1 (len nombres)))
  )

(define (IniciarJuego nombres)
  (GenerarJugadores nombres)
  (IniciarCartas (* 2 CantidadJugadores))
  )

;Método que actualiza los mazos de la partida al pedir una nueva carta
(define (pedir Njugador)
  (let* ([rnd (random conteo)] [carta (get mazo rnd)])
    (set! jugadores (pedir-aux Njugador jugadores rnd mazo))
    (set! conteo (- conteo 1))
    (set! mazo (delete mazo rnd))
    carta)
                               )

;Método que pide un número N de cartas que se distribuyen entre todos los jugadores
(define (IniciarCartas Ncartas) (
                           cond ((zero? Ncartas)
                                 )
                                (else
                                 (let ([turn (remainder Ncartas CantidadJugadores)])
                                   (get_card (pedir turn) turn)
                                   )
                                 (IniciarCartas (- Ncartas 1)))
                                ))

;Método que finaliza el turno de un jugador y pasa al siguiente al siguiente
(define (plantar)
  (set! turno (remainder (+ turno 1) CantidadJugadores))
  turno
  )
;

;Método que evalúa si las cartas del crupier suman 16 o menos
;de ser así pide una carta más para el crupier y devuelve un True
;de lo contrario devuelve un False para avisar que no se debe volver a pedir
(define (pedirCrupier) (
                        cond (< (suma 0) 17
                              (pedir 0)
                              #t)
                             (else
                              #f)
                             ))


(define (suma njugador)
  (sumar_jugador_aux njugador jugadores 0)
  )



; Variables globales
(define game_spacing 10)

(define (change_turn player_num)
  (cond
    ((and (equal? player_num 1) (>= (list_length jugadores 0) (+ player_num 1))) (begin
                                                                             (send player1_turn set-label "Playing!")
                                                                             (send player1-hit enable #t)
                                                                             (send player1-stand enable #t)
                                                                             (set! current_turn player_num)))
  ((and (equal? player_num 2) (>= (list_length jugadores 0) (+ player_num 1))) (begin
                                                                                       (send player1_turn set-label "Already played")
                                                                                       (send player1-stance-panel enable #f)
                                                                                       (send player2_turn set-label "Playing!")
                                                                                       (send player2-hit enable #t)
                                                                                       (send player2-stand enable #t)
                                                                                       (set! current_turn player_num)))
  ((and (equal? player_num 3) (>= (list_length jugadores 0) (+ player_num 1))) (begin
                                                                                 (send player2_turn set-label "Already played")
                                                                                 (send player2-stance-panel enable #f)
                                                                                 (send player3_turn set-label "Playing")
                                                                                 (send player3-hit enable #t)
                                                                                 (send player3-stand enable #t)
                                                                                 (set! current_turn player_num)))
  ((equal? player_num -1) (send game_frame show #t))
   
  (else
   (begin
     (set! current_turn 0)
      (send player1_turn set-label "Already played")
      (send player1-stance-panel enable #f)
      (send player2_turn set-label "Already played")
      (send player2-stance-panel enable #f)
      (send player3_turn set-label "Already played")
      (send player3-stance-panel enable #f)
      (send crupier_turn set-label "Playing!")
      (crupier_play)))))

(define (update_cards player_num)#t)

(define (validate_score player_num)
  (cond
    ((>= (suma player_num) 21) (cond
                                 ((equal? player_num 0) (change_turn -1))
                                 ((< (list_length jugadores 0) (+ player_num 2)) (change_turn 0))
                                 (else
                                  (change_turn (+ player_num 1))
                                  )
                                 )
                               )
   )
 )

(define (crupier_play)
  (cond
    ((< (suma 0) 16) (begin (sleep/yield 2) (get_card (pedir 0) 0) (crupier_play)))
    (else
     (begin (sleep/yield 2) (send game_frame show #f))
     )
    )
  )
    
        

; Total game scene
(define game_frame (new frame%
                   [label "Blackjack"]
                   [spacing game_spacing]
                   [min-width 1280]
                   [min-height 720]))


; Crupier scene (inside total game scene)
(define crupier-panel (new vertical-panel%
                           [parent game_frame]
                           [spacing game_spacing]))

(new message%
     [parent crupier-panel]
     [label "Crupier"])


(define crupier_cards (new horizontal-panel%
                           [parent crupier-panel]
                           [style '(auto-hscroll)]
                           [alignment '(center center)]))

(define crupier_turn (new message% [parent crupier-panel]
                 [label "Not your turn"]))

(define crupier_score (new message% [parent crupier-panel]
                           [label "Score : 0"]))


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

(define player1_name (new message%
                          [parent player1-panel]
                          [label ""]))

(define player1_cards (new horizontal-panel%
                           [parent player1-panel]
                           [alignment '(center center)]
                           [style '(auto-hscroll)]))

(define player1_turn (new message%
                          [parent player1-panel]
                          [label "Not your turn"]))

(define player1_score (new message%
                           [parent player1-panel]
                           [label "Score : 0"]
                           [auto-resize #t]))

(define player1-stance-panel (new horizontal-panel%
                                  [parent player1-panel]  
                                  [alignment '(center center)]
                                  [spacing (/ game_spacing 2)]))

(define player1-hit (new button% [parent player1-stance-panel]
     [label "Hit"]
     [enabled #f]
     [callback (lambda (button event)
                 (begin (get_card (pedir current_turn) current_turn) (validate_score 1)))]))

(define player1-stand (new button%
                           [parent player1-stance-panel]
                           [label "Stand"]
                           [enabled #f]
                           [callback (lambda (button event)
                 (change_turn 2))]))

; Player 2 scene (inside scene shared by all players)
(define player2-panel (new vertical-panel%
                           [parent players-panel]
                           [enabled #f]
                           [spacing game_spacing]))

(define player2_name (new message% [parent player2-panel]
                          [label ""]))

(define player2_cards (new horizontal-panel%
                           [parent player2-panel]
                           [alignment '(center center)]
                           [style '(auto-hscroll)]))

(define player2_turn (new message% [parent player2-panel]
                 [label "Not your turn"]))

(define player2_score (new message% [parent player2-panel]
                           [label "Score : 0"]))

(define player2-stance-panel (new horizontal-panel%
                                  [parent player2-panel]
                                  [spacing (/ game_spacing 2)]
                                  [alignment '(center center)]))

(define player2-hit (new button% [parent player2-stance-panel]
     [label "Hit"]
     [enabled #f]
     [callback (lambda (button event)
                 (begin (get_card (pedir current_turn) current_turn) (validate_score 2)))]))

(define player2-stand (new button% [parent player2-stance-panel]
     [label "Stand"]
     [enabled #f]
     [callback (lambda (button event)
                 (change_turn 3))]))

; Player 3 scene (inside scene shared by all players)
(define player3-panel (new vertical-panel%
                           [parent players-panel]
                           [enabled #f]
                           [spacing game_spacing]))

(define player3_name (new message% [parent player3-panel]
                          [label ""]))

(define player3_cards (new horizontal-panel%
                           [parent player3-panel]
                           [alignment '(center center)]
                           [style '(auto-hscroll)]))


(define player3_turn (new message% [parent player3-panel]
                 [label "Not your turn"]))

(define player3_score (new message% [parent player3-panel]
                           [label "Score : 0"]))

(define player3-stance-panel (new horizontal-panel%
                                  [parent player3-panel]
                                  [spacing (/ game_spacing 2)]    
                                  [alignment '(center center)]))

(define player3-hit (new button% [parent player3-stance-panel]
     [label "Hit"]
     [enabled #f]
     [callback (lambda (button event)
                 (begin (get_card (pedir current_turn) current_turn) (validate_score 3)))]))

(define player3-stand (new button% [parent player3-stance-panel]
     [label "Stand"]
     [enabled #f]
     [callback (lambda (button event)
                 (change_turn 4))]))

(define (get_card card player_num)
  (cond ((file-exists? (string-append "imgs/cards/" (car (cdr (cdr card))) (car (cdr card)) ".png"))
         (cond
           ((equal? player_num 0) (cond ((equal? (list_length (car (cdr (car jugadores))) 0) 1)
                                         (begin
                                         (new message% [parent crupier_cards]
                 [label (pict->bitmap (scale (bitmap (read-bitmap (string-append "imgs/cards/back.png"))) 0.6))]))
                                         (send crupier_score set-label (string-append "Score : " (number->string (suma 0)))))
                                        (else (begin
                                         (new message% [parent crupier_cards]
                 [label (pict->bitmap (scale (bitmap (read-bitmap (string-append "imgs/cards/" (car (cdr (cdr card))) (car (cdr card)) ".png"))) 0.9))]))
                                              (send crupier_score set-label (string-append "Score : " (number->string (suma 0)))))))
            
           ((equal? player_num 1)
            (begin
            (new message% [parent player1_cards]
                 [label (pict->bitmap (scale (bitmap (read-bitmap (string-append "imgs/cards/" (car (cdr (cdr card))) (car (cdr card)) ".png"))) 0.9))])
            (send player1_score set-label (string-append "Score : " (number->string (suma 1))))))
           
           ((and (equal? player_num 2) (>= (list_length jugadores 0) 2))
            (begin
            (new message% [parent player2_cards]
                 [label (pict->bitmap (scale (bitmap (read-bitmap (string-append "imgs/cards/" (car (cdr (cdr card))) (car (cdr card)) ".png"))) 0.9))])
            (send player2_score set-label (string-append "Score : " (number->string (suma 2))))))
           
           ((and (equal? player_num 3) (>= (list_length jugadores 0) 3))
            (begin
            (new message% [parent player3_cards]
                 [label (pict->bitmap (scale (bitmap (read-bitmap (string-append "imgs/cards/" (car (cdr (cdr card))) (car (cdr card)) ".png"))) 0.9))])
            (send player3_score set-label (string-append "Score : " (number->string (suma 3))))))
           
           (else
           (write "Invalid player card assignment")))
         )
        (else
         (write "Invalid file path"))))

;; Functions utilized to initialize game
(define (list_length arr length)
  (cond ((equal? arr '()) length)
        (else
         (list_length (cdr arr) (+ length 1)))))

; Names players on gui
(define (set_player_name name num)
  (cond ((equal? num 1) (begin (send player1-panel enable #t) (send player1_name set-label name)))
        ((equal? num 2) (begin (send player2-panel enable #t) (send player2_name set-label name)))
        ((equal? num 3) (begin (send player3-panel enable #t) (send player3_name set-label name)))))

(define (bCEj X)
  (cond
    ((equal? (list_length jugadores 0) 0) (begin (set! jugadores (append jugadores (list (list "crupier" '())))) (bCEj X)))
    ((and (>= (list_length X 0) 1) (<= (list_length X 0) 3)) (begin
                                                               (set! jugadores (append jugadores (list (list (car X) '()))))
                                                               (set_player_name (car X) (- (list_length jugadores 0) 1))
                                                               (bCEj (cdr X))))
        ((and (equal? X '()) (not (equal? jugadores '()))) (begin
                                                             (change_turn 1)
                                                             (set! CantidadJugadores (list_length jugadores 0))
                                                             (send game_frame show #t)
                                                             (IniciarCartas (* 2 CantidadJugadores))))
        (else
         (write "Cant start game with given number of players"))))





