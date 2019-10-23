#lang racket

(require racket/gui pict)
(#%require (only racket/base random))
(require "logica.rkt")

; Variables globales
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
(define CantidadJugadores 4)
(define game_spacing 10)

(define huge_font (make-object font% 30 'modern))
(define big_font (make-object font% 20 'modern))
(define medium_font (make-object font% 16 'modern))
(define small_font (make-object font% 12 'modern))

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



; Devuelve el puntaje total de un dado jugador según las cartas que posean
(define (suma njugador)
  (sumar_jugador_aux njugador jugadores 0)
  )


; Cambia el turno según el jugador especificado
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


; Revisa que el puntaje de un jugador sea menor a 21 para pedir una siguiente carta. De otro modo pasa el turno.
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

; Le permite al crupier actuar como jugador y una vez termina procede a pedir la tabla de puntuacion
(define (crupier_play)
  (cond
    ((<= (suma 0) 16) (begin (sleep/yield 1) (get_card (pedir 0) 0) (sleep/yield 2) (crupier_play)))
    (else
     (begin
       (send crupier_score set-label (string-append "Score : " (number->string (suma 0))))
       (send (car (send crupier_cards get-children))
             set-label (pict->bitmap (scale (bitmap (read-bitmap (string-append "imgs/cards/" (caddr (caar (cdar jugadores))) (cadr (caar (cdar jugadores))) ".png"))) 0.9)))
       (sleep/yield 2)
       (ventana_resultados jugadores))
     )
    )
  )
    

;;;;;;;;;;;;;;;;;;;;;
;;;;;; GUI ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;


; Main Menu
(define menu_frame (new frame%
                   [label "BlaCEkjack"]
                   [spacing game_spacing]
                   [min-width 1280]
                   [min-height 720]))

(new message%
     [parent menu_frame]
     [label "BlaCEkjack"]
     [vert-margin (* game_spacing 5)]
     [font huge_font])

(define players-input-panel (new vertical-panel%
                                 [parent menu_frame]
                                 [spacing (* game_spacing 8)]
                                 [horiz-margin (* game_spacing 20)]
                                 [alignment '(center center)]))

(define input1 (new text-field%
                    [label "Jugador 1"]
                    [parent players-input-panel]
                    [font medium_font]))

(define input2 (new text-field%
                    [label "Jugador 2"]
                    [parent players-input-panel]
                    [font medium_font]))

(define input3 (new text-field%
                    [label "Jugador 3"]
                    [parent players-input-panel]
                    [font medium_font]))

(define player_names '())

(define (validate_names)
  (cond ((and (not (equal? (send input1 get-value) "")) (not (equal? (send input2 get-value) "")) (not (equal? (send input3 get-value) "")))
              (set! player_names (list (send input1 get-value) (send input2 get-value) (send input3 get-value))))
         ((and (not (equal? (send input1 get-value) "")) (not (equal? (send input2 get-value) "")))
              (set! player_names (list (send input1 get-value) (send input2 get-value))))
         ((and (not (equal? (send input1 get-value) "")))
              (set! player_names (list (send input1 get-value))))))
                       

(define start_game_button (new button%[parent players-input-panel]
     [label "Comenzar"]
     [font medium_font]
     [callback (lambda (button event)
                 (begin (validate_names) (cond ((not (equal? player_names '())) (begin (bCEj player_names) (send menu_frame show #f))))))]))

                   

; Total game scene
(define game_frame (new frame%
                   [label "BlaCEkjack"]
                   [spacing game_spacing]
                   [min-width 1280]
                   [min-height 720]))


; Crupier scene (inside total game scene)
(define crupier-panel (new vertical-panel%
                           [parent game_frame]
                           [spacing game_spacing]))

(new message%
     [parent crupier-panel]
     [label "Crupier"]
     [font big_font])


(define crupier_cards (new horizontal-panel%
                           [parent crupier-panel]
                           [style '(auto-hscroll)]
                           [alignment '(center center)]))

(define crupier_turn (new message% [parent crupier-panel]
                 [label "Not your turn"]))


(define crupier_score (new message% [parent crupier-panel]
                           [label "Score : ?"]))


(define crupier-stance-panel (new horizontal-panel%
                                  [parent crupier-panel]
                                  [spacing (/ game_spacing 2)]
                                  [alignment '(center bottom)]))

; Scene shared by all players (inside Total game scene)
(define players-panel (new horizontal-panel%
                           [parent game_frame]
                           [spacing 1]))

; Player 1 scene (inside scene shared by all players)
(define player1-panel (new vertical-panel%
                           [parent players-panel]
                           [style '(border deleted)]
                           [enabled #f]
                           [spacing game_spacing]))

(define player1_name (new message%
                          [parent player1-panel]
                          [font big_font]
                          [label ""]))

(define player1_cards (new horizontal-panel%
                           [parent player1-panel]
                           [alignment '(center center)]
                           [style '(auto-hscroll)]))

(define player1_turn (new message%
                          [parent player1-panel]
                          [font medium_font]
                          [label "Not your turn"]))

(define player1_score (new message%
                           [parent player1-panel]
                           [font medium_font]
                           [label "Score : 0"]
                           [auto-resize #t]))

(define player1-stance-panel (new horizontal-panel%
                                  [parent player1-panel]  
                                  [alignment '(center center)]
                                  [spacing (/ game_spacing 2)]))

(define player1-hit (new button%[parent player1-stance-panel]
     [label "Hit"]
     [enabled #f]
     [font small_font]
     [callback (lambda (button event)
                 (begin (get_card (pedir current_turn) current_turn) (validate_score 1)))]))

(define player1-stand (new button%
                           [parent player1-stance-panel]
                           [label "Stand"]
                           [font small_font]
                           [enabled #f]
                           [callback (lambda (button event)
                 (change_turn 2))]))

; Player 2 scene (inside scene shared by all players)
(define player2-panel (new vertical-panel%
                           [parent players-panel]
                           [style '(border deleted)]
                           [enabled #f]
                           [spacing game_spacing]))

(define player2_name (new message%
                          [parent player2-panel]
                          [font big_font]
                          [label ""]))

(define player2_cards (new horizontal-panel%
                           [parent player2-panel]
                           [alignment '(center center)]
                           [style '(auto-hscroll)]))

(define player2_turn (new message% [parent player2-panel]
                 [label "Not your turn"]
                 [font medium_font]))

(define player2_score (new message% [parent player2-panel]
                           [label "Score : 0"]
                           [font medium_font]))

(define player2-stance-panel (new horizontal-panel%
                                  [parent player2-panel]
                                  [spacing (/ game_spacing 2)]
                                  [alignment '(center center)]))

(define player2-hit (new button% [parent player2-stance-panel]
     [label "Hit"]
     [enabled #f]
     [font small_font]
     [callback (lambda (button event)
                 (begin (get_card (pedir current_turn) current_turn) (validate_score 2)))]))

(define player2-stand (new button% [parent player2-stance-panel]
     [label "Stand"]
     [enabled #f]
     [font small_font]
     [callback (lambda (button event)
                 (change_turn 3))]))

; Player 3 scene (inside scene shared by all players)
(define player3-panel (new vertical-panel%
                           [parent players-panel]
                           [style '(border deleted)]
                           [enabled #f]
                           [alignment '(center center)]
                           [spacing game_spacing]))

(define player3_name (new message%
                          [parent player3-panel]
                          [font big_font]
                          [label ""]))

(define player3_cards (new horizontal-panel%
                           [parent player3-panel]
                           [alignment '(center center)]
                           [style '(auto-hscroll)]))


(define player3_turn (new message% [parent player3-panel]
                 [label "Not your turn"]
                 [font medium_font]))

(define player3_score (new message% [parent player3-panel]
                           [label "Score : 0"]
                           [font medium_font]))

(define player3-stance-panel (new horizontal-panel%
                                  [parent player3-panel]
                                  [spacing (/ game_spacing 2)]    
                                  [alignment '(center center)]))

(define player3-hit (new button% [parent player3-stance-panel]
     [label "Hit"]
     [enabled #f]
     [font small_font]
     [callback (lambda (button event)
                 (begin (get_card (pedir current_turn) current_turn) (validate_score 3)))]))

(define player3-stand (new button% [parent player3-stance-panel]
     [label "Stand"]
     [enabled #f]
     [font small_font]
     [callback (lambda (button event)
                 (change_turn 4))]))

; Gets image from the given card and adds it to the corresponding player of the given player number to their card panel
(define (get_card card player_num)
  (cond ((file-exists? (string-append "imgs/cards/" (car (cdr (cdr card))) (car (cdr card)) ".png"))
         (cond
           ((equal? player_num 0) (cond ((equal? (list_length (car (cdr (car jugadores))) 0) 1)
                                         (begin
                                         (new message% [parent crupier_cards]
                 [label (pict->bitmap (scale (bitmap (read-bitmap (string-append "imgs/cards/back.png"))) 0.6))])))
                                        (else (begin
                                         (new message% [parent crupier_cards]
                 [label (pict->bitmap (scale (bitmap (read-bitmap (string-append "imgs/cards/" (car (cdr (cdr card))) (car (cdr card)) ".png"))) 0.9))])))))
            
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

; Returns list length
(define (list_length arr length)
  (cond ((equal? arr '()) length)
        (else
         (list_length (cdr arr) (+ length 1)))))

; Names players on gui
(define (set_player_name name num)
  (cond ((equal? num 1) (begin (send player1-panel enable #t) (send player1_name set-label name) (send players-panel add-child player1-panel)))
        ((equal? num 2) (begin (send player2-panel enable #t) (send player2_name set-label name) (send players-panel add-child player2-panel)))
        ((equal? num 3) (begin (send player3-panel enable #t) (send player3_name set-label name) (send players-panel add-child player3-panel)))))

; Initializes game setting up necessary logic and gui elements
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
     (write "Cant start game with given amount of players"))))





(define window_width 45)
(define window_height 30)
(define cell_width (/ window_width 3))
(define cell_height (/ window_height 3))
(define frame (new frame% [label "Resultados - ♠ ♥ ♦ ♣"]
                   [min-width window_width]	 
                   [min-height window_height]))

(define column0 (new horizontal-panel% [parent frame]))
(define column1 (new horizontal-panel% [parent frame]))
(define column2 (new horizontal-panel% [parent frame]))
(define column3 (new vertical-panel% [parent frame]))

(define celda00 (new text-field%
                      (label "")
                      (parent column0)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
                      [enabled #f]
                      ))
(define celda01 (new text-field%
                      (label "")
                      (parent column0)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
                      [enabled #f]
                      ))
(define celda02 (new text-field%
                      (label "")
                      (parent column0)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
                      [enabled #f]
                      ))

(define celda10 (new text-field%
                      (label "")
                      (parent column1)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
                      [enabled #f]
                      ))
(define celda11 (new text-field%
                      (label "")
                      (parent column1)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
                      [enabled #f]
                      ))
(define celda12 (new text-field%
                      (label "")
                      (parent column1)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
                      [enabled #f]
                      ))

(define celda20 (new text-field%
                      (label "")
                      (parent column2)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
                      [enabled #f]
                      ))
(define celda21 (new text-field%
                      (label "")
                      (parent column2)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
                      [enabled #f]
                      ))
(define celda22 (new text-field%
                      (label "")
                      (parent column2)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
                      [enabled #f]
                      ))


(define boton_repetir (new button% [parent column3]
     [label "Repetir"]
     [min-width 50]
     [min-height 50]
     ))

(define boton_salir (new button% [parent column3]
     [label "Salir"]
     [min-width 50]
     [min-height 50]
     ))

; ACTUALIZADA
; funcion que despliega la tabla de resultados

(define (ventana_resultados jugadores)
  (mostrar_resultado_final (conteo_final (cdr jugadores) (sumar_jug (car jugadores)) '()) 0)
  )

; ACTUALIZADA
; funcion que muestra una tabla con las puntuaciones finales de los jugadores
; recibe un conteo final y un contador
(define (mostrar_resultado_final conteo_final cont)

   (cond
     [(or (null? conteo_final) (= cont 3)) (send frame show #t)]
     (else
      (cond
        [(= cont 0) (send celda00 set-value (caar conteo_final))     ; Nombre
                     (send celda01 set-value (~v (cadar conteo_final)))    ; Puntaje final
                     (send celda02 set-value (caddar conteo_final))  ; Estado final
                     (mostrar_resultado_final (cdr conteo_final) 1)]
        
        [(= cont 1)(send celda10 set-value (caar conteo_final))
                     (send celda11 set-value (~v (cadar conteo_final)))
                     (send celda12 set-value (caddar conteo_final))
                     (mostrar_resultado_final (cdr conteo_final) 2)]
        
        [(= cont 2)(send celda20 set-value (caar conteo_final))
                     (send celda21 set-value (~v (cadar conteo_final)))
                     (send celda22 set-value (caddar conteo_final))
                     (mostrar_resultado_final (cdr conteo_final) 3)]
        (else "Ya no puedo ingresar mas jugadores")
        )

   )  
     )
  
  )

(define (sumar_jugador njugador lista)
  (sumar_jugador_aux njugador lista 0)
  )
(define (sumar_jugador_aux njugador lista n)
  (cond
    [(= n njugador)
     (sumar_jug (car lista))]
    (else
     (sumar_jugador_aux njugador (cdr lista) (+ n 1))
     )
    )
  )


;; Initializes main menu
(send menu_frame show #t)

