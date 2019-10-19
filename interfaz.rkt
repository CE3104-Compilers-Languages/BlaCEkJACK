#lang racket
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


(define jugadores '())
(define CantidadJugadores 0)
(define conteo 52)
(define turno 1)

(define (GenerarJugadores nombres)
  (set! jugadores (AsignarNombres nombres))
  (set! CantidadJugadores (+ 1 (len nombres)))
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
(define (iniciar Ncartas) (
                           cond ((zero? Ncartas)
                                 )
                                (else
                                 (pedir (remainder Ncartas CantidadJugadores))
                                 (iniciar (- Ncartas 1)))
                                ))
;Método que finaliza el turno de un jugador y pasa al siguiente al siguiente
(define (plantar)
  (set! turno (remainder (+ turno 1) CantidadJugadores))
  turno
  )
;
(define (suma Njugador) 1)

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


