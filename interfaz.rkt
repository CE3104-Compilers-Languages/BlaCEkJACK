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


(define jugadores
  '(("crupier" (list))
    ("pancho" (list))
    ("lola" (list))
    ("manuela" (list))
  )
  )
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


