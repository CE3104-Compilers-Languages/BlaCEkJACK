#lang racket

; Método que toma tres listas y realiza todas las combinaciones del primer elemento de la lista1
; con todos los elementos de la lista 2 y a cada cominación pone un valor tomado de la lista de valores
(define (combinar-aux Lista1 Lista2 valores) (
                                  cond ((null? Lista2)
                                        '())
                                       (else
                                        (append
                                         (list (list (car valores ) (car Lista2) (car Lista1)))
                                         (combinar-aux Lista1 (cdr Lista2) (cdr valores))))
                                       ))

; Método que toma tres listas y realiza todas las combinaciones de
; todos los elementos de la lista1 con todos los elementos de la lista 2
(provide combinar)
(define (combinar Lista1 Lista2 valores) (
                                  cond ((null? Lista1)
                                        '())
                                       (else
                                        (append (combinar-aux Lista1 Lista2 valores) (combinar (cdr Lista1) Lista2 valores)))
                                       ))

; Método que devuelve una lista sin su N-éssimo término
(provide delete)
(define (delete Lista N) (
                    cond ((null? Lista)
                          '())
                         ((zero? N)
                          (cdr Lista))
                         (else
                          (cons (car Lista) (delete (cdr Lista) (- N 1))))
                         ))
; Método que devuelve el N-ésimo de una lista
(define (get Lista N) (
                    cond ((null? Lista)
                          #f)
                         ((zero? N)
                          (car Lista))
                         (else
                          (get (cdr Lista) (- N 1)))
                         ))

; Método que añade una carta al jugador N de la partida a partir del mazo
; y con un número aleatorio de carta dada externamente
; se encarga de reconstruir toda la lista de jugadores y devolver una lista actualizada
(provide pedir-aux)
(define (pedir-aux Njugador jugadores carta mazo) (
                          cond ((zero? Njugador)
                                (cons (list (caar jugadores)
                                      (append (cadar jugadores) (list (get mazo carta))))
                                      (cdr jugadores)))
                               (else
                                (cons (car jugadores) (pedir-aux (- Njugador 1) (cdr jugadores) carta mazo)))
                               ))


