#lang racket
(#%require (only racket/base random))
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

(define (combinar-aux Lista1 Lista2 valores) (
                                  cond ((null? Lista2)
                                        '())
                                       (else
                                        (append
                                         (list (list (car valores )(car Lista1) (car Lista2)))
                                         (combinar-aux Lista1 (cdr Lista2) (cdr valores))))
                                       ))

(define (combinar Lista1 Lista2 valores) (
                                  cond ((null? Lista1)
                                        '())
                                       (else
                                        (append (combinar-aux Lista1 Lista2 valores) (combinar (cdr Lista1) Lista2 valores)))
                                       ))
(define (test1 N) (
                   cond ((equal? N 10)
                         N)
                        (else
                         (test1 (random 100)))
                        ))

(define mazo (combinar palos nombres valores))

(define jugadores
  '(("crupier" #f (list))
    ("pancho" #f (list))
    ("lola" #f (list))
    ("manuela" #f (list))
  )
  )
(define conteo 52)

(define CantidadJugadores 4)

(define (delete Lista N) (
                    cond ((null? Lista)
                          '())
                         ((zero? N)
                          (cdr Lista))
                         (else
                          (cons (car Lista) (delete (cdr Lista) (- N 1))))
                         ))

(define (get Lista N) (
                    cond ((null? Lista)
                          #f)
                         ((zero? N)
                          (car Lista))
                         (else
                          (get (cdr Lista) (- N 1)))
                         ))

(define (test2 v) (set! v (+ v 1)))
  
(define (pedir-aux Njugador jugadores carta) (
                          cond ((zero? Njugador)
                                (cons (list (caar jugadores)
                                      (cadar jugadores)
                                      (append (caddar jugadores) (list (get mazo carta))))
                                      (cdr jugadores)))
                               (else
                                (cons (car jugadores) (pedir-aux (- Njugador 1) (cdr jugadores) carta)))
                               ))

(define (pedir Njugador) (cond (#t
                                (let ([rnd (random conteo)])
                                  (set! jugadores (pedir-aux Njugador jugadores rnd))
                                  (set! conteo (- conteo 1))
                                  (set! mazo (delete mazo rnd)))
                                )
                               ))

(define (iniciar-aux Ncartas) (
                           cond ((zero? Ncartas)
                                 )
                                (else
                                 (pedir (remainder Ncartas CantidadJugadores))
                                 (writeln (remainder Ncartas CantidadJugadores))
                                 (iniciar-aux (- Ncartas 1)))
                                ))

(define (iniciar Ncartas) (iniciar-aux (* Ncartas CantidadJugadores)))

(define (plantar-aux Njugador jugadores) (
                                cond ((zero? Njugador)
                                      (cons (list (caar jugadores)
                                                  #t
                                                  (caddar jugadores))
                                            (cdr jugadores)))
                               (else
                                (cons (car jugadores) (plantar-aux (- Njugador 1) (cdr jugadores))))
                               ))

(define (plantar Njugador) (
                            cond (#t
                                  (set! jugadores (plantar-aux Njugador jugadores)))
                                 ))

(define (plantado?-aux Njugador jugadores) (
                              cond ((zero? Njugador)
                                    (cadar jugadores))
                                   (else
                                    (plantado?-aux (- Njugador 1) (cdr jugadores)))
                               ))

(define (plantado? Njugador) (plantado?-aux Njugador jugadores))

(define (suma Njugador) 1)

(define (pedirCrupier) (
                        cond (< (suma 0) 17
                              (pedir 0)
                              (pedirCrupier))
                             (else
                              1)
                             ))


