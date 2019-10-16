#lang racket


(define (combinar-aux Lista1 Lista2 valores) (
                                  cond ((null? Lista2)
                                        '())
                                       (else
                                        (append
                                         (list (list (car valores ) (car Lista2) (car Lista1)))
                                         (combinar-aux Lista1 (cdr Lista2) (cdr valores))))
                                       ))

(define (combinar Lista1 Lista2 valores) (
                                  cond ((null? Lista1)
                                        '())
                                       (else
                                        (append (combinar-aux Lista1 Lista2 valores) (combinar (cdr Lista1) Lista2 valores)))
                                       ))
(provide combinar)

(define (delete Lista N) (
                    cond ((null? Lista)
                          '())
                         ((zero? N)
                          (cdr Lista))
                         (else
                          (cons (car Lista) (delete (cdr Lista) (- N 1))))
                         ))

(provide delete)

(define (get Lista N) (
                    cond ((null? Lista)
                          #f)
                         ((zero? N)
                          (car Lista))
                         (else
                          (get (cdr Lista) (- N 1)))
                         ))

  
(define (pedir-aux Njugador jugadores carta mazo) (
                          cond ((zero? Njugador)
                                (cons (list (caar jugadores)
                                      (append (cadar jugadores) (list (get mazo carta))))
                                      (cdr jugadores)))
                               (else
                                (cons (car jugadores) (pedir-aux (- Njugador 1) (cdr jugadores) carta mazo)))
                               ))
(provide pedir-aux)

