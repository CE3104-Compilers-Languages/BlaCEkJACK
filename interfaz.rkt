#lang racket/gui
(require "logica.rkt")
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
                      ))
(define celda01 (new text-field%
                      (label "")
                      (parent column0)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
                      ))
(define celda02 (new text-field%
                      (label "")
                      (parent column0)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
                      ))

(define celda10 (new text-field%
                      (label "")
                      (parent column1)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
                      ))
(define celda11 (new text-field%
                      (label "")
                      (parent column1)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
                      ))
(define celda12 (new text-field%
                      (label "")
                      (parent column1)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
                      ))

(define celda20 (new text-field%
                      (label "")
                      (parent column2)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
                      ))
(define celda21 (new text-field%
                      (label "")
                      (parent column2)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
                      ))
(define celda22 (new text-field%
                      (label "")
                      (parent column2)
                      (init-value "")
                      [min-width cell_width]	 
                      [min-height cell_height]
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




