#lang racket/gui

(define frame (new frame% [label "Resultados - ♠ ♥ ♦ ♣"]
                   [min-width 500]	 
                   [min-height 300]))

(define column0 (new horizontal-panel% [parent frame]))
(define column1 (new horizontal-panel% [parent frame]))
(define column2 (new horizontal-panel% [parent frame]))

(define celda00 (new text-field%
                      (label "")
                      (parent column0)
                      (init-value "")))
(define celda01 (new text-field%
                      (label "")
                      (parent column0)
                      (init-value "")))
(define celda02 (new text-field%
                      (label "")
                      (parent column0)
                      (init-value "")))

(define celda10 (new text-field%
                      (label "")
                      (parent column1)
                      (init-value "")))
(define celda11 (new text-field%
                      (label "")
                      (parent column1)
                      (init-value "")))
(define celda12 (new text-field%
                      (label "")
                      (parent column1)
                      (init-value "")))

(define celda20 (new text-field%
                      (label "")
                      (parent column2)
                      (init-value "")))
(define celda21 (new text-field%
                      (label "")
                      (parent column2)
                      (init-value "")))
(define celda22 (new text-field%
                      (label "")
                      (parent column2)
                      (init-value "")))

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




