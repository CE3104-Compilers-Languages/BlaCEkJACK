#lang racket/gui

(define frame (new frame% [label "Resultados - ♠ ♥ ♦ ♣"]
                   [min-width 500]	 
                   [min-height 300]))

(define column0 (new horizontal-panel% [parent frame]))
(define column1 (new horizontal-panel% [parent frame]))
(define column2 (new horizontal-panel% [parent frame]))

(define (mostrar_resultado_final conteo_final)

   (cond
     [null? conteo_final (send frame show #t)]
     (else
   ;Nombre
      (new text-field%
                      (label "")
                      (parent column0)
                      (init-value (caar conteo_final)))
   
   ;Puntaje final
      (new text-field%
                      (label "")
                      (parent column1)
                      (init-value (cadar conteo_final)))
  

   ;Estado final
      (new text-field%
                      (label "")
                      (parent column2)
                      (init-value (caddar conteo_final)))
   

   (mostrar_resultado_final (cdr conteo_final))
   )
     )
  
  )





