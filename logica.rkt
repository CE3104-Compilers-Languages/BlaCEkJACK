#lang racket
; ACTUALIZADA
; funcion que retorna el valor de una carta determinada
; carta es la carta de la cual se quiere obtener su valor
; retorna un numero entero
(define (get_valor carta)
  (car carta)
  )

; ACTUALIZADA
; funcion que retorna el nombre de una carta determinada
; carta es la carta de la cual se quiere obtener su nombre
; retorna un char con el cual se identifica la carta
(define (get_char carta)
  (cadr carta)
  )

; ACTUALIZADA
; funcion que retorna el palo de una carta determinada
; carta es la carta de la cual se quiere obtener su palo
; retorna un string que corresponde al palo de la carta
(define (get_palo carta)
  (caddr carta)
  )

; ACTUALIZADA
; funcion que retorna el nombre de un jugador determinado
; jugador es el jugador del cual se quiere obtener su nombre
; retorna un sring que representa el nombre del jugador
(define (get_nombre jugador)
  (car jugador)
  )

; OBSOLETA - ELIMINAR
; funcion que retorna el estado de plantarse de un jugador determinado
; jugador es el jugador del cual se quiere obtener su estado
; retorna un booleano que representa el estado del jugador
(define (get_plantado jugador)
  (cadr jugador)
  )

; ACTUALIZADA
; funcion que permite obtener la mano (cartas) de un jugador
; jugador es una estructura jugador
; retorna una lista de cartas
(define (mano_jug jugador)
  (cadr jugador)
  )

; ACTUALIZADA
; funcion que suma una mano (cartas) de un jugador
; lista es la lista de cartas en la mano de un jugador
; retorna un numero entero
(define (sumar_cartas lista)
   (cond
     [(null? lista) 0]
     (else
      (+ (get_valor (car lista)) (sumar_cartas (cdr lista)))
      )
         )
  )

; ACTUALIZADA
; funcion que identifica si una carta especifica se encuentra dentro de una lista de cartas determinada
; carta es una carta definida y lista es una lista de cartas
; retorna un valor booleano
(define (carta_miembro carta lista)
  (cond
    [(null? lista) #f]
    [(equal? carta (get_char (car lista))) #t]
    (else
     (carta_miembro carta (cdr lista))
     )
    )
  )
; ACTUALIZADA
; funcion que suma la totalidad de puntos de un jugador
; jugador es una lista formada por nombre (string), plantado (booleano) y mano (lista de cartas)
; retorna un numero entero que representa la totalidad de puntos del jugador en un momento dado
(define (sumar_jug jugador)
  (cond
    [ (and
       (not (menor_igual_21 (sumar_cartas (mano_jug jugador))))
       (carta_miembro "As" (mano_jug jugador))
       )
      (- (sumar_cartas (mano_jug jugador)) 10)
      ]
    (else
     (sumar_cartas (mano_jug jugador))
     )
    )
  )

; ACTUALIZADA
; funcion que determina si un numero es menor o igual a 21
; recibe un numero entero
; retorna un valor booleano segun la entrada recibida
(define (menor_igual_21 n)
  (cond
    [(<= n 21) #t]
  (else #f)
  )
  )


; ACTUALIZADA
; funcion que determina el estado final de juego de un jugador
; recibe un jugador y una puntuacion
; retorna el estado final del jugador en el juego segun una puntuacion de comparacion
(define (estado_final jugador puntuacion)
  (cond
    [(> (sumar_jug jugador) 21) "PERDIO"]
    (else
     (cond
       [(menor_igual_21 puntuacion)
        (cond
          [(>= (sumar_jug jugador) puntuacion) "GANO"]
          (else
           "PERDIO"
           )
          )
        ]
       (else
        "GANO"
        )
       )
     )
    )
  )

; ACTUALIZADA
; funcion que genera el conteo final y determina ganadores y perdedores
#| recibe una lista cuyos elementos son jugadores, la puntuacion del crupier
 y SIEMPRE una lista vacia como tercer parametro|#
; retorna una lista cuyos elementos son listas que contienen el nombre del jugador y los puntos actuales
(define (conteo_final jugadores punt_crupier lista_final)
  (cond
    [(null? jugadores) '()]
    (else
   (append
    (list
    (list
     (get_nombre (car jugadores))
    (sumar_jug (car jugadores))
    (estado_final (car jugadores) punt_crupier)
    )
    )
   (conteo_final (cdr jugadores) punt_crupier lista_final)
   )
   )
  )
  )

; ACTUALIZADA PERO PENDIENTE DE TERMINAR Y DOCUMENTAR
; DEBE SER ADAPTADA
; DEDE SER CONECTADA A LA TABLA
; DEBE LLAMARSE CON CONTEO FINAL COMO PARAMETRO
(define (mostrar_resultado_final conteo_final)
   (cond
     [null? conteo_final 0]
     (else
   ;Nombre
   (caar conteo_final)

   ;Puntaje final
   (cadar conteo_final)

   ;Estado final
   (caddar conteo_final)

   (mostrar_resultado_final (cdr conteo_final))
   )
     )
  )

;ACTUALIZADA
; funcion para crear estructuras jugador predeterminadas a partir de una lista de nombres
; recibe una lista de nombres y como segundo parametro SIEMPRE una lista vacia
; retorna una lista de jugadores inicializados (predeterminados) de la forma (Nombre  #f ())

(define (asignar_nombres nombres lista)
  (cond
    [(null? nombres) '()]
    (else
     (append
    (list
    (list
     (car nombres)
     '()
    )
    )
   (asignar_nombres (cdr nombres) lista)
   )
     )
    )
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

(provide (all-defined-out))

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
(provide get)
(define (get Lista N) (
                    cond ((null? Lista)
                          #f)
                         ((zero? N)
                          (car Lista))
                         (else
                          (get (cdr Lista) (- N 1)))
                         ))

;Método que retorna el largo de una lista
(provide len)
(define (len Lista) (
                     cond ((null? Lista)
                          0)
                          (else
                           (+ 1 (len (cdr Lista))))
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


;Método que añade el crupier a la lista de jugadores
(provide AsignarNombres)
(define (AsignarNombres nombres)
  (cons (list "Crupier" (list))
  (AsignarNombres-aux nombres (list)))
  )

;Método que toma una lista vacía y pone jugadores en la lista
(define (AsignarNombres-aux nombres lista)
  (cond
    [(null? nombres) '()]
    (else
     (append
    (list
    (list
     (car nombres)
     '()
    )
    )
   (AsignarNombres-aux (cdr nombres) lista)
   )
     )
    )
  )

;Toma la lista de jugadores y la ordena según el criterio de "mejor?"
(provide ordenar)
(define (ordenar jugadores) (
                             cond ((null? jugadores)
                                   '())
                                  (else
                                   (let ([i (maximo-aux jugadores 0 0 0 0)])
                                     (cons
                                      (get jugadores i)
                                      (ordenar (delete jugadores i)))
                                     )
                                    )
                                  ))

;Metodo para sacar el indice del jugador con el mayor puntaje
(define (maximo-aux jugadores i i-aux val cartas) (
                            cond((null? jugadores)
                                 i)
                                (else
                                 (cond
                                   ((mejor? (sumar_jugador_aux 0 jugadores 0) val (len (cdar jugadores)) cartas)
                                    (maximo-aux (cdr jugadores) i-aux (+ 1 i-aux) (sumar_jugador_aux 0 jugadores 0) (len (cdar jugadores))))
                                   (else
                                    (maximo-aux (cdr jugadores) i (+ 1 i-aux) val cartas))
                                   )
                                 )
                             ))

;Determina si una mano es superior basado en la suma de sus cartas y la cantidad de cartas en la mano
(define (mejor? sumaJugador sumaCrupier cartasJugador cartasCrupier) (
                      cond
                       ((> sumaJugador 21)
                        #f)
                       ((> sumaCrupier 21)
                        #t)
                       ((> sumaJugador sumaCrupier)
                        #t)
                       ((> sumaCrupier sumaJugador)
                        #f)
                       ((< sumaJugador 21)
                        #f)
                       ((equal? cartasCrupier 2)
                        #f)
                       ((equal? cartasJugador 2)
                        #t)
                       (else
                        #f)
                       ))
