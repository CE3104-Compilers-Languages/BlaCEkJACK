#lang racket
; Estructuras:
; Carta: es una lista formada por: un valor (integer), char (char) y un palo (string).
; Jugador: es una lista formada por: un nombre (string), plantado (boolean) y cartas (lista de cartas).
; Lista de nombres: 
;Ejemplos para pruebas:
;  Carta:   '(10 #\J "corazones")
;  Jugador:   '("Tom" #t ( (10 #\J "corazones") (6 #\6 "picas") (2 #\2 "diamantes") ))
;  Lista de jugadores: '( ("Tom" #t ( (10 #\J "corazones") (6 #\6 "picas") (2 #\2 "diamantes"))) ("Juan" #t ( (10 #\J "corazones") (10 #\0 "picas") (2 #\2 "diamantes"))) ("David" #t ( (10 #\J "corazones") (3 #\3 "picas") (2 #\2 "diamantes"))) )
;  Lista de nombres: '("Tom" "Juan" "David" "Marco")

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

; ACTUALIZADA
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
  (caddr jugador)
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
; funcion que suma la totalidad de puntos de un jugador
; jugador es una lista formada por nombre (string), plantado (booleano) y mano (lista de cartas)
; retorna un numero entero que representa la totalidad de puntos del jugador en un momento dado
(define (sumar_jug jugador)
  (sumar_cartas (mano_jug jugador))
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
    [(and (equal? (get_plantado jugador) #t) (> (sumar_jug jugador) 21) ) "PERDIO"]
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
    (sumar_cartas (mano_jug(car jugadores)))
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
     #f
     '()
    )
    )
   (asignar_nombres (cdr nombres) lista)
   )
     )
    )
  )