#lang racket

; USAR IN PARA LEER LA ENTRADA DE TEXTO y poner en el archivo txt "destino" "100" etc. 
; 

; ----- Estructuras ------- ;
; Estado: estado en el mapa de estados. (Que no el de ciudades) {camino seguido, coste en km}
(define-struct estado (camino km))
; Arista: unión de una ciudad a otra {ciudadA, ciudadB, coste}
(define-struct arista (origen destino km))


; ----- Lectura de fichero  -------- ; 

(define (transforma-arista lista-leida)
; Dada una línea del archivo de texto, la transforma en dos aristas:
  (list
  ; De destino a origen
  (make-arista 
    (symbol->string (car lista-leida))  ; Ciudad A
    (symbol->string (cadr lista-leida)) ; Ciudad B
    (caddr lista-leida)                 ; Coste
  )
  ; De origen a destino
  (make-arista 
    (symbol->string (cadr lista-leida))  ; Ciudad A
    (symbol->string (car lista-leida)) ; Ciudad B
    (caddr lista-leida)                 ; Coste
  )
  )               
)

(define (leer-grafo-aux entrada)
; Lee el archivo de grafo recursivamente
  (let ([posible-arista (read entrada)])
    (cond
      ; Si he llegado a final de línea, termino:
      [(eof-object? posible-arista) (list)]
      ; Si la siguiente línea es el origen o destino
      ; lo tomo y llamo recursivamente. 
      [(or (equal? posible-arista 'Origen:) (equal? posible-arista 'Destino:))
        (append 
          (list (symbol->string (read entrada)))
          (leer-grafo-aux entrada))]
      ; Si no, creo la arista y llamo recursivamente
      [(append (transforma-arista posible-arista) (leer-grafo-aux entrada))]
    )
  )
)

(define (leer-grafo)
  (define entrada (open-input-file "grafo-ciudades.txt"))
  (leer-grafo-aux entrada)
  ;(close-input-port entrada)
)

; ------- Variables globales ------ ; 

(define estado-inicial (make-estado (list (car (leer-grafo))) 0))
(define objetivo (cadr (leer-grafo)))
(define grafo (cddr (leer-grafo)))


; ------- Algoritmo de búsqueda ------ ; 


;Devuelve los sucesores de un estado, dado un grafo de ciudades
(define (sucesores grafo estado)
  (cond
    ; Si está vacío, devuelve vacío
    [(empty? grafo) empty]
    ; Elif 
    [(or
      ; Si no son iguales el origen y el elemento
      (not (equal? (arista-origen (car grafo)) (car (estado-camino estado))))
      ;O No está el destino en el camino recorrido
      (member (arista-destino (car grafo)) (estado-camino estado)))
     ; Seguir buscando el destino en el resto del camino recorrido
              (sucesores (cdr grafo) estado)]
    [else (cons
           ; Cabeza
           (make-estado
            ; Nuevo camino
            (cons
             ;Cabeza: cabeza del camino (nuevo nodo)
             (arista-destino (car grafo))
             ; Cola: lista que ya teniamos 
             (estado-camino estado))
            ; distancia en km 
            (+ (arista-km (car grafo)) (estado-km estado)))
           ; Cola
          (sucesores (cdr grafo) estado))]
    )
  )


; Devuelve el minimo en la lista de estados
(define (minimo-aux colaestados min)
  (cond
    [(null? colaestados) min]
    [(comparador (car colaestados) min)
     (minimo-aux (cdr colaestados) (car colaestados))]
    [else
     (minimo-aux (cdr colaestados) min)]
    )
  )

 ; Comparador de dos elementos del struct por la distancia en kilometros, devuelve true si el primero es menor que el segundo
(define (comparador elemento1 elemento2)
  (< (estado-km elemento1) (estado-km elemento2))
)

; Esto pilla el minimo de la lista de estados dada
(define (minimo listaestados)
  (if (null? listaestados)
      #f
      (minimo-aux (cdr listaestados) (car listaestados))))

; Genera la lista ordenada de sucesores
(define (cola-de-prioridad lista-sucesores)
  (cond
    [(equal? (length lista-sucesores) 1) car lista-sucesores]
    [else
     (let ([estado-minimo (minimo lista-sucesores)])
           (cons  estado-minimo (cola-de-prioridad (remove estado-minimo lista-sucesores))))]))
  

; ------- Funciones principales ------- ;

(define (anchura estado-actual abiertos)
  ; Abiertos es una cola FIFO. ( Abiertos + sucesores)
  (cond
    ; Si el estado actual es el objetivo, imprimo y pa casa
    [(equal? (car(estado-camino estado-actual)) objetivo)
     (display "\n¡Has llegado a tu destino!\n")
     (imprime-estado estado-actual)]
    ; Si no, pillo sucesor y tiro palante
    [
     ; Imprimo estado
     (display "\nEstado: ")(imprime-estado estado-actual)
     ; Llamo a main-aux tomando el estado más barato y pasando el resto como abiertos
     (anchura (car (append abiertos (sucesores grafo estado-actual)))
           (cdr (append abiertos (sucesores grafo estado-actual))))
    ]
  )
)

(define (profundidad estado-actual abiertos)
  ; Abiertos es una cola LIFO. (Sucesores + Abiertos)
  (cond
    ; Si el estado actual es el objetivo, imprimo y pa casa
    [(equal? (car(estado-camino estado-actual)) objetivo)
     (display "\n¡Has llegado a tu destino!\n")
     (imprime-estado estado-actual)]
    ; Si no, pillo sucesor y tiro palante
    [
     ; Imprimo estado
     (display "\nEstado: ")(imprime-estado estado-actual)
     ; Llamo a main-aux tomando el estado más barato y pasando el resto como abiertos
     (profundidad (car (append (sucesores grafo estado-actual) abiertos))
           (cdr (append (sucesores grafo estado-actual) abiertos)))
    ]
  )
)

(define (optimal estado-actual abiertos)
  ; Abiertos es una cola de prioridad ( menor (sucesores+abiertos))
  (cond
    ; Si el estado actual es el objetivo, imprimo y pa casa
    [(equal? (car(estado-camino estado-actual)) objetivo)
     (display "\n¡Has llegado a tu destino!\n")
     (imprime-estado estado-actual)]
    ; Si no, pillo sucesor y tiro palante
    [
     ; Imprimo estado
     (display "\nEstado: ")(imprime-estado estado-actual)
     ; Llamo a main-aux tomando el estado más barato y pasando el resto como abiertos
     (optimal (car (cola-de-prioridad (append abiertos (sucesores grafo estado-actual))))
           (cdr (cola-de-prioridad (append abiertos (sucesores grafo estado-actual)))))
    ]
  )
)

(define (main)
  (display "\nAlgoritmo optimal (cola de prioridad): \n")
  (optimal estado-inicial (list ))
  (display "\n\nAlgoritmo búsqueda en profundiad (cola LIFO): \n")
  (profundidad estado-inicial (list ))
  (display "\n\nAlgoritmo profundidad (cola FIFO): \n")
  (anchura estado-inicial (list ))
)
  
; ------- Funciones para imprimir cosas ------ ; 

; Imprime un estado
(define (imprime-estado estado)
  (display "Ruta: ")(display "( ")(pintar-lista-invertida (estado-camino estado)) (display ")")
                           (display ", Coste: ") (display (estado-km estado)))

; Imprime la lista de caminos asociada a un estado invertida
(define (pintar-lista-invertida lista)
  (cond
    [(null? (cdr lista)) (display(car lista)) (display " ")]
    [else (pintar-lista-invertida (cdr lista))(display (car lista))(display " ")]
    )
  )


; Imprime una lista de estados 
(define (imprime-estados lista-estados)
  (for-each imprime-estado lista-estados)
)

; Imprime una arista
(define (imprime-arista arista)
  (display (list (arista-origen arista) (arista-destino arista) (arista-km arista))))

; Imprime un grafo
(define (imprime-grafo grafo)
  (cond
    [(empty? grafo)(display "")]
    [(imprime-arista (car grafo)) (display ",") (imprime-grafo (cdr grafo))]
  )
)

