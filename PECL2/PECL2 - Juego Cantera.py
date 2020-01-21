import random

MENOS_INFINITO = float("Inf")*-1
MAS_INFINITO = float("Inf")


class Estado():
    def __init__(self, esMax, estado):
        self.esMax = esMax
        self.estado = estado
        self.sucesores = self.generaSucesores()
        self.esHoja = self.soyHoja()

    def sucesoresAux(self, numero, estado):
        sucesoresTemp = list()
        for i in range(estado[numero]-1):
            estado_temp = list(estado)
            estado_temp[numero] = i+1
            sucesoresTemp.append(Estado(not(self.esMax), (estado_temp)))
        return sucesoresTemp

    def soyHoja(self):
        hoja = True
        for numero in self.estado:
            if(numero != 1):
                hoja = False
        return hoja

    def generaSucesores(self):
        """ Dado un estado, genera una lista de sucesores de primer nivel """
        """ Ej. [3,2,1]: [2,2,1], [1,2,1], [3,1,1] """
        # Calculo cuantos estados sucesores hay
        sucesores = []
        for i in range(len(self.estado)):
            sucesores += self.sucesoresAux(i, self.estado)
        return sucesores

    def __str__(self):
        soy = "Min"
        if(self.esMax):
            soy = "Max"
        return f"{soy}: [{self.estado}; Hoja: {self.esHoja}]"

    __repr__ = __str__

    def poda_alfa_beta(self, alfa, beta, devuelveEstado):
        """ Algoritmo minimax con poda alfa-beta. 
        Si es hoja, devuelve valor.
        Si no es hoja, evalúa y en función de si es Min o Max 
        calcula el mejor resultado posible y devuelve un nodo
        que conduce a ese resultado.
        """

        # Estado devuelve el mejor estado para optimizar.
        estado = None

        # Si soy hoja, devuelvo valor:
        if(self.esHoja and self.esMax):
            return -1
        elif(self.esHoja and not self.esMax):
            return 1

        # Si no soy hoja, calculo el mejor movimiento
        else:
            # Valores iniciales
            mejorEstado = None
            mejorValor = MENOS_INFINITO if self.esMax else MAS_INFINITO

            # Para cada estado sucesor al inicial
            for estado in self.sucesores:
                evaluado = estado.poda_alfa_beta(alfa, beta, False)

                # Si estamos en un nivel del árbol en el que estamos calculando máximos..
                if self.esMax:
                    # El mejor valor es el máximo entre el mejor valor anterior y el nuevo valor evaluado.
                    mejorValor = max(mejorValor, evaluado)
                    # Alfa será el nuevo mejor máximo.
                    alfa = max(alfa, mejorValor)

                    # Si el nuevo nodo evaluado nos da el mejor valor conseguido hasta ahora..
                    if mejorValor == evaluado:
                        mejorEstado = estado  # Será el nuevo estado elegido

                # Si estamos en un nivel del árbol en el que estamos calculando mínimos..
                else:
                    # El mejor valor es el mínimo entre el mejor valor anterior y el nuevo valor evaluado
                    mejorValor = min(mejorValor, evaluado)
                    # Beta será el nuevo valor mínimo.
                    beta = min(beta, mejorValor)

                    # Si el nuevo nodo evaluado nos da el mejor valor conseguido hasta ahora..
                    if mejorValor == evaluado:
                        mejorEstado = estado  # Será el nuevo estado elegido

                if beta <= alfa:    # Si en algún momento beta es menor o igual que alfa..
                    # Dejamos de comprobar el resto de ramas, ya que no habrá valores mejores que los que ya tenemos.
                    break

            if devuelveEstado:
                return mejorValor, mejorEstado
            else:
                return mejorValor


def getDimensionesIniciales():
    """
        Pide las dimensiones iniciales del bloque de mármol
        y comprueba que son válidas antes de continuar
    """
    dimCorrectas = False
    listaDimInt = [0, 0, 0]

    while(not dimCorrectas):
        try:
            print(
                "Introduzca las dimensiones iniciales del bloque de mármol separadas por espacios(Ej: 2 2 2):")
            listaDimInput = input().strip().split()

            if(len(listaDimInput) != 3):
                raise Exception
            else:
                # Compruebo inputs erróneos
                for num in listaDimInput:
                    if(int(num) < 0):
                        raise Exception
                listaBloques = [int(i) for i in listaDimInput]
                dimCorrectas = True
        except:
            print("Error al leer las dimensiones iniciales.\n")

    return listaBloques


def cortarBloque(listaBloques, bloque, corte):
    """
    Corta un bloque determinado por un índice en la lista de bloques
    tanto como especifique el parametro de corte.
    """
    listaBloques[bloque] -= corte


def pedirInput(listaBloques):
    """
    Cuando es el turno del jugador, se le pide un input.
    Esta función pide inputs hasta que se introduzca uno válido.
    """
    inputCorrecto = False

    while(not inputCorrecto):
        try:

            print("Introduzca el bloque a cortar [0 - 2]: ")
            bloque = int(input().strip())
            print("Introduzca el tamaño del corte [1 - (tamaño-1)]: ")
            corte = int(input().strip())

            if bloque not in [0, 1, 2]:
                raise Exception
            elif corte < 1 or corte >= listaBloques[bloque]:
                raise Exception
            else:
                inputCorrecto = True
        except:
            print("Error al leer los parámetros de entrada. Bloque fuera de rango, corte inválido, o el valor introducido no es un número entero.\n")
            print("\n", listaBloques)

    return bloque, corte


def bucleJuego(listaBloques):
    """
    Método que contiene el bucle de juego del programa.
    Empieza sorteando quién empieza jugando y ajustando el algoritmo minimax en función de ello.
    Después, entra en el bucle de juego en el que dependiendo de a quien le toque jugar, se calcula una jugada
    con minimax, o se pide un input al jugador.
    El juego termina cuando se detecta que todos los bloques son 1, es decir, no hay más cortes posibles.
    """
    juegoTerminado = False

    # Se determina quien empieza la partida, (1/True = jugador, 2/False = ordenador)
    turno = False
    turnoStr = ""

    if(random.randint(1, 2) == 1):
        print("Empieza el jugador.\n")
        turno = True
        turnoStr = "Jugador"
        maquina_esMax = True
    else:
        print("Empieza la máquina.\n")
        turnoStr = "Máquina"
        maquina_esMax = False

    while(not juegoTerminado):
        print("Juega: ", turnoStr)
        print(listaBloques)

        if turno:
            bloque, corte = pedirInput(listaBloques)
            print("Bloque: ", bloque, " corte: ", corte, "\n")
            cortarBloque(listaBloques, bloque, corte)
        else:
            e = Estado(maquina_esMax, listaBloques)
            v, estado_siguiente = e.poda_alfa_beta(
                MENOS_INFINITO, MAS_INFINITO, True)
            listaBloques = estado_siguiente.estado

        # Si hay 3 unos en el array de bloques, el juego ha terminado
        if listaBloques.count(1) == 3:
            juegoTerminado = True
        else:
            turno = not turno   # Se pasa el turno al otro jugador
            if turno:
                turnoStr = "Jugador"
            else:
                turnoStr = "Máquina"

    print("Final: ", listaBloques)
    if turno:
        print("Gana el jugador!")
    else:
        print("Gana la máquina!")


listaBloques = getDimensionesIniciales()
bucleJuego(listaBloques)
