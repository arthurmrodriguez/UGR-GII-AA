## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
set.seed(24)

## ----Funcion E(u,v)------------------------------------------------------
## Funcion E(u,v) que recibe dos parametros u y v y calcula el valor de la funcion
## que se nos da en el apartado a) en esos puntos
E = function(u,v){
  e=exp(1)
  valorE= ((u^2) * (e^v) - 2*(v^2) * (e^-u))^2
  valorE
}

## ----Gradiente de E(u,v)-------------------------------------------------
## Funcion GradienteDeE(u,v) que a partir del calculo de las derivadas parciales
## de la funcion E con respecto de u y v, calcula el gradiente de la funcion E
GradienteDeE = function(u,v){
  e=exp(1)
  derivadaParcialRespectoU = 2*((u^2)* (e^v) - 2*(v^2)*(e^-u))*(2*u*(e^v) + 2*(v^2)* (e^-u))
  derivadaParcialRespectoV = 2*((u^2)* (e^v) - 2*(v^2)*(e^-u))*(u^2 *(e^v) - 4*v*(e^-u))
  gradiente = c(derivadaParcialRespectoU,derivadaParcialRespectoV)
  gradiente
}

## ----Algoritmo Gradiente Descendente-------------------------------------
## GradienteDescendente(valoresIniciales,funcion,gradienteFuncion,tasaAprendizaje,cotaError) 
## Este algoritmo calcula el numero de iteraciones necesarias para que una funcion 
## determinada partiendo de unos valoresIniciales (u y v) y siguiendo su gradienteFuncion,
## consiga un error menor que cotaErrorMin o llegue a ejecutar un maximo de iteraciones 
## maxIters siguiendo una tasaDeAprendizaje prefijada. 
## El algoritmo devuelve los valores de U y V tras converger a esa cota de error 
GradienteDescendente = function(valoresIniciales,funcion,gradienteFuncion, 
                                tasaDeAprendizaje, cotaErrorMin, maxIters){
  
  #Declaramos las variables que vamos a utilizar en nuestro algoritmo
  #y calculamos un primer valor de error
  iteraciones = 0
  u = valoresIniciales[1]
  v = valoresIniciales[2]
  W_old = c(u,v)
  cotaError = funcion(W_old[1],W_old[2])

  #Mientras que  error este por encima de la cota minima de error 
  while(cotaError > cotaErrorMin && iteraciones < maxIters){
    
    #Calculamos el vector gradiente
    gradiente = gradienteFuncion(W_old[1],W_old[2])
    vectorGradiente = -gradiente
    
    #Actualizamos los valores de los pesos y calculamos la nueva cota de Error
    #con los valores U y V actualizados (los de W)
    W_new = W_old + (tasaDeAprendizaje*vectorGradiente)
    cotaError = abs(funcion(W_old[1],W_old[2]) - funcion(W_new[1],W_new[2]))
    iteraciones = iteraciones + 1
    
    #Nos quedamos con los pesos de la iteracion anterior
    #ya que seran los que nos serviran para comparar 
    W_old = W_new
  }
  
  #Devolvemos el numero de iteraciones y los valores de U y V finales
  list(iteracionesMax = iteraciones, pesos = W_new)
  
}

## ----Prueba apartado a---------------------------------------------------
resultados1a = GradienteDescendente(c(1,1),E,GradienteDeE,0.1, 10^-4, 30000)
print(sprintf("El algoritmo termina tras %i iteraciones",resultados1a$iteracionesMax))
print(sprintf("Los valores obtenidos son u = %f y v = %f",
              resultados1a$pesos[1],resultados1a$pesos[2]))

## ----Funcion F(x,y)------------------------------------------------------
## Funcion f(x,y) que recibe dos parametros (x e y) y calcula el valor de la funcion
## que se nos da en el apartado b) en esos puntos
f = function(x,y){
  
  valorFuncion = (x-2)^2 + 2*(y-2)^2 + 2*sin(2*pi*x)*sin(2*pi*y)
  
}

## ----Gradiente de f(x,y)-------------------------------------------------
## Funcion GradienteDeF(x,y) que a partir del calculo de las derivadas parciales
## de la funcion F con respecto de x e y, calcula el gradiente de la funcion F
GradienteDeF = function(x,y){

  derivadaParcialRespectoX = 2*(x-2) + 2*cos(2*pi*x)* sin(2*pi*y)*2*pi
  derivadaParcialRespectoY = 4*(y-2) + 2*sin(2*pi*x)* cos(2*pi*y)*2*pi
  
  gradiente = c(derivadaParcialRespectoX,derivadaParcialRespectoY)
  gradiente
}


## ----Algoritmo Gradiente Descendente Apartado B--------------------------
## Realiza la misma funcion que la funcion GradienteDescendente solo que la condicion
## de parada ahora son solo maxIters. El algoritmo devuelve el conjunto de valores (x,y)
## en cada iteracion del mismo.
GradienteDescendenteB = function(valoresIniciales,funcion,gradienteFuncion, 
                                tasaDeAprendizaje, maxIters){
  
  #Declaramos las variables que vamos a utilizar en nuestro algoritmo
  #y calculamos un primer valor de error
  iteraciones = 0
  x = valoresIniciales[1]
  y = valoresIniciales[2]
  W = c(x,y)
  cotaError = funcion(W[1],W[2])
  puntosPorIteracion = numeric(0)
  valoresDeVariables = numeric(0)
  
  #Mientras que  error este por encima de la cota minima de error 
  while(iteraciones < maxIters){
    
    #Calculamos el vector gradiente
    gradiente = gradienteFuncion(W[1],W[2])
    vectorGradiente = -gradiente
    
    #Guardamos el valor de la funcion y los valores (x,y) que
    #me dan esos valores
    puntosPorIteracion = c(puntosPorIteracion,cotaError)
    valoresDeVariables = rbind(valoresDeVariables,matrix(W,1,2,byrow=TRUE))
    
    #Actualizamos los valores de los pesos y calculamos la nueva cota de Error
    #con los valores U y V actualizados (los de W)
    W = W + (tasaDeAprendizaje*vectorGradiente)
    cotaError = funcion(W[1],W[2])
    iteraciones = iteraciones + 1
  }
  
  #Devolvemos el numero de iteraciones y los valores de U y V finales
  list(iteraciones = iteraciones, valoresFinales = W, 
       puntos = puntosPorIteracion, 
       valoresDeVariables = valoresDeVariables)
  
}

## ----Prueba 1 apartado b-------------------------------------------------
## Realizamos las pruebas para comparar con los resultados
## posteriores
resultados1b = GradienteDescendenteB(c(1,1),f,GradienteDeF,0.01, 50)
print(sprintf("El algoritmo termina tras %i iteraciones",
              resultados1b$iteraciones))

print(sprintf("Los valores obtenidos son x = %f e y=%f",
resultados1b$valoresFinales[1],resultados1b$valoresFinales[2]))

plot(1:length(resultados1b$puntos), resultados1b$puntos, xlab="Iteraciones", 
     ylab="Valor de la funcion", col = 4)

## ----Prueba 2 apartado b-------------------------------------------------
## Realizamos las pruebas pertinentes para comparar con los
## resultados anteriores
resultados1b2 = GradienteDescendenteB(c(1,1),f,GradienteDeF,0.1, 50)
print(sprintf("El algoritmo termina tras %i iteraciones",
              resultados1b2$iteraciones))

print(sprintf("Los valores obtenidos son x = %f e y =%f",
resultados1b2$valoresFinales[1],resultados1b2$valoresFinales[2]))

plot(1:length(resultados1b2$puntos), resultados1b2$puntos, 
     xlab="Iteraciones",ylab="Valor de la funcion", col = 2)

## ----Prueba 1 apartado b 2-----------------------------------------------
## Establecemos los 4 puntos de inicio
puntoDeInicio1 = c(2.1,2.1)
puntoDeInicio2 = c(3,3)
puntoDeInicio3 = c(1.5,1.5)
puntoDeInicio4 = c(1,1)

## Calculamos los resultados con los 4 puntos de inicio
resultadosPIni1 = GradienteDescendenteB(puntoDeInicio1,f,GradienteDeF,0.1, 50)
resultadosPIni2 = GradienteDescendenteB(puntoDeInicio2,f,GradienteDeF,0.1, 50)
resultadosPIni3 = GradienteDescendenteB(puntoDeInicio3,f,GradienteDeF,0.1, 50)
resultadosPIni4 = GradienteDescendenteB(puntoDeInicio4,f,GradienteDeF,0.1, 50)

## Calculamos los valores minimos de la funcion a partir de los 4 puntos
## de inicio
minimoPunto1 = min(resultadosPIni1$puntos)
minimoPunto2 = min(resultadosPIni2$puntos)
minimoPunto3 = min(resultadosPIni3$puntos)
minimoPunto4 = min(resultadosPIni4$puntos)

## Ahora calculamos, para cada valor minimo que se ha obtenido segun
## cada punto de inicio, los valores (x,y) que evaluados por la funcion
## f(x,y) nos dan esos mismos valores minimos
pesosPunto1 = which(resultadosPIni1$puntos == minimoPunto1)
pesosPunto2 = which(resultadosPIni2$puntos == minimoPunto2)
pesosPunto3 = which(resultadosPIni3$puntos == minimoPunto3)
pesosPunto4 = which(resultadosPIni4$puntos == minimoPunto4)


## ----Generacion de la tabla de los resultados anterior-------------------

## Primero le damos un formato a nuestra tabla: los puntos de inicio
tablaResultadosEj1b = matrix(c(puntoDeInicio1[],minimoPunto1,
                               resultadosPIni1$valoresDeVariables[pesosPunto1,]),
                             1,5,byrow=T)

tablaResultadosEj1b = rbind(tablaResultadosEj1b,
c(puntoDeInicio2[],minimoPunto2,resultadosPIni2$valoresDeVariables[pesosPunto2,]))

tablaResultadosEj1b = rbind(tablaResultadosEj1b,
c(puntoDeInicio3[],minimoPunto3,resultadosPIni2$valoresDeVariables[pesosPunto3,]))

tablaResultadosEj1b = rbind(tablaResultadosEj1b,
c(puntoDeInicio4[],minimoPunto4, resultadosPIni2$valoresDeVariables[pesosPunto4,]))

colnames(tablaResultadosEj1b)= c("Punto Inicio (x)","Punto Inicio (y)","Valor minimo",
                "Valor de variable (x)","Valor de variable (y)")

library(knitr)
kable( tablaResultadosEj1b , caption = "Resultados Ejercicio 1b",
       align = c('c', 'c', 'c', 'c', 'c'),
       format.args = list( decimal.mark = ".")
)


## ----Simula_unif---------------------------------------------------------

## Funcion simula_unif(N,dim,rango) : por defecto genera 2 puntos entre 
## el intervalo [0,1] de 2 dimensiones 
simula_unif = function (N=2,dim=2, rango = c(0,1)){
  
  m = matrix(runif(N*dim, min=rango[1], max=rango[2]),
             nrow = N, ncol=dim, byrow=T)
  m
}

## ----Simula_recta--------------------------------------------------------
##  Función simula_recta(intervalo) una funcion que calcula los parámetros
##  (Para calcular la recta se simulan las coordenadas de 2 ptos dentro del 
##  de una recta aleatoria, y = ax + b, que corte al cuadrado 
## [-50,50]x[-50,50] y se calcula la recta que pasa por ellos), 
##  se pinta o no segun el valor de parametro visible
simula_recta = function (intervalo = c(-1,1), visible=F){
  
  ptos = simula_unif(2,2,intervalo) # se generan 2 puntos
  a = (ptos[1,2] - ptos[2,2]) / (ptos[1,1]-ptos[2,1]) # calculo de la pendiente
  b = ptos[1,2]-a*ptos[1,1]  # calculo del punto de corte
  
  if (visible) {  # pinta la recta y los 2 puntos
    if (dev.cur()==1) # no esta abierto el dispositivo lo abre con plot
      plot(1, type="n", xlim=intervalo, ylim=intervalo)
    points(ptos,col=3)  #pinta en verde los puntos
    abline(b,a,col=3)   # y la recta
  }
  c(a,b) # devuelve el par pendiente y punto de corte
}


## ----Funcion Generar etiquetas-------------------------------------------
## Funcion generarEtiquetas(muestra,rectaSimulada) 
## para generar las etiqueta teniendo en cuenta los valores de una muestra y 
## una recta simulada siguiendo la función f(x,y)= y - ax - b

generarEtiquetas=function(muestra,rectaSimulada=simula_recta(c(-50,50)) ){
  
  #Comprobamos si tenemos datos de muestra
  if(missing(muestra)) stop("Error: no ha proporcionado un conjunto de muestras")
  
  #Ahora para cada componente de la muestra 2D comprobamos su signo segun
  #la recta que le hemos pasado por parametros
  etiquetas= sign(muestra[,2] - (rectaSimulada[1]*muestra[,1]) -rectaSimulada[2])
  etiquetas
  
}

## ----Calculo de coeficientes de la recta---------------------------------
## Funcion calcularCoeficientes(vini)que calcula los coeficientes 
## de la recta (pendiente y punto de corte)
calcularCoeficientes = function(vini){
  
  #Calculamos la pendiente y el punto de corte según la formula
  # W1*x1 + W2*x2 -bias = 0
  puntoCorte = -vini[3]/vini[2]
  pendiente = -vini[1]/vini[2]
  
  coeficientes=c(puntoCorte,pendiente)
  coeficientes
}


## ----Gradiente Descendente Estocastico (SGD)-----------------------------
## Funcion SGD(datoRL, evaluacionDatoRL,W)
## Calcula la expresion del gradiente que se utiliza en la
## Regresion Logistica: recibe un dato de entrada, la evaluacion
## para ese dato de entrada y el vector de pesos W_t+1
SGD=function(datoRL, evaluacionDatoRL,W){
  
  #Calculamos la expresion total del gradiente
  expresionGradiente = -(evaluacionDatoRL*datoRL)/(1+exp(crossprod((evaluacionDatoRL*t(W)),datoRL)))
  expresionGradiente
}

## ----Algoritmo LGRA------------------------------------------------------
## Algoritmo LGRA_SGD(Wini, datosRL, evaluacionesRL, tasaAprendizaje, 
##                  cotaErrorMin)
## Algoritmo de Regresion Logistica con Gradiente Descendente Estocastico que
## partiendo de unos pesos iniciales Wini, un conjunto de datos datosRL
## y las evaluaciones de los mismos segun una recta generada en
## X= [0,2]x[0,2], calcula el minimo local siguiendo una tasaDeAprendizaje
## fijada cuando llegue a una cota minima de Error.
LGRA_SGD = function(W_antes, datosRL, evaluacionesRL, tasaAprendizaje,cotaErrorMin){
  
  #Declaramos las variables que vamos a utilizar en nuestro algoritmo
  #y calculamos un primer valor de error
  epoca = 0
  W_t = W_antes
  noSuperadaCotaError = F
  
  #Mientras que  error este por encima de la cota minima de error 
  while(!noSuperadaCotaError){
    
    #Iniciamos una epoca y por tanto aplicamos una permutacion aleatoria
    #sobre el conjunto de datos antes de empezar a calcular el gradiente
    permutacion = 1:length(evaluacionesRL)
    permutacion = sample(permutacion)
    
    #Comenzamos la epoca y actualizamos los pesos
    for(i in permutacion){
      
      #Calculamos el vector gradiente con un dato seleccionado
      #de forma aleatoria
      gradiente = SGD(datosRL[i,], evaluacionesRL[i],W_t)
      vectorGradiente = -gradiente
    
      #Actualizamos los valores de los pesos 
      W_t = W_t + (tasaAprendizaje*vectorGradiente)
      
    }
    
    #Finalizamos una época y realizamos la comprobacion
    #en cuanto a la cota de error
    epoca = epoca + 1
    vectorDiferencia = (W_antes - W_t)
    
    if(sqrt(sum((vectorDiferencia)^2)) < cotaErrorMin)
      noSuperadaCotaError = T
    
    #Actualizamos los pesos en cualquier caso
    W_antes = W_t
  }
  
  #Devolvemos el numero de epocas y los valores de W finales
  list(epoca = epoca, valoresFinales = W_t)
  
}

## ----Preparacion para Regresión Logística--------------------------------
set.seed(24)
## El primer paso es generar los dos valores que formaran nuestra recta
## Esto se realiza internamente en la funcion simula_recta, por lo que
## es inmediato obtener la recta que servirá para clasificar nuestros datos
rectaRegresionLogistica = simula_recta(c(0,2))

## Generamos nuestro conjunto de 100 datos en el intervalo [0,2]x[0,2]
datosRegresionLogistica = simula_unif(100,2,c(0,2))
## Evaluamos las respuestas {yn} con respecto de la frontera definida
evaluacionesDatosRL = generarEtiquetas(datosRegresionLogistica,rectaRegresionLogistica)
datosRegresionLogistica = cbind(datosRegresionLogistica,1)



## ----Primera prueba con datos--------------------------------------------
## Primera prueba con 100 datos generados anteriormente
## para obtener unos pesos
Wini = matrix(c(0,0,0),1,3)
tasaAprendizaje = 0.01
cotaErrorMin = 0.01
## Ejecutamos la Regresion Logistica
resultadosRL = LGRA_SGD(Wini,datosRegresionLogistica, evaluacionesDatosRL,
                        tasaAprendizaje, cotaErrorMin)
print(sprintf("El algoritmo acaba tras %i iteraciones",resultadosRL$epoca))
print(sprintf("Los pesos finales son %f, %f y %f",resultadosRL$valoresFinales[1],
              resultadosRL$valoresFinales[2],resultadosRL$valoresFinales[3]))

## Pintamos los resultados para que sean mas visibles
coefsDatosTrain = calcularCoeficientes(resultadosRL$valoresFinales)
plot(datosRegresionLogistica, pch = evaluacionesDatosRL+4, 
     col = evaluacionesDatosRL+4,xlab= "X", ylab = "Y",
     main = "Prueba con una muestra de 100 datos", 
     sub = "En negro: recta original. En azul: recta calculada a partir de los pesos")
abline(rectaRegresionLogistica[2],rectaRegresionLogistica[1])
abline(coefsDatosTrain[1],coefsDatosTrain[2], col = 4)

## ----Ein para la Regresion Logistica-------------------------------------
## Funcion Ein(datosRL, evalucionesRL, W) que permite calcular el Error 
## de una muestra de datos que tienen unas evaluaciones teniendo en
## en cuenta los pesos W que se han calculado con los datos de Train
Ein=function(datos, evaluacionesDatos,W){
  
  #Calculamos la dimension de datos 
  N = dim(datos)[1]
  Ein = 0
  #Para todos los datos
  for(i in 1:N){
    
    #Calculamos el neperiano de la expresion que me define el error
    errorParcial = log(1 + exp((crossprod((-evaluacionesDatos[i]*t(W)),datos[i,]))))
    Ein = Ein + errorParcial
    
  }
  
  Ein = (1/N)*Ein
}

## ----Calculo del Eout----------------------------------------------------
set.seed(24)
## Generar dos nuevos datos: han de tener mas de 999 ejemplos
datos_Eout = simula_unif(1000,2,c(0,2))
## Evaluamos las respuestas {yn} con respecto de la frontera definida
## en el apartado anterior (tenemos en cuenta que las que sean 0, por tanto
## su evaluacion es 0, serán etiquetados como positivas)
evaluacionesDatos_Eout = generarEtiquetas(datos_Eout,rectaRegresionLogistica)
evaluacionesDatos_Eout[evaluacionesDatos_Eout==0]=1

## Calculamos el Eout de la nueva muestra de datos
datos_Eout = cbind(datos_Eout,1)
Eout = Ein(datos_Eout,evaluacionesDatos_Eout,resultadosRL$valoresFinales)
print(sprintf("El Eout estimado para una muestra de %i datos es de %f",
              dim(datos_Eout)[1],Eout))

## Pintamos los resultados para que sean mas visibles
rectaEout= calcularCoeficientes(resultadosRL$valoresFinales)
plot(datos_Eout,col = evaluacionesDatos_Eout + 4, xlab= "X", ylab = "Y",
     main = "Prueba con una muestra distinta de 1000 datos", 
     sub = "En azul: recta calculada a partir de los pesos anteriormente calculados")
abline(rectaEout[1],rectaEout[2])


## ----Funcion Leer y Estructurar Datos------------------------------------
## En una funcion que llamaremos leerDatosYEstructurar(fichero)
## englobaremos el funcionamiento de las lineas de codigo proporcionadas
## para utilizarla sobre distintos ficheros de datos
leerDatosYEstructurar=function(fichero){
  
  # Lectura de fichero datos.train (2780 muestras con 256 valores en escala de gris)
  # o datos.test
  datos = read.table(fichero, quote="\"", comment.char="", stringsAsFactors=FALSE)
  
  ## Nos limitamos a seleccionar las instancias de los numeros
  ## 4 y 8 (aquellos en los que la columna 1 - V1 tengan un 4 u 8) 
  digitos_48 = datos[datos$V1==4 | datos$V1==8,]
  
  digitos = digitos_48[,1]  # nombre de las etiquetas (si es 4 u 8)
  ndigitos = nrow(digitos_48) # numero de muestras de 4 y 8
  
  ## Se retira la clase y se monta una matriz 3D: 432(51 para el train)*16*16
  ## Para cada instancia de 1 o 5, dimesionamos los 256 valores de
  ## escala de gris en una matriz 16x16
  grises = array(unlist(subset(digitos_48,select=-V1)),c(ndigitos,16,16))
  
  ## Eliminamos los dataframes que ya no vamos a utilizar
  rm(datos) 
  rm(digitos_48)
  
  list(datos=grises,labels=digitos)
}

## ----Funcion de simetria-------------------------------------------------
## Funcion fsimetria(A)
fsimetria = function(A){
  A = abs(A-A[,ncol(A):1])
  -mean(A)
}

## ----Algoritmo Perceptron------------------------------------------------
ajusta_PLA = function(datos, label,max_iter=300, vini=c(0,0,0)){
  
  #Comprobamos si tenemos datos
  if(missing(datos)) stop("Error: no ha proporcionado el conjunto de datos")
  
  #Comprobamos si tenemos etiquetas
  if(missing(label)) stop("Error: no ha proporcionado las etiquetas")
  
  #Booleano para controlar si en alguna de las pasadas 
  #completas hemos modificado los pesos
  modificacionPesos=TRUE
 
  #Mientras no superemos el numero de iteraciones
  iteraciones = 0
  
  while(iteraciones < max_iter & modificacionPesos ){
    
    modificacionPesos=FALSE
    vectorDeErroneos =numeric(0)
    
    #Recorremos la matriz de datos, accediendo a cada 
    #fila (vector de características)
    for(i in sample(1:length(datos[,1]))){
      
      #Calculamos el producto escalar
      valor = crossprod(datos[i,],vini)
      
      #Solo modificaremos el vector de pesos cuando 
      # los signos sean distintos (mal clasificado)
      # y lo hacemos con el primero que encontremos
      #dada que la componente de seleccion aleatoria 
      #de los datos permite que todos puedan ser elegidos
      if(sign(valor) != label[i]){
        vini = vini + label[i]*datos[i,]
        break
      }
        
  
    }
    iteraciones=iteraciones+1
    
  }
  
  #Damos como salida los pesos para luego calcular
  # los coeficientes de la recta
  list(pesos = vini,iters = iteraciones)
}


## ----Cálculo de Ein------------------------------------------------------
## Creamos una funcion para obtener el error en funcion del numero de muestras
## mal clasificadas que se llamara calculaErrorMalClasificadas(datos,label,pesos)
calcularErrorMalClasificadas = function(datos,label,pesos){
  
  # Calculamos el producto de los datos por los pesos
  malClasificadas = apply(datos,1,crossprod,pesos)
  
  #Obtenemos cuantas etiquetas han sido mal clasificadas
  numMalClasificadas = sum((sign(malClasificadas)!=label))
  
  #Obtenemos el error
  error = numMalClasificadas/(length(label))

}


## ----Regresion Lineal----------------------------------------------------

## Funcion Regress_Lin(datos, label) 
Regress_Lin = function(datos,label){
  # Calculamos la descomposicion en valores singurales
  # y recuperamos sus valores
  descompValSing = svd(datos)
  D = descompValSing$d
  V = descompValSing$v
  
  #Calculamos la pseudoinversa de D
  pInversaD = diag(1/D)
  
  #Calculamos (X'X)^-1 = V*D^2*V'
  traspuestaXInversa = V%*%(pInversaD^2) %*%t(V)
  
  # Finalmente obtenemos la pseudoinversa
  pseudoInversa = traspuestaXInversa%*%(t(datos))
  
  # Nuestro vector de pesos final será PseudoInvX*label
  W = (pseudoInversa%*%label)
  
}

## ----PLA Pocket----------------------------------------------------------
PLA_Pocket = function(datos, label, pesosRegressLin, max_iter){
  
  #Inicializamos el vector de pesos del Pocket y los del PLA
  W_Pocket = pesosRegressLin
  W_PLA = W_Pocket
  #Evaluamos la calidad de los pesos del pocket (los que
  #nos ofrece la Regresion Lineal)
  EinPocket = calcularErrorMalClasificadas(datos,label,W_Pocket)
  
  for(i in 1:max_iter){
    
    #Ejecutamos UNA iteracion del PLA: este no llega a converger
    #aunque ya parte de una solucion de cierta calidad dado que los
    #pesos son los que me devuelve la Regresion Lineal (sin haber
    #realizado una primera modificacion, garantizando que la solucion
    #no empeore)
    resultadosPLA = ajusta_PLA(datos,label,1,W_PLA)
    W_PLA = resultadosPLA$pesos
    
    #Calculamos los Ein en funcion de los pesos del PLA anterior
    EinPLA = calcularErrorMalClasificadas(datos,label,W_PLA)

    #Si el nuevo vector de Pesos calculado por la ejecucion del PLA
    #es mejor que el actual (W_Pocket) en terminos de Ein, actualizamos
    #los pesos del Pocket
    if(EinPLA<EinPocket){
      
      print(sprintf("Mejora Realizada"))
      EinPocket = EinPLA
      W_Pocket = W_PLA
      
    }
  
  }
  
  #Devolvemos los pesos del Pocket, que son los pesos de la regresion lineal
  #pero a los que se les ha aplicado una mejora sustancial
  W_Pocket
}

## ----Apartado b1 Lectura de los datos------------------------------------
## Leemos los datos de train y test
datosTrain_48 = leerDatosYEstructurar("datos/zip.train")
datosTest_48 =leerDatosYEstructurar("datos/zip.test")

## Mostramos los 4 primeros ejemplos del train
par(mfrow=c(2,2)) 
# for(i in 1:4){
#   imagen = datosTrain_48$datos[i,,16:1] # se rota para verlo bien
#   image(z=imagen)
# }

## Calculamos el grado de simetria y el valor de intensidad media
## para el conjunto de train y hacemos lo propio para el de test
gradoSimetriaTrain = apply(datosTrain_48$datos,1,fsimetria)
intensidadMediaTrain = apply(datosTrain_48$datos,1,mean)

gradoSimetriaTest = apply(datosTest_48$datos,1,fsimetria)
intensidadMediaTest = apply(datosTest_48$datos,1,mean)

## Modificamos las etiquetas de tal forma que los 4s se queden con etiqueta
## 1 y los 8s cambiarán a -1. Lo hacemos para ambos conjuntos de datos
etiquetasTrain=datosTrain_48$labels
etiquetasTest=datosTest_48$labels

etiquetasTrain[etiquetasTrain==4]=1
etiquetasTrain[etiquetasTrain==8]=-1

etiquetasTest[etiquetasTest==4]=1
etiquetasTest[etiquetasTest==8]=-1

## Creacion de la matriz de datos train a partir de la intensidad promedio y el valor medio
datosTrain=matrix(c(intensidadMediaTrain,gradoSimetriaTrain),length(etiquetasTrain),2)
# Anidimos un 1 al final de nuestro vector de caracteristicas
datosTrain=cbind(datosTrain,1)

## Creacion de la matriz de datos test a partir de la intensidad promedio y el valor medio
datosTest=matrix(c(intensidadMediaTest,gradoSimetriaTest),length(etiquetasTest),2)
# Anidimos un 1 al final de nuestro vector de caracteristicas
datosTest=cbind(datosTest,1)


## ----Apartado b1 Experimentacion-----------------------------------------
set.seed(24)
## Ejecutamos la Regresion Lineal para obtener unos pesos iniciales
## que serviran como punto de partida para el PLA Pocket
pesosRL_Train = Regress_Lin(datosTrain,etiquetasTrain)
pesosRL_Train = c(pesosRL_Train[1],pesosRL_Train[2], pesosRL_Train[3])
## Ejecutamos el PLA Pocket para mejorarlo
pesosPLA_Pocket_Train = PLA_Pocket(datosTrain,etiquetasTrain,pesosRL_Train,1000)

## Calculamos los coeficientes de la recta con los 2 pesos obtenidos
## tras ejecutar la Regresion Lineal y tras aplicar la mejora con 
## PLA Pocket
coeficientesRegressLin_Train = calcularCoeficientes(pesosRL_Train)
coeficientesPLA_Pocket_Train = calcularCoeficientes(pesosPLA_Pocket_Train)

## Pintamos los resultados del conjunto de datos de entrenamiento con
## las dos rectas calculadas a partir de los pesos que proporcionaban
## tanto la Regresion Lineal como la mejora PLA Pocket
plot(datosTrain, pch=etiquetasTrain + 2, col = etiquetasTrain + 3, xlab="Intensidad Media",
     ylab = "Grado de Simetria", main="Datos de Entrenamiento",
     sub="En negro: Recta de Regresion Lineal. En verde: recta mejorada por el PLA Pocket")
abline(coeficientesRegressLin_Train[1],coeficientesRegressLin_Train[2])
abline(coeficientesPLA_Pocket_Train[1],coeficientesPLA_Pocket_Train[2],col=3)

## Pintamos los resultados del conjunto de datos de test con
## las dos rectas calculadas a partir de los pesos que proporcionaban
## tanto la Regresion Lineal como la mejora PLA Pocket
plot(datosTest, pch=etiquetasTest + 2, col = etiquetasTest + 3, xlab="Intensidad Media",
     ylab = "Grado de Simetria", main="Datos de Test",
     sub="En negro: Recta de Regresion Lineal. En verde: recta mejorada por el PLA Pocket")
abline(coeficientesRegressLin_Train[1],coeficientesRegressLin_Train[2])
abline(coeficientesPLA_Pocket_Train[1],coeficientesPLA_Pocket_Train[2],col=3)


## ----Calculo del Ein y el ETest------------------------------------------
## Calculamos el Ein y el ETest en funcion de la cantidad de datos mal clasificadas
## Para hacer una comparativa y ver si los resultados que ofrece el Pocket realmente mejoran 
## el porcentaje de clasificacion, vamos a realizar una comparativa entre los pesos
## de la Regresion Lineal y los pesos del Pocket
Ein_48_RegressLin = calcularErrorMalClasificadas(datosTrain,etiquetasTrain,pesosRL_Train)
ETest_48_RegressLin = calcularErrorMalClasificadas(datosTest,etiquetasTest,pesosRL_Train)
print(sprintf("El valor del Ein (en porcentaje) es %f  y el del ETest (en porcentaje) es %f ",
              Ein_48_RegressLin*100 , ETest_48_RegressLin*100))


Ein_48 = calcularErrorMalClasificadas(datosTrain,etiquetasTrain,pesosPLA_Pocket_Train)
ETest_48 = calcularErrorMalClasificadas(datosTest,etiquetasTest,pesosPLA_Pocket_Train)
print(sprintf("El valor del Ein (en porcentaje) es %f  y el del ETest (en porcentaje) es %f ",
              Ein_48*100 , ETest_48*100))


## ----Calculo de una cota verdadera del Eout------------------------------
## Funcion calcularCotaEout(cotaError, tamanioDatos,Dvc, delta)
## Calcula una cota del verdadero Eout en funcion de un determinado
## error (que puede ser tanto Ein como Etest), un tamaño de los datos,
## la dimension de Vapnik-Chervonenkis y el valor delta
calcularCotaEout = function(cotaError, tamanioDatos,Dvc=3, delta=0.05){
  
  N = tamanioDatos
  cotaEout = cotaError + sqrt(8/N * log(4*( ((2*N)^Dvc) +1)/delta))
  cotaEout
  
}

## Calculamos el tamaño de los datos de Train y Test
N_DatosTrain = dim(datosTrain)[1]
N_DatosTest = dim(datosTest)[1]

## Calculamos las cotas de Eout
Eout_Ein = calcularCotaEout(Ein_48,N_DatosTrain)
Eout_Etest = calcularCotaEout(ETest_48,N_DatosTest)
print(sprintf("Las cotas de Eout calculadas son las siguientes"))
print(sprintf("Cota de Eout calculada con el Ein = %f", Eout_Ein))
print(sprintf("Cota de Eout calculada con el Etest = %f", Eout_Etest))


## ----Simula_gauss--------------------------------------------------------
## Función simula_gaus(N, dim, sigma) que genera un
## conjunto de longitud N de vectores de dimensión dim, conteniendo números 
## aleatorios gaussianos de media 0 y varianzas dadas por el vector sigma.
## por defecto genera 2 puntos de 2 dimensiones 
simula_gauss = function(N=2,dim=2,media,sigma){
  
  if (missing(sigma)) stop("Debe dar un vector de varianzas")
  sigma = sqrt(sigma)  # para la generación se usa sd, y no la varianza
  if(dim != length(sigma)) stop ("El numero de varianzas es distinto de la dimensión")
  
  # genera 1 muestra, con las desviaciones especificadas
  simula_gauss1 = function() rnorm(dim,mean= media, sd = sigma) 
  # repite N veces, simula_gauss1 y se hace la traspuesta
  m = t(replicate(N,simula_gauss1())) 
  m
}


## ----Funcion Generar etiquetas RSM---------------------------------------
## Generar etiquetas
generarEtiquetasRegSelMod = function(W_f, datos, sigma=0.5, ruido){
  
    ## Creacion de una funcion anidada que calcule una etiqeuta
    ## Yi a partir de un dato Xi. Esta funcion es crucial para
    ## poder aplicar un "apply" para todos los datos
    funcionWporX = function(Xi){
      Yi = crossprod(t(W_f),Xi) + sigma*ruido
    }
  
    Yn = apply(datos,1,funcionWporX)
}


## ----Weight Decay--------------------------------------------------------
## Funcion Weigth_Decay(datos, etiquetas, lambda)
## Calcula los pesos W_reg en funcion de unos datos de entrada
## un conjunto de etiquetas y un parametro de regularizacion, todo
## esto siguiendo la formula descrita anteriormente
Weight_Decay = function(datos, etiquetas, lambda){
  
  #Nombramos las variables tal y como
  #se hacen en la formula
  Z = datos
  y = etiquetas
  
  #Calculamos la SVD de 
  SVD = svd(t(Z)%*%Z + (lambda* diag(1,ncol(Z))))
  D = SVD$d
  V = SVD$v
  
  #Calculamos la pseudoinversa de D
  pInversaD = diag(1/D)
  
  #Calculamos (X'X)^-1 = V*D^2*V'
  traspuestaXInversa = V%*%(pInversaD^2) %*%t(V)
  
  # Finalmente obtenemos la pseudoinversa
  W = pseudoInversa = traspuestaXInversa%*%(t(Z))%*%etiquetas
  
}

## ----Algoritmo de Regularizacion en la Seleccion de Modelos--------------
## Algortimo RegSeleccionModelos(datos, etiquetas)
## A partir de un conjunto de datos con unas determinadas etiquetas,
## todos ellos generados a partir de una distribucion Gaussiana
## con la media y varianza indicada en los apartados anteriores,
## y considerando un parametro de regularizacion lambda,
## devuelve los errores e1 y e2, asi como el Ecv que luego utilizaremos
## para predecir el Eout
RegSeleccionModelos_CV = function(W_f,ruido, varRuido){
  
  ## Generamos el conjunto de datos inicial y su etiquetado
  ## con el mismo W_f en cada experimento
  datos = simula_gauss(113,3,1,c(1,1,1))
  datos = cbind(datos,1)
  etiquetas = generarEtiquetasRegSelMod(W_f,datos,varRuido,ruido)
  
  #El primer paso consiste en generar un vector de indices
  #que me permitira acceder comodamente a los datos y etiquetas
  indices = 1:nrow(datos)
  indices = sample(indices)
  numeroIteraciones = 11
  E_cv = numeric(0)
  inicio = 1
  fin = 13
  
  for(i in 1:numeroIteraciones){
    
    #En la primera particion, el conjunto de validacion 
    #sera de 13 elementos y el de train de 100
    #La segunda y demas particiones tendran un conjunto de 
    #validacion de 10 elementos y el de train tendra 103
    datos_TrainWD = datos[-indices[inicio:fin],]
    datos_ValWD = datos[indices[inicio:fin],]
    etiquetas_TrainWD = etiquetas[-indices[inicio:fin]]
    etiquetas_ValWD = etiquetas[indices[inicio:fin]]
    
    #Una vez tenemos las particiones tanto de train como
    #de validacion procedemos al calculo de los pesos W_reg
    #para posteriormente validar esos pesos
    N = nrow(datos_TrainWD)
    lambda = 0.05/N
    W_reg = Weight_Decay(datos_TrainWD,etiquetas_TrainWD, lambda)
    #Procedemos a la validacion de los pesos obtenidos con la particion
    #de validacion
    diferenciaMinCuadrados = function(datos, etiquetas, W_reg){
      
      #Calculamos las etiquetas con los pesos obtenidos
      #durante el proceso de aprendizaje
      g_minusDatos = apply(datos,1,crossprod, W_reg)
      
      #Devolvemos el Error cuadratico medio
      mean( (g_minusDatos-etiquetas)*(g_minusDatos-etiquetas))
    }
  
    #Calculamos el error y guardamos el ei
    Error = diferenciaMinCuadrados(datos_ValWD, etiquetas_ValWD,W_reg)
    E_cv = c(E_cv,Error)
    
    inicio = fin+1
    fin = fin+10    
    
  }

  #Devolvemos el E_cv como la media de los errores y los errores e1 y e2  
  c(mean(E_cv),E_cv[1],E_cv[2])
}

## ----Experimentos--------------------------------------------------------
set.seed(24)
## Generamos los pesos con los que realizaremos
## todas las experimentaciones
W_f = simula_gauss(1,4,0,c(1,1,1,1))
W_f[4] = W_f[4]+1
ruido = simula_gauss(1,1,0,1)
varRuido = 0.5

## Ejecutamos la Cross Validation 1000 veces y guardamos todos los
## resultados para obtener los promedios y varianzas de Ecv, E1 y E2
resultados_CrossValidation = replicate(1000,RegSeleccionModelos_CV(W_f,ruido,varRuido))

## ----Obtener resultados--------------------------------------------------
##Calculamos la media y varianza de los Ecv, E1 y E2
media_ECV = mean(resultados_CrossValidation[,1])
varianza_ECV = var(as.vector(resultados_CrossValidation[1,]))
media_E1 = mean(resultados_CrossValidation[2,])
varianza_E1 = var(resultados_CrossValidation[2,])
media_E2 = mean(resultados_CrossValidation[3,])
varianza_E2 = var(resultados_CrossValidation[3,])

print(sprintf("Media de E_cv = %f y varianza de E_cv = %f",media_ECV,varianza_ECV))
print(sprintf("Media de E_1 = %f y varianza de E_1 = %f",media_E1,varianza_E1))
print(sprintf("Media de E_2 = %f y varianza de E_2 = %f",media_E2,varianza_E2))

## ----Algoritmo Coordenada Descendente------------------------------------
## CoordenadaDescendente(valoresIniciales,funcion,gradienteFuncion, 
## tasaDeAprendizaje, cotaErrorMin, maxIters)

CoordenadaDescendente = function(valoresIniciales,funcion,gradienteFuncion, 
                                tasaDeAprendizaje, cotaErrorMin, maxIters){
  
  #Declaramos las variables que vamos a utilizar en nuestro algoritmo
  #y calculamos un primer valor de error
  iteraciones = 0
  u = valoresIniciales[1]
  v = valoresIniciales[2]
  W_old = c(u,v)
  W_new = W_old
  cotaError = funcion(W_old[1],W_old[2])
 
  #Mientras que  error este por encima de la cota minima de error 
  while(cotaError > cotaErrorMin && iteraciones < maxIters){
    
    #Paso 1: Calculamos el vector gradiente con los primeros pesos
    gradiente = gradienteFuncion(W_old[1],W_old[2])
    vectorGradiente = -gradiente
    
    #Paso 1: Actualizamos la primera componente del vector de pesos
    W_new[1] = W_old[1] + (tasaDeAprendizaje*vectorGradiente[1])
    
    #Paso 2: Reevaluamos el gradiente con los pesos actualizados y 
    #modificamos la segunda componente del vector de pesos
    gradiente = gradienteFuncion(W_new[1],W_new[2])
    vectorGradiente = -gradiente
    W_new[2] = W_new[2] + (tasaDeAprendizaje*vectorGradiente[2])
    
    #Calculamos la cota del error una vez hemos realizado los dos pasos
    cotaError = abs(funcion(W_old[1],W_old[2]) - funcion(W_new[1],W_new[2]))
    iteraciones = iteraciones + 1
    #Nos quedamos con los pesos de la iteracion anterior
    #ya que seran los que nos serviran para comparar 
    W_old = W_new
  }
  
  #Devolvemos el numero de iteraciones y los valores de U y V finales
  list(iteracionesMax = iteraciones, pesos = W_new)
  
}

## ----BONUS: Prueba Ejercicio 1-------------------------------------------
resultados1a_BONUS = CoordenadaDescendente(c(1,1),E,GradienteDeE,0.1, 10^-4, 15)
print(sprintf("El algoritmo termina tras %i iteraciones",resultados1a_BONUS$iteracionesMax))
print(sprintf("Los valores obtenidos son u = %f y v = %f",
              resultados1a_BONUS$pesos[1],resultados1a_BONUS$pesos[2]))

## ----BONUS: Comprobacion de E(u,v)---------------------------------------
## Comprobamos el valor de E(u,v) en
## los puntos obtenidos
print(sprintf("El valor de E(u,v) en los puntos u = %f y v = %f es de %f",
              resultados1a_BONUS$pesos[1],resultados1a_BONUS$pesos[2],
              E(resultados1a_BONUS$pesos[1], resultados1a_BONUS$pesos[2]) ))


## ----BONUS: Comparacion CD vs GD-----------------------------------------
resultadosGD = GradienteDescendente(c(1,1),E,GradienteDeE,0.1, 10^-14, 300)
print(sprintf("El Gradiente Descendente (GD) termina tras %i iteraciones",
              resultadosGD$iteracionesMax))
print(sprintf("El valor de E(u,v) en los puntos u = %f y v = %f es de %f",
              resultadosGD$pesos[1],resultadosGD$pesos[2],
              E(resultadosGD$pesos[1], resultadosGD$pesos[2]) ))

resultados1a_BONUS = CoordenadaDescendente(c(1,1),E,GradienteDeE,0.1, 10^-14,300 )
print(sprintf("El algoritmo Coordenada Descendente (CD) termina tras %i iteraciones",
              resultados1a_BONUS$iteracionesMax))
print(sprintf("El valor de E(u,v) en los puntos u = %f y v = %f es de %f",
              resultados1a_BONUS$pesos[1],resultados1a_BONUS$pesos[2],
              E(resultados1a_BONUS$pesos[1], resultados1a_BONUS$pesos[2]) ))

## ----Experimento: 100 veces la RL----------------------------------------
set.seed(24)
## Generamos nuestro conjunto de 100 datos en el intervalo [0,2]x[0,2]
datosRL_Bonus = simula_unif(100,2,c(0,2))
datosRL_Bonus = cbind(datosRL_Bonus,1)

## ----BONUS: primer experimento Ej3---------------------------------------
## Primera prueba con 100 datos generados anteriormente
## para obtener unos pesos
Wini = matrix(c(0,0,0),1,3)
tasaAprendizaje = 0.01
cotaErrorMin = 0.01
## Ejecutamos la Regresion Logistica durante 100 iteraciones 
epocas = numeric(0)
EoutRL_Bonus = numeric(0)
set.seed(24)
for(i in 1:100){
  
  #Generamos unas etiquetas con una funcion frontera distinta cada vez
  rectaRL_Bonus = simula_recta(c(0,2))
  etiquetasRL_Bonus = generarEtiquetas(datosRL_Bonus, rectaRL_Bonus)
  
  #Ejecutamos la RL con un conjunto de N=100 datos
  #y guardamos los pesos y las epocas
  resultadosRL_Bonus = LGRA_SGD(Wini,datosRL_Bonus,etiquetasRL_Bonus,
                                tasaAprendizaje,cotaErrorMin)
  pesosRL_Bonus = resultadosRL_Bonus$valoresFinales
  epocas = c(epocas,resultadosRL_Bonus$epoca)
  
  #Generamos un conjunto de test con N=100 datos para validar
  datosTest_RL_Bonus = simula_unif(100,2,c(0,2))
  datosTest_RL_Bonus = cbind(datosTest_RL_Bonus,1)
  etiquetasTest = generarEtiquetas(datosTest_RL_Bonus,rectaRL_Bonus)
  EoutRL_Bonus = c(EoutRL_Bonus, Ein(datosTest_RL_Bonus,etiquetasTest,pesosRL_Bonus))

}

print(sprintf("El valor de Eout medio para N=100 es de %f",mean(EoutRL_Bonus)))
print(sprintf("El numero medio de epocas que tarda en converger para N=100 es %f",
              mean(epocas)))

