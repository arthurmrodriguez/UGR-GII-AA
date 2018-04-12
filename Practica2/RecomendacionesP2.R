f = function(u,v){

	z=...
	z
}

#Una funcion puede ocultar otra funcion
#Las variables u y v se pueden utilizar 
#dentro de cualquier funcion
f = function(u,v){

	#Las funciones se definen DENTRO de la funcion f
	#y su ambito es el de la funcion F
	dx=function(){

	}
	
	#En vez de funciones tambien se pueden utilizar 
	#expresiones (para sustituir estas funciones)
	dy=function(){

	}

	#Aqui es donde se hace el llamado a las funciones
	c(dx(),dy())
}

#Tecnica del gradiente descendente para encontrar un minimo
#Se aplica sobre funciones convexas: buscamos el W tal que 
#se minimice el Ein=(Wt*X,Y). Dada una coordenada X, usamos
# el gradiente descendente para movernos hacia abajo tal que 
#f(W)=MIN
## (u,v) es el punto de inicio de W, puntos para evaluar inicialmente
## esta funcion
## GradientDescent(wini,mu,threshold)
# donde wini es el vector de pesos inicialmente
# mu : tasa de aprendizaje
# threshold: umbral que tenemos que superar (por debajo)
# max_iter: condicion de parada junto con threshold
# la/las funciones: la funcion, su derivada, etc, etc


# E = expression(exp(-V)
# duv= deriv(E,e("u","v"))
# 
# u=1
# v=1
# z=eval(E)
# w= eval(duv)
# (24.94, 4.96)
# El error será mas pequeña que una cota 10^-4 en vez de 10^-14 
# (ultimo apartado del Ejercicio 1)
