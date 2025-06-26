
#PRÁCTICA 5. INTRODUCCIÓN A LA INFERENCIA ESTADÍSTICA: Distribuciones en el muestreo, 
#estimación puntual y estimación por intervalos de confianza. 


#______Ejercicio 1_______
# Generar 500 muestras de tamaño n ∈ {10, 20, 100, 500} de X ∈ N(5, 1). 
# Para cada muestra calcular X−μ/(Sc/√n) y comprobar que este estadístico sigue 
# una distribución t de Student con n − 1 grados de libertad.

# μ = 5
# x: media muestral 
# n = tamaño de la muestra
# Sc: cuasi desviación-típica

#Tendremos un Histograma en forma de campana T student 

#guardamos los datos en una matriz en la que cada columna es una muestra.  

# Para n = 10 Tendremos una martiz de 500 columnas y 10 filas 

n = 10 #tamaño muestral 
ns = 500 #número de muestras. 

mu = 5 
sigma = 1

#rnorm(n*ns,mean = mu, sd = sigma) generamos los valores aleatorios  
x = matrix(rnorm(n*ns,mean = mu, sd = sigma), nrow = n, ncol = ns)
x

#calcular la media y desviación de cada muestral
#para cada columna aplicar el comando mean 

media = apply(x,2,mean) #aplicamos el comando de la media a las columnas (2) de x 
media #media de la correspondiente columna/muestra

cuasidesv = apply(x,2,sd)
cuasidesv

estadistico = (media - mu)/(cuasidesv/sqrt(n))
estadistico


#hacer un histograma para ver si ttiene forma de campana

hist(estadistico, freq = F, ylim = c(0, 0.4))

lines(density(estadistico), col = 2, lwd = 2)

#aqui tenemos solo una muestra de tamaño 10 por lo que la aproximacion 


#CÓDIGO PARA CADA TAMAÑO MUESTRAL 

ntotal = c(10, 20, 100, 500)

par(mfrow = c(2,2))

for(i in 1:length(ntotal)){
  x = matrix(rnorm(ntotal[i]*ns,mean = mu, sd = sigma), nrow = ntotal[i], ncol = ns)
  media = apply(x,2,mean)
  cuasidesv = apply(x,2,sd)
  estadistico = (media - mu)/(cuasidesv/sqrt(ntotal[i]))
  hist(estadistico, freq = F, ylim = c(0, 0.5))
  lines(density(estadistico), col = 2, lwd = 2)
  #añadimos la T-student teórica correspondiente para compararla con la aproximación
  xseq = seq(min(estadistico), max(estadistico), len = 1000)
  lines(xseq, dt(xseq, df = ntotal[i] -1,), lwd = 2, col = 4)
}

par(mfrow = c(1,1))



#______Ejercicio 2_____
# En un estudio sobre el tiempo de respuesta de un sistema informático, se ha
# registrado una muestra aleatoria de 10 mediciones (en milisegundos)

                #  12.5 13.2 11.8 12.9 13.5 12.2 11.9 12.7 13.1 12.4

# Sabiendo que los tiempos de respuesta siguen una distribución Normal, obtén un intervalo de
# confianza para el tiempo medio de respuesta a un nivel de confianza del 92 %.


# X = "Tiempo de respuesta de un sistema informático (milisegundos)" X∈ N(μ,σ^2)

# T = X−μ/(Sc/√n) ∈ Tn-1

library(LearningStats)

x = c(12.5, 13.2, 11.8, 12.9, 13.5, 12.2, 11.9, 12.7, 13.1, 12.4) #muestra de los datos 

#comando para calcular un intervalo de confianza para la media 
Mean.CI(x, conf.level = 0.92)
# Intervalo de confianza: (12.26859 , 12.97141) units 

#comando para calcular un intervalo de confianza para la varianza : variance.CI(muestra, nivel de confianza)




#______Ejercicio 3______
# Un equipo de analistas de sistemas desea comparar los tiempos de respuesta
# de dos servidores distintos. Se toman dos muestras aleatorias de tiempos de respuesta (en milisegundos):

# Servidor A: nA = 15, media muestral ¯xA = 120 ms, varianza muestral s2A = 25 ms2.
# Servidor B: nB = 12, media muestral ¯xB = 130 ms, varianza muestral s2B = 30 ms2.

# Asumiendo que los tiempos de respuesta siguen distribuciones normales e independientes,
# construye un intervalo de confianza del 95 % para la diferencia de los tiempos medios de
# respuesta de ambos servidores.

# Xa = "Tiempo de respuesta del servidor A (milisegundos)" Xa ∈ N(μa,σa^2)

# Xb = "Tiempo de respuesta del servidor B (milisegundos)" Xb ∈ N(μb,σb^2)


#Variables independientes. Medias y Varianzas desconocidas. Las varianzas se asumen distintas. 
# Tenemos la media y varianza muestrales pero no las teóricas

nA = 15
nB =12
xA = 120
xB = 130
s2A = 25 
s2B = 30

diffmean.CI(x1 = xA, x2 = xB, n1 = nA, n2 = nB, s1 = sqrt(s2A), s2 = sqrt(s2B), conf.level = 0.95)

# x1 - x2 (xA - xB): Mean of differences estimate: -10 

#Intervalo de confianza para la diferencia de medias:  (-14.43138 , -5.56862) 



#_______Ejercicio 4_____
# Generar 500 muestras de tamaño 100 de una Bi(1, 0.7) y calcular las 500 proporciones muestrales asociadas.
# Dibujar el histograma obtenido a partir de las proporciones calculadas y compararlo con la función de densidad 
# de una distribución N (0.7, (0.7·0.3)/100 ).

#matriz de 500 columnas y 100 filas. Cada columna es una muestra 

#general datos de la binomial rbinom(numero de datos(en este caso 500*100), size = 1, prob = 0.7)


muestras = 500
tam = 100

datos = matrix(rbinom(tam*muestras, size = 1, prob = 0.7), nrow = tam, ncol = muestras)
datos


proporciones = apply(datos, 2, mean) #para las propociones hacemos tamb la media 
proporciones


hist(proporciones, freq = F, ylim = c(0,10))
lines(density(proporciones), col = 2, lwd = 2)


#poner sobre el gráfico la densidad de la normal teórica para compararlas 

xseq = seq(min(proporciones), max(proporciones), len = 1000)
lines(xseq, dnorm(xseq, mean = 0.7, sd = sqrt(0.7*(1-0.7)/tam)), col = 4, lwd = 2)


#_______Ejercicio 5_____
# Una empresa de software ha desarrollado una nueva aplicación móvil y quiere
# estimar la proporción de usuarios que están satisfechos con el producto. Se realiza una encuesta 
# a 200 usuarios seleccionados  aleatoriamente y 150 de ellos indican estar satisfechos con la aplicación. 
# Obtén un intervalo de confianza del 95 % para la proporción poblacional de usuarios satisfechos.

# X = "Estar satisfecho con el producto" X ∈ Ber(p) 

encuestados = 200
satisfechos = 150

proportion.CI(x=satisfechos, n= encuestados , conf.level = 0.95)

# Intervalo de confianza para la proporción = (0.68999 , 0.81001)


#_______Ejercicio 6____
# Un departamento de ciberseguridad está evaluando la efectividad de dos métodos de autenticación en la detección de accesos no autorizados. 
# Se probaron dos sistemas diferentes en condiciones controladas y se registró cuántos accesos sospechosos fueron correctamente
# detectados en cada caso:

# Método 1: se realizaron n1 = 400 intentos de acceso, de los cuales x1 = 280 fueron correctamente detectados como sospechosos.

# Método 2: se realizaron n2 = 350 intentos de acceso, de los cuales x2 = 245 fueron correctamente detectados como sospechosos

# Construye un intervalo de confianza a un nivel de confianza del 95 % para la diferencia de proporciones entre el Método 1 y el Método 2

# X1 = "Ser correctamente detectado por el método 1" X1 ∈ Ber(p) 

# X2 = "Ser correctamente detectados por el método 2" X2 ∈ Ber(p) 

n1 = 400
x1 = 280
n2 = 350
x2 = 245

diffproportion.CI(x1 = x1, x2 = x2, n1 = n1, n2 = n2, conf.level = 0.95)

# Intervalo de confianza para la diferencia de proporciones = (-0.06574 , 0.06574)


#_______Ejercicio 7____
# Generar 500 muestras de tamaño n ∈ {10, 20, 50, 100} de X ∈ N (μ, σ^2) con μ = 3 y σ = 1/2. 
# Calcular el estadístico (n−1)Sc^2 / σ2 para cada una de las 500 muestras y comprobar que sigue una distribución χ2n−1.

numMuestras = 500 

tam = c(10,20,50,100)

#X∈ N (3, 1/2) 

media = 3
sigma = 1/2

for(i in 1:length(tam)){
  x = matrix(rnorm(numMuestras*tam[i], media, sigma), ncol = numMuestras)
  
}

  



#______Ejercicio 8_____
# Un equipo de ingenieros informáticos está evaluando la variabilidad en los tiempos de ejecución de un algoritmo. 
# Se toma una muestra aleatoria de 12 ejecuciones (en segundos), obteniendo los siguientes tiempos:

           #    2.1   2.4   2.0   2.5   2.3   2.2   2.6   2.1   2.3   2.4   2.2   2.5

# Se asume que los tiempos de ejecución siguen una distribución Normal, calcula una estimación puntual de la media y de la varianza 
# y obtén además los correspondientes intervalos de confianza a un nivel de confianza del 97 %

# X = "Tiempo de ejecución de un algortimo (segundos)" X∈N(μ,σ^2)

x = c(2.1,2.4,2.0,2.5,2.3, 2.2, 2.6, 2.1, 2.3, 2.4, 2.2, 2.5)

media = mean(x)
media

#cuasi-varianza 
cuasivar = var(x)
cuasivar

library(LearningStats)
Mean.CI(x = x , conf.level = 0.97)
# Ic =  (2.16637 , 2.43363)

variance.CI(x = x, conf.level = 0.97)
# Ic =  (0.12715 , 0.33613)


#______Ejercicio 9_____
# Generar 500 muestras de tamaño n = 100 de X ∈ N (μX = 0, σx^2 = 1) y 
# 500 muestras de tamaño m = 200 de Y ∈ N (μY = 1, σy^2 = 1/4). 
# Comprobar que   (Sx^2 /σx^2) / (Sy^2/σy^2) sigue una distribución Fn−1,m−1


ns= 500 
n = 100
muX = 0
sigmaX = 1
m = 200
muY = 1
sigmaY = 0.5

x<-matrix(rnorm(n*ns,mean=muX,sd=sigmaX),nrow=n,ncol=ns)

cvarX<-apply(x,2,var) #CALCULAMOS LA CUASI-VARIANZA DE X 

y<-matrix(rnorm(m*ns,mean=muY,sd=sigmaY),nrow=m,ncol=ns)

cvarY<-apply(y,2,var) #CALCULAMOS LA CUASI-VARIANZA DE Y 

est<-(cvarX/sigmaX^2)/(cvarY/sigmaY^2) #CALCULAMOS EL ESTIMADOR

#GRAFICAMOS EL ESTIMADOR 
hist(est,freq=FALSE,col="grey",xlab=" ",ylab=" ",ylim=c(0,3), main="Comparación de varianzas",xlim=c(0.5,2))
lines(density(est),lwd=2,col=2)

#CALCULAMOS Y DIBUJAMOS LA DISTRIBUCIÓN Fn−1,m−1
x<-seq(min(est),max(est),by=0.01)
fx<-df(x,df1=n-1,df2=m-1)
lines(x,fx,lwd=2,col=4)



#______Ejercicio 10____
# En el contexto del Ejercicio 3, construye ahora un intervalo de confianza del 99 % para el cociente de varianzas.


diffvariance.CI(n1 = n, n2 = m, conf.level = 0.99, s1 = 1, s2 = 1/2)

#Ic =  (1.61216 , 2.53074) 






#  EJERCICIOS PROPUESTOS: 

#------Ejercicio 1-----
# Generar 500 muestras de tamaño n (con n ∈ {10, 20, 100, 500}) de X ∈ N (5, 1). 
# Comprobar que la distribución de la media muestral sigue una distribución N (5, 1/n).

muestras = 500
n = c(10, 20, 100, 500)
for (i in 1:length(n)){
  x = matrix(rnorm(n[i]*muestras, mean = 5, sd = 1), nrow = n[i] , ncol = muestras)
  media = apply(x, 2, mean)
  hist(media, freq = F, ylim = c(0,10), xlim = c(4.8, 5.15))
  lines(density(media), col = 2, lwd = 2)
  xseq = seq(min(media), max(media), by = 0.01)
  lines(xseq,dnorm(xseq, mean = 5, sd = sqrt(1/n[i])) , lwd = 2, col = 4)
}


#------Ejercicio 2----
# Se desea estimar el tiempo medio (en horas) que los estudiantes del GrEI dedican semanalmente a desarrollar proyectos de programación. 
# Para ello, se selecciona una muestra aleatoria de 30 estudiantes,  obteniendo una media muestral de 12 horas. 
# Asumiendo que dicho tiempo sigue una distribución Normal con una varianza de 16 horas^2. 
# Obtén un intervalo de confianza para el tiempo medio que dedican semanalmente a proyectos de programación 
# con un nivel de confianza del 95 %.

# X = "tiempo que dedican los estudiantes de GrEI semanalmente 
#        a desarrollar proyectos de programación"

# X ∈ N(mu, 16 horas^2)

n = 30
mumuestral = 12 

Mean.CI(x = 12, sigma = 16, n = 30, conf.level = 0.95)

# Ic =(6.27458 , 17.72542)


#--------Ejercicio 3------ DISTRIBUCION T STUDENT
#Generar 500 muestras de tamaño n ∈ {10, 20, 100, 500} de X ∈ N (5, 1). Para cada
#muestra calcular X−μ Sc/√n y comprobar que este estadístico sigue una distribución
# t de Student con n − 1 grados de libertad.

ns<-500; n<-c(10,20,100,500); mu<-5; sigma<-1
par(mfrow=c(2,2))
for (i in 1:length(n)){
  x<-matrix(rnorm(n[i]*ns,mean=mu,sd=sigma),nrow=n[i],ncol=ns)
  media<-apply(x,2,mean)
  cvar<-apply(x,2,var)
  est<-(media-mu)/sqrt(cvar/n[i])
  hist(est,freq=FALSE,col="grey",xlab=" ",ylab=" ",main=paste("n=",n[i]),
       xlim=c(-6,6))
  lines(density(est),lwd=2,col=2)
  x<-seq(min(est),max(est),by=0.01)
  fx<-dt(x,df=n[i]-1)
  lines(x,fx,lwd=2,col=4)
}


#-------Ejercicio 4-----  DISTRIBUCIÓN CHI-CUADRADO 
# Generar 500 muestras de tamaño n ∈ {5, 10, 20, 50} de X ∈ N (3, 0.52). 
# Calcular el estadístico (n−1)S2c/σ2 para cada una de las 500 muestras y 
# comprobar que sigue una distribución χ2n−1.

ns<-500; n<-c(5,10,20,50); mu<-3; sigma<-0.5
par(mfrow=c(2,2))
for (i in 1:length(n)){
  x<-matrix(rnorm(n[i]*ns,mean=mu,sd=sigma),nrow=n[i],ncol=ns)
  cvar<-apply(x,2,var)
  est<-(n[i]-1)*cvar/sigma^2
  hist(est,freq=FALSE,col="grey",xlab=" ",ylab=" ",main=paste("n=",n[i]),
       xlim=c(0,90))
  lines(density(est),lwd=2,col=2)
  x<-seq(min(est),max(est),by=0.01)
  fx<-dchisq(x,df=n[i]-1)
  lines(x,fx,lwd=2,col=4)
}
par(mfrow=c(1,1))


#------EJERCICIO 5---
#Generar 500 muestras de tamaño 100 de una Bi(1, 0.7) y calcular las 500 proporciones
#muestrales asociadas. Dibujar el histograma obtenido a partir de las proporciones calculadas y
#compararlo con la función de densidad de una distribución N (0.7, 0.7·0.3/100 ).

ns = 500
n = 100 
p = 0.7
x<-matrix(rbinom(n=n*ns,size=1,prob=p),nrow=n,ncol=ns)
prop<-apply(x,2,mean)

hist(prop,freq=FALSE,col="grey",xlab=" ",ylab=" ",main="Proporción muestral", ylim = c(0, 12))
lines(density(prop),lwd=2,col=2)

x = seq(min(prop),max(prop),by=0.01)
fx = dnorm(x,mean=p,sd=sqrt(p*(1-p)/n))
lines(x,fx,lwd=2,col=4)

