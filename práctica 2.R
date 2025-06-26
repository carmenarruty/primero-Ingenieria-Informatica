#PRÁCTICA 2: ESTADÍSTICA DESCRIPTIVA. 

##############################################################
#----------EJERCICIO 1. 
#Una empresa de hosting web ha hecho una encuesta de satisfacción entre 20 de sus clientes. 
#Entre otras cuestiones, se preguntaba sobre la calidad del servicio de atención al cliente obteniendo las siguientes respuestas:
#   buena, buena, regular, muy buena, mala, buena, buena, regular, muy mala,
#   buena, muy buena, muy buena, regular, buena, mala, buena, muy buena,
#   muy buena, buena, regular.
# a) Construye la tabla de frecuencias.
# b) Representa gráficamente los datos

    # X="GRADO DE SATISFACCIÓN"
      # VARIABLE ALEATORIA CUALITATIVA ORDINAL

#introducimos los valores en un vector 

muestra = c("b","b", "r", "mb", "m","b","b","r","mm", "b","mb", "mb","r", "b", "m", "b", "mb", "mb", "b","r")
length(muestra)

muestraf= factor(muestra,levels=c("mm","m","r","b","mb")) #converir a factor que es el tipo de variables que R emplea para las variables cualitativas 
# factor permite asignar categorias 
muestraf

#calcular las frecuencias absolutas, absolutas acumuladas, realtivas y relativas acumuladas. 

ni=table(muestraf); ni  #frecuencia absoluta

#frecuencia relativa 
n = length(muestraf)
fi =ni/n; fi

#FRECUENCIAS ABSOLUTAS ACUMULADAS.
Ni=numeric(0) #inicializa un vector que no ocupa en memoria, se va completando posteriormente.
Ni[1]=ni[1]
for (t in 2:5){
  Ni[t]=sum(ni[1:t])
}
Ni
Ni2= cumsum(ni)
Ni2

#FRECUENCIAS RELATIVAS ACUMULAS. 
Fi=Ni/n; Fi
Fi2= cumsum(fi); Fi

#TABLA DE FRECUENCIAS 
tab=cbind(ni,fi,Ni,Fi); tab #tabla por columnas 

barplot(ni, col="purple", border="cyan", space=1.5, main="Diagrama de barras")
pie(ni, col=rainbow(5))
pie(ni,col=terrain.colors(5))
pie(ni, col= c("red","blue","purple","green","yellow"))


######################################################
#---------EJERCICIO 2.
#En el archivo quine (disponible en la librería MASS) se recogen datos de 146 escolares.
#a) Construir las tablas de frecuencias para las variables sexo y edad.
#b) Construye una tabla de contingencia para ambas variables y obtén las distribuciones marginales.
# c) Representa gráficamente los datos de edad.

install.packages("MASS")
library(MASS)
head(quine)
?quine

#Eth: Étnica: Autóctono(A) /No autóctono (N) 
#Sex: F o M  
#Age group: F0= Primary 
#Lrn: nivel educacion AL (average level), SL (slow learner)
#Days: dias de abscinencia de clase en el año 

#...Sexo, variable cualitativa nominal
ni= table(quine$Sex)
n=length(quine$Sex)
fi=ni/n
tab2= cbind(ni,fi); tab2

#Edad. Variable cualitativa ordinal. HACER. 
ni2= table(quine$Age)
n=length(quine$Age)
fi2=ni2/n
tab3= cbind(ni2,fi2); tab3


#TABLA DE CONTINGENCIA: representar datos de dos o mas variables 
tab4=table(quine$Sex,quine$Age); tab4
addmargins(tab4) #distribuciones marginales 

#REPRESENTACIÓN GRÁFICA DE LA EDAD 
barplot(table(quine$Age)) #Diagrama de barras 


###############################################################
#--------EJERCICIO 3.
#En el archivo geyser (disponible en la librería MASS) se recogen datos de duración de
#erupciones y tiempos entre erupciones en el geyser Old Faithfull.
# a) Representa gráficamente los datos de duración de erupciones.
# b) Obtén medidas características para los datos de duración de erupciones

head(geyser)
# X="duración de las erupciones"(seg) cuantitativa continua 
hist(geyser$duration, col="pink", main="HISTOGRAMA CON FRECUENCIAS ABSOLUTAS")
hist(geyser$duration, col="pink", freq=F, main= "HISTOGRAMA DE ÁREA UNO", xlab="Duración (segs)", ylab = "")

boxplot(geyser$duration)

#MEDIDAS CARACTERÍSTICAS (posicion, dispersion, forma)

summary(geyser$duration)
#minimo, primer cuartil, mediana, mediana, 3º cuartil, maximo 

mean(geyser$duration) #media
median(geyser$duration) #mediana
range(geyser$duration)
diff(range(geyser$duration)) #rango

quantile(geyser$duration, probs=c(0.25,0.5,0.75)) 
quantile(geyser$duration, probs=c(0.1))

IQR(geyser$duration) #rango intercuantílico 

var(geyser$duration) #cuasi-varianza 

sc2= var(geyser$duration)*((n-1)/n); sc2#varianza

n=length(geyser$duration)

sd(geyser$duration) #cuasi-desviación típica
ss= sd(geyser$duration)*sqrt(((n-1)/n)); ss


################################################################
#-------EJERCICIO 4.
#Sobre los datos de quine, compara gráficamente los días de absentismo para hombres
# y para mujeres y obtén medidas características para ambos grupos
head(quine)
DaysF=quine$Days[quine$Sex=="F"]
head(DaysF)
DaysM=quine$Days[quine$Sex=="M"]
head(DaysM)

# x= "cualitativa discreta" pero como hay muchos datos la tratamos como cualitativa continua 

#HISTOGRAMA 
par(mfrow=c(1,2))#dividimos la ventana gráfica 
hist(DaysF)
hist(DaysM)

hist(DaysF,ylim = c(0,45), xlim=c(0,90))
hist(DaysM,ylim = c(0,45), xlim= c(0,90))
par(mfrow=c(1,1))

boxplot(quine$Days~quine$Sex)

#medidas características: 



######################################################
#------EJERCICIO 5
#El conjunto de datos airquality contiene información sobre calidad de aire. Entre
#otras variables, se recoge la velocidad de viento (Wind) en millas por hora.
#a) Obtén representaciones gráficas de la variable Wind.
#b) Construye histogramas para 5, 10, 15 y 20 intervalos.
#c) ¿Existen datos atípicos? ¿Qué medidas de localización serían adecuadas?

head(airquality)
viento= airquality$Wind
hist(viento, freq=F)
boxplot(viento, col="blue") 
#hay tres valores atípicos superiores 
#para saber sus valores podemos: sabemos q son los tres valores mas grandes de la muestra 
sort(viento)


# HISTOGRAMAS: 
par(mfrow=c(2,2))
#breaks = num de cortes que hay que hacer para conseguir los intervalos que queremos 
hist(viento,breaks=4) #histograma para 5 intervalos 
hist(viento,breaks=9) #histograma para 10 intervalos 
hist(viento,breaks=14) #histograma para 15 intervalos 
hist(viento,breaks=19) #histograma para 20 intervalos 

par(mfrow=c(1,1))



#########################################################################
#------EJERCICIO PROPUESTO 

#Empleando el conjunto de datos mtcars de la librería datasets que contiene
#información sobre 32 modelos de automóviles con 11 variables relacionadas con su rendimiento y
# características, resuelve los siguientes apartados.

# a) Importa el conjunto de datos mtcars y visualiza las primeras filas.
head(mtcars)
?mtcars

#b) Clasifica las variables del conjunto de datos.

# mpg: millas/galón(US). Variable cuantitativa continua 

# cyl: Número de cilindros. Variable cuantitativa discreta 

# disp: cilindrada(pulgadas cúbicas). Variable cuantitativa continua

# hp: potencia bruta. Variable cuantitativa discreta 

# drat: Relación del eje trasero. Variable cuantitativa continua 

# wt: peso en miles de libras. Variable cuantitativa continua 

# qsec: tiempo en 1/4 de milla. Variable cuantitativa continua 

# vs: motor: 0= V-shaped 1=straight. Variable cualitativa nominal

# am: trasmisión. 0=automatico, 1= manual. Variable cualitativa nominal 

# gear: numero de marchas adelante. Variable cuantitativa discreta 

# carb: numero de carburadores. Variable cuantitativa discreta

#c) Obtén medidas características de las variables con la función summary.

summary(mtcars$mpg)
summary(mtcars$cyl)
summary(mtcars$disp)
summary(mtcars$hp)
summary(mtcars$drat)
summary(mtcars$wt)
summary(mtcars$qsec)
summary(mtcars$vs)
summary(mtcars$am)
summary(mtcars$gear)
summary(mtcars$carb)

summary(mtcars)

apply(mtcars,2,summary)
#1 = por filas 
#2= por columnas

#d) Calcula la media, mediana y moda (si aplica) de la variable mpg (millas por galón).

mean(mtcars$mpg) #media 
median(mtcars$mpg) #mediana
hist(mtcars$mpg) #intervalo modal


#e) Calcula la varianza y la desviación estándar de hp (caballos de fuerza).

s2= var(mtcars$hp)*((n-1)/n) #varianza 
s2

sqrt(s2) #desviación típica

#f) Determina los cuartiles de la variable wt (peso del vehículo).

quantile(mtcars$wt, probs=c(0.25,0.5,0.75)) 

#g) Resume gráficamente la variable mpg.
boxplot(mtcars$mpg)

#h) Resume gráficamente la variable hp y detecta si hay valores atípicos.

#hacemos un histograma porque toma 22 valores diferentes. 
hist(mtcars$hp)
boxplot(mtcars$hp) #si tiene valores atípicos
max(mtcars$hp) #valor atípico

#i) Realiza un diagrama de dispersión entre hp y mpg. 
#¿Qué conclusiones puedes extraer sobre la relación entre el consumo de combustible (mpg) y la potencia del motor (hp)?

plot(mtcars$hp, mtcars$mpg, pch=16, cex= 1.5)

#RELACIÓN INVERSA. 