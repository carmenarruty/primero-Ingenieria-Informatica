#####1º INGENIERÍA INFORMÁTICA USC. CURSO 2024/25. 
## ESTADÍSTICA: PRÁCTICA 1 EN RSTUDIO


#utilizar R como calculadora 
2*3 #Para ejecutar en la consola hacemos ctrl + intro

#tipos de datos: numeric (números reales), character(cadenas de caracteres), 
#logical (booleano), factor (subclase del tipo caracter, para variables cualitativas ordinales)
class(3)
class('bac')
class(TRUE)
class(T)
class(FALSE)
class(F)

#unidad elemental: vectores que empiezan desde 1, no desde 0 como en C

#Asignación: x=3 y<-6
x=3
y<-6

#______________________________Práctica 1____________________________

#_____EJERCICIO 1: Operaciones con vectores y matrices.  

#--Apartado a) Construye el vector t=0,0.25,0.5,0.75. 
# Ordénalo de forma decreciente. Calcula el sumatorio de los ti^2 y sin 2πt.

t=c(0,0.25,0.5,0.75)

#t1= seq(from = donde empieza la secuencia, to = donde termiana la secuencia, by = salto entre cada elemento)
# ; para saltar una línea 

t1=seq(from=0, to= 0.75, by=0.25); t1

?seq
t2=seq(0,0.75, len=4); t2

#secuencias de números: 
seq(-5,5,length=5); seq(-5,5,by=2)

#repeticiones 
rep(9,4) #se repite el 9 cuatro veces. 

sort(t) #ordena de menor a mayor  (creciente)

to = sort(t, decreasing=T) #ordenar de mayor a menor (DECRECIENTE)
to

#si hacemos una operación a un vector se hace en todos los elemtos del vector 

tc = t^2
tc

sum(t^2) #suma de los ti^2 
prod(t^2) #producto de los ti^2

#sin(2*pi*t).

sin(2*pi*t) #=sen(0,90,180,270)
# = (0,1,0,-1) 


#si queremos crear un vector sin asignar de salida sus valores: 
y = numeric(12) #12 = tamño vector 
y
z = vector(mode='numeric',length=5)
z

#-----Apartado b)Crea un vector con los datos de ‘Esperanza media de vida en los países de Europa Occidental. 
#Obtén la esperanza de vida media. 
#Toma un subconjunto del vector que muestre únicamente aquellos valores por debajo de la media, y otro subconjunto que muestre aquellos valores por encima de la media. ¿A qué países corresponden? 

esp=c(83.2, 82.5, 83.0, 83.4, 82.4, 81.4, 81.4, 81.8, 81.6)
med = mean(esp) #media 
length(esp)  #tamaño del espacio muestral

esp[3]
esp[1:4] #secuencia de la posición uno hasta la cuatro de uno en uno `
esp[c(3,5,6)] #vector con las posiciones 3,5 y 6 

esp[-5] #todo el vector salvo la posición 5 

esp[-c(1,5)] #todos menos las posiciones 1 y 5 

esp[esp<med]
which(esp<med) #posiciones del vector que cumple que esp es menor que la media 

esp[esp>med]


#----Apartado c) crear las matrices. 
#Calcula su producto matricial y asígnalo a C. 
#Obtén el determinante, la matriz inversa y los autovalores de C. 
#Comprueba que el producto de los autovalores es igual al determinante y la suma de los autovalores es igual a su traza (la suma de la diagonal) 

f1= 1:4
f2= 5:8
f3= 9:12
f4= 13:16
A=rbind(f1,f2,f3,f4) #r de row(filas)
A

B=t(A) #matriz traspuesta 

B1= cbind(f1,f2,f3,f4) #c de columns

#(columnas) 
#podemos asignar nombre a las filas y columnas con colnames(A) = " " // rownames(A) = " "


B2= matrix(1:16, nrow=4, nco =4)

A1 = matrix(1:16, nrow=4, ncol=4, byrow=T)

A+B
A*B #producto elemento a elemento, no es el producto matricial 

C= A%*%B #producto matricial 

det(C) #determinante de C 
#no tiene inversa 

solve(C) #cálculo de inversas

diag(C) #extraer la diagonal de la matriz 

#cálculo de los autovalores y los autovectores. 
eigen(C) #acceso a los autovalores y los autovectores 
eigen(C)$values #solo autovalores 

prod(eigen(C)$values) #determinante 

# traza = suma de los elementos de la diagonal de una matriz = suma de los autovalores 

sum(eigen(C)$values)



#____________EJERCICIO 2: Crea una lista que contenga la siguiente información sobre una familia: 
#Nombre de los padres: Juan y Josefina 
#Número de hijos: 3 
#Edad de los hijos: 16,11 4.
# REsultados de una encuesta de satisfacción: Valoración de 0 a 10 de la zona como
#enclave turístico y del hotel: padre (8 y 3), madre (7 y 5), hijo 1 (5 y 10), hijo 2 (4 y 7), hijo 3 (4 y 7).

familia = list(padres=c("Juan", "Josefina"), numhijos=3, edad= c(16,11,4), encuesta= matrix(c(8,7,5,4,4,3,5,10,7,7), nrow=5, ncol=2))
familia$numhijos #acceso a elementos de la lista por nombre 
familia[[3]] #acceso a elementos de la lista por posición 



#_________EJERCICIO 3: La siguiente tabla muestra el área (en hectáreas) de una serie de campings y su
# ocupación (en número de turistas). Dibuja el diagrama de dispersión de la variable Hectáreas (eje X) frente a Ocupación (eje Y ).
# Supongamos despues que los tres primeros campings pertenecen a zonas de la costa peninsular,
#el cuarto, el sexto y el último al interior peninsular, y los demás, a una zona insular. Crea
#una variable llamada Zona conteniendo esta información. Repite el diagrama de dispersión,
# utilizando distintos colores para cada una de las zonas.

datos= data.frame(hec=c(0.8,1.1, 0.6, 3.2, 1.4, 2.1, 1.5, 0.2, 1.0, 6.1), ocu=c(1200, 650, 1060, 235, 1300, 300, 750, 150, 440, 500))
datos
datos[2,2] #dato de la fila dos columna 2. 

datos[1:3, 1:2]
datos[1:3,]
datos$hec
datos$ocu

#DIAGRAMA DE DISPERSIÓN
plot(datos$hec, datos$ocu)
plot(datos$hec, datos$ocu, main="DIAGRAMA DE DISPERSIÓN")
plot(datos$hec, datos$ocu, main="DIAGRAMA DE DISPERSIÓN", xlab= "Área (hectáreas)", ylab= "Ocupación(num turistas)")
plot(datos$hec, datos$ocu, main="DIAGRAMA DE DISPERSIÓN", xlab= "Área (hectáreas)", ylab= "Ocupación(num turistas)", pch=16, col="purple")
plot(datos$hec, datos$ocu, main="DIAGRAMA DE DISPERSIÓN", xlab= "Área (hectáreas)", ylab= "Ocupación(num turistas)", pch=20, col="#2ca25f",cex=2)

datos$zona=c(1,1,1,2,3,2,3,3,3,2) #1=costa (verde), 2= interior(negro), 3= islas(rojo) 

plot(datos$hec, datos$ocu, main="DIAGRAMA DE DISPERSIÓN", xlab= "Área (hectáreas)", ylab= "Ocupación(num turistas)", pch=16, col=datos$zona)



#__________EJERCICIO 4. Crear un programa que escoja cinco letras del abecedario al azar y forme una palabra uniéndolas

palabra = function(){
  aux=sample(letters, size=5) #elige 5 letras al azar
  res=paste(aux, collapse="") #une las letras 
  return(res)
}
palabra()

palabra1= function(n) {   #FUNCIÓN PARA CUALQUIER NUM DE LETRAS 
  aux= sample(letters, size=n, replace = T)
  res= paste(aux, collapse="") 
  return(res)
}
palabra1(37)
#paste concatena los argumentos en secuencia de caracteres 


#______EJERCICIO 5: INSTALACIÓN DE PAQUETES.

#1. intalar paquetes install.packages("nombrepaquete)
#2. cargar paquete: library(nombrepaquete)

install.packages("LearningStats")
library(LearningStats)


#________EJERCICIO 6: IMPORTAR INFORMACIÓN. 

setwd("C:/Users/carme/OneDrive - Universidade de Santiago de Compostela/INGENIERÍA INFORMÁTICA/1º/SEGUNDO CUATRI/ESTADÍSTICA/PRÁCTICAS/1. Introducción a R")

#pasar archivos a txt y cvs los archivos para leer los archivos mejor. 

# read.table("nombrearchivo.extensión", header, sep, dec)
#header = True si los datos tienen encabezado // False (si la primera linea ya son datos)
#sep= " simbolo de separación de los datos" 
# dec=  como se denotan los números decimales, en este caso, el símbolo decimal es un punto 

dat=read.table("ige.csv",header= T, sep= ",", dec="." )
dat
head(dat)



#COMANDOS PARA GRÁFICAS: 

#plot --> gráfico de dispersión // diagrama de caja(boxplot)
# hist --> histograma 

# points --> añadir a un gráfico uno o mas puntos especificando sus coordenadas
# pch -->  para que el usuario indique cómo quiere señalar tales puntos (círculo sólido o hueco, diamante, triángulo, . . . )
# lines -->  permite añadir segmentos lineales a un gráfico preexistente, indicando las coordenadas de los puntos a unir
# abline --> e permite incluir rectas dentro de un gráfico, indicando su vector director y el término independiente.
# legend, que permite incluir una leyenda en un gráfico
# title --> añadir títulos y subtítulos a un gráfico ya creado