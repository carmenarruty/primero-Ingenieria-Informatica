# PRÁCTICA 3. ANÁLISIS DE DATOS Y REGRESIÓN. 

#______Ejercicio 1. Descarga del Campus Virtual el archivo DatosGapminder.csv. Importa los datos a y comprueba
#que todo ha funcionado correctamente con el comando head. Se pide:

#---1º leer fichero de datos
dat=read.table("DatosGapminder.csv", header=TRUE, sep=',', dec=".")
head(dat)
dim(dat)

#a) Resumen gráfico de la variable PCs.
# PCs: variable cuantitativa continua. Histograma y diagrama de cajas

hist(dat$PCs)
boxplot(dat$PCs)

sort(dat$GDP) #ordenar muestra 
val_atipicos= sort(dat$GDP)[173:177] #para ver los valores de los datos atípicos 
val_atipicos
#ver los paises a los que se correspondenlos valores atípicos
dat$Paises[dat$GDP%in%val_atipicos]

#b) Varianza y desviación típica de la variable PCs.
var(dat$PCs)#cuasivarianza 
n=length(dat$PCs)

v= var(dat$PCs)*((n-1)/n)
v
desv=sqrt(v)
desv

sd(dat$PCs) #cuasi-desviacion tipica 
desv1 = sd(dat$PCs)*sqrt(((n-1)/n))
desv1

#c) Cuantiles de orden 0.3, 0.6 y 0.9 de la variable PCs.
quantile(dat$PCs, probs=c(0.3, 0.6,0.9))

#d) Tipificar la variable PCs. Comprobar que la variable tipificada tiene media 0 y varianza 1.
#restar la media y dividir entre la desviación típica 

med = mean(dat$PCs)

PCStip= (dat$PCs-med)/desv
PCStip #muestra de la variable tipificada 

mean(PCStip)
desv_tip = sd(PCStip)*sqrt(((n-1)/n))
desv_tip

#e) Realizar el mismo análisis descrito en los pasos anteriores para la variable GDP. (variable cuantitativa continua)

# -----1. Resumen gráfico de GDP. 
hist(dat$GDP)
boxplot(dat$GDP)
#-----2. Varianza y desviación típica 
l= length(dat$GDP)
v1= var(dat$GDP)*((l-1)/l)
v1
desv2= sqrt(v1)
desv2
#-----3. Cuantiles de orden 0.3, 0.6 y 0.9
quantile(dat$GDP, probs=c(0.3, 0.6, 0.9))
#-----4. Tipificación.

media= mean(dat$GDP)
media 
GDP_tip = (dat$GDP-media)/desv2
GDP_tip

mean(GDP_tip)
de_t= sd(GDP_tip)*sqrt(((l-1)/l))
de_t





#________Ejercicio 2. Estudia la dependencia de la variable GDP(variable y) sobre la variable PCs(variable x), es decir, 
# ¿podríamos explicar el comportamiento de la variable GDP sobre la variable PCs?

#a) Representa un diagrama de dispersión de GDP sobre PCs.
x = dat$PCs
y=dat$GDP
plot(x,y, main= "DIAGRAMA DE DISPERSIÓN") #relación directa con crecimiento exponencial 

#b) Estudia la relación entre el GDP y los PCs mediante la covarianza y el coeficiente de correlación.

cov(x,y) #covarianza 
#signo positivo-> relación directa 

#coeficiente de correlación con valor alto, por tanto la relación directa entre x e y es fuerte 
cor(x,y)

#c) Ajusta la recta de regresión de la variable del GDP sobre PCs. Representa el modelo ajustado en el diagrama de dispersión obtenido en el apartado a).

mod= lm(y~x) #lm: lineal mode
mod
#intercept= ordenada en el origen  
#x = pendiente (coeficiente que acompaña x)

#recta de regresión: y= -3032.6 + 226.6x

summary(mod)
abline(mod, lw=2, col=2)


#d) Obtén el coeficiente de determinación e interpreta el valor obtenido.

#dentro de summary(mod) nos quedamos con Multiple R-squared: 0.5988. Indica que el 60% de la varible y está definida con la variable x 

#e) Representa los residuos del ajuste frente a la variable explicativa (x). ¿Crees que la variabilidad de los residuos (dispersion) presenta una varianza que no depende de la variable explicativa?

res = mod$residuals
res

plot(x, res, pch=16)

abline(h=0)
#vemos que cuando tomamos valores de x mas grandes la variabilidad aumenta (los puntos se separan de la linea horizontal)
#no hay variabilidad constante 
#esto significa que la recta de regresión no es útil 

#f) Calcula predicciones para la variable GDP sabiendo que la variable PCs toma los valores x0 = 5, x1 = 25 y x2 = 75.
#hacer las prediciones: calcular los valores de y que obtenemos al sustituir los valores de x en la recta de regresión. 
#date.frame(nombre de la segunda componente de lm (~x))
predict(mod, newdata= data.frame(x=c(5,25,75)))

#solo podemos hacer predicciones si los x estan en el rango de valores de la variable. Fuera de ese rango no podemos hacer predicciones 
summary(x) #5, 25 y 75 están en el rango de valores por tanto podemos hacer las predicciones 

#g) Calcula y representa de nuevo la recta de regresión de GDP sobre PCs, pero sólo teniendo en cuenta los datos de los países de la Unión Europea. Para facilitar la tarea de selección, a continuación se ofrece una lista que los contiene:
UE=c("Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus","Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany","Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania","Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania","Slovakia", "Slovenia", "Spain", "Sweden")
UE
length(UE)

#creamos un base de datos con los paises que nos interesan 

datUE=dat[dat$Paises%in%UE,] #[filas, columnas] quiero las filas de los paises de UE y todas las columnas por tanto ponemos la coma 
head(datUE)
dim(datUE) #hay 4 paises de UE que no están en la base de datos por tanto la dimension es menor 
setdiff(UE, dat$Paises) #paises en UE que no estan en el campo Pais de la base de datos dat 

xUE= datUE$PCs
yUE= datUE$GDP

plot(xUE, yUE, pch=16)
modUE= lm(yUE~xUE)
modUE
abline(modUE, lwd=2, col=2)

summary(xUE)

# 5 y 25 no están en el rango de valores de xUE por tanto no podemos hacer predicciones de estos valores 

predict(modUE, newdata= data.frame(xUE = c(75)))











#__________Ejercicio 3. En este ejercicio volveremos a trabajar con los datos del geyser Old Faithful que están guardados en la variable geyser de la librería MASS. Recuerda que tienes que cargar dicha librería mediante el comando libray(MASS).
library(MASS)

#En este estudio la variable respuesta es el tiempo de espera entre erupciones (waitings) y la variable explicativa el tiempo de duración de la erupción (eruptions)

#y: tiempo de espera entre erupciones (waitings)
#x: tiempo de duración de la erupcion (duration)
head(geyser)
x= geyser$duration
y= geyser$waiting


#a) Representa un diagrama de dispersión de ambas variables.
plot(x,y)

#b) Estudia la relación lineal entre las dos variables mediante la covarianza y el coeficiente de correlación.

cov(x,y) #negativo -> relación inversa 
cor(x,y) #valor negativo intermedio -> no hay relación muy fuerte 

#c) Ajusta la recta de regresión de Y sobre X. Representa el modelo ajustado en el diagrama de dispersión obtenido en el apartado a).

m = lm(y~x)
m
# recta de regresión: y = 99.31 - 7.80x
 summary(m)
 abline(m,lw=2, col=2 )

#d) Obtén el coeficiente de determinación e interpreta el valor obtenido.

 #dentro de summary(m) cojemos el valor de Multiple R-squared: 0.4155.
 # Este valor nos indica que un 42% de la variabilidad del  tiempo de espera entre erupciones se puede exlicar a partir del tiempo de duracion de las mismas 
 # el modelo explica el 42% de la varabilidad del tiempo de espera entre erupciones a parir de la duración de las erupciones 
#e) Representa los residuos del ajuste frente a la variable explicativa. ¿Crees que la variabilidad de los residuos presenta una varianza que no depende de la variable explicativa?
 
r = m$residuals
r

plot(x, r, pch=16)
abline(h=0)

#la variabilidad de los residuos no crece a medida que crecen los valores de x 
#no hay una homogeniedad perfecta pero tmcp podemos decir que la variabilidad cambia 

#f) Calcula las predicciones para la variable Y sabiendo que la variable X vale x0 = 4.3 y x1 = 2.3. ¿Sería razonable hacer una predicción para x2 = 7?

#no sería razonable hacer una predicción para x=7 porque 7 no pertenece al rango de valores que toma la variable x 

summary(x)
predict(m, newdata= data.frame(x=c(4.3, 2.3)))


