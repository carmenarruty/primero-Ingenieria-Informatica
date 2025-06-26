#################################
#   EXAMEN MARTES 6/05/25 
##################################
# Descarga el archivo datos_examen_martes.txt del Campus Virtual y guárdalo en tu directorio de trabajo.


####EJERCICIO 1###
#En la variable tiempo_espera, filtra los datos por una de las categorías de una variable
#cualitativa nominal. De la muestra resultante, realiza un análisis exploratorio que
#incluya medidas características y dos representaciones gráficas.

datos = read.table("datos_examen_martes.txt", header = T, sep = ";", dec = ",")
head(datos)

tiempo = datos$tiempo_espera

#variables cualitativas nominales: sexo, categoria 

# tiempomujeres = "Tiempo de espera de las mujeres" 

tiempomujeres = tiempo[c(which(datos$Sexo == "M"))]
tiempomujeres

mean(tiempomujeres)#media 
median(tiempomujeres) #mediana 
sd(tiempomujeres) #cuasi desviación-típica 
var(tiempomujeres) #cuasi varianza 
var(tiempomujeres)*((length(tiempomujeres)-1)/length(tiempomujeres)) #varianza  
sqrt(var(tiempomujeres)*((length(tiempomujeres)-1)/length(tiempomujeres))) #desviacion típica 


#representaciones gráficas. 
boxplot(tiempomujeres) #diagrama de cajas 
sort(tiempomujeres)
# tenemos un valor atípico: 9.48
hist(tiempomujeres, main = "Tiempo de espera de las mujeres", xlab = "tiempo de espera") #histograma



#####EJERCICIO 2####
#Sobre la muestra del ejercicio anterior, calcula una medida de posición distinta de
#media y mediana, y una medida de dispersión distinta de varianza y desviación típica.

#MEDIDA DE POSICION: 
quantile(tiempomujeres, probs=c(0.25,0.5,0.75)) 
# viendo el histograma tmb podemos decir que el intervalo modal es el [2,4]

#MEDIDA DE DISPERSIÓN:
#rango muestral 
diff(range(tiempomujeres))
#rango intercuartilico 
IQR(tiempomujeres)


####EJERCICIO 3###
#Ajusta una recta de regresión de la satisfacción sobre el tiempo de espera. Representa
#el diagrama de dispersión con la recta ajustada. Interpreta la pendiente y la
#ordenada en el origen en el contexto del problema. Calcula covarianza y coeficiente
#de correlación e interpreta los valores obtenidos.

#recta de regresión de la satisfacción sobre el tiempo de espera 

#queremos predecir la satisfacción en funcion del tiempo de espera 

satisfaccion = datos$satisfaccion_cliente

mod = lm(satisfaccion~tiempo)
mod

# (Intercept): ordenada en el origen = 9.966       
# tiempo: pendiente = -0.755  

#recta de regresión: satisfacción = -0.755*tiempo + 9.966 
# Esto nos indica una relación inversa 
# por cada unidad de tiempo de espera que se aumenta la satisfacción disminuye 0.755 unidades 
# con una tiempo de espera de 0 la satisfacción es casi de un 10 (9.966)

plot(tiempo, satisfaccion, main = "Diagrama de dispersión")
abline(mod, lwd = 2, col = 2)
#relación lineal inversa 

cov(tiempo, satisfaccion) # = -3.694979 < 0 --> relación inversa 
cor(tiempo, satisfaccion) # -0.891968  
#coeficiente de correlación con valor cercano a -1, por tanto la relación inversa
# entre el tiempo de espera  y la satisfacción es fuerte 




########EJERCICIO 4######
#Una empresa farmacéutica está comparando dos formulaciones diferentes de un medicamento
#para reducir la presión arterial. Los resultados de presión arterial sistólica
#(en mmHg) fueron los siguientes:
# Fármaco A: 8.5, 12.1, 9.7, 10.3, 11.6, 9.2, 10.8, 8.9, 11.4, 10.5
# Fármaco B: 15.4, 13.2, 16.7, 14.9, 12.8, 15.1, 14.3, 16.1
#Asumiendo que la presión arterial sistólica sigue una distribución normal en ambos
#grupos y que las varianzas poblacionales pueden asumirse iguales, calcula e interpreta
#un intervalo de confianza del 99% para la diferencia en la presión arterial sistólica
#media entre los dos grupos.

x = c(8.5, 12.1, 9.7, 10.3, 11.6, 9.2, 10.8, 8.9, 11.4, 10.5)
y = c(15.4, 13.2, 16.7, 14.9, 12.8, 15.1, 14.3, 16.1)  

# X = "Presión Arterial sistólica del Fármaco A" XE N(mux, sigma^2)
# Y = "Presión ARterial sistólica del Fármaco B" YE N(muy, sigma^2)


# las varianzas poblacionales pueden asumirse iguales
# Ic al 99% 
#intervalo de confianza para la diferencia de medias 

library(LearningStats)

diffmean.CI(x1 = x, x2 = y, var.equal = T, conf.level = 0.99)
# Ic = (-6.27074 , -2.75426) 




########EJERCICIO 5#####
#La variable satisfaccion_cliente mide el grado de satisfacción de los usuarios
#con el servicio de atención al cliente recibido. Se cree que la variabilidad de las puntuaciones
#obtenidas se ha incrementado con respecto al valor histórico de desviación típica de 0.75 puntos.
#Asumiendo que la variable satisfacción sigue una distribución Normal, ¿existen pruebas de tal aumento en base a los datos recabados?

#contraste de hipótesis para la varianza 

# satisfacción = "grado de satisfacción de los usuarios con el servicio de atención al cliente" ∈ N(mu, sigma^2)


#Ho: sigma^2 <= 0.75^2
#Ha: sigma^2 > ^2

variance.test(x = satisfaccion, sigma02 =0.75^2, alternative = "greater", alpha = 0.05)
#T_obs = 180.78578 
#RR = [42.55697, +∞) 
#α = 0.05
#p-value = 0 

# T_obs ∈ RR y p-value < α, por lo tanto rechazamos la hipótesis nula (Ho)

#Conclusión: Existen evidencias estadísticamente significativas en esta muestra para afirmar que la 
# variabilidad de las puntuaciones obtenidas se ha incrementado con respecto al valor histórico. 
