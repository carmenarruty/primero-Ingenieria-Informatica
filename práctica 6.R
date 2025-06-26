#________________________PRÁCTICA 6: CONTRASTE DE HIPÓTESIS____________

################################
#--------Ejercicio 1------

#El profesorado de la materia de Estadística del Grado en Ingeniería Informática han
#decidido hacer una encuesta entre su alumnado permitiéndoles escoger si preferirían ser evaluados
#a través de una prueba final de carácter oral o escrita. De los/las matriculados/as, 72 han respondido
#a la encuesta y de ellos/as 41 han manifestado su preferencia por la prueba escrita.
#A la vista de los resultados anteriores, ¿se puede afirmar que más de la mitad de los/las alumnos/as 
#prefieren una prueba escrita? Redacta la conclusión del contraste suponiendo que el
#nivel de significación es α = 0.1.

# X = "preferencia por prueba escrita" XeBer(p)
# Ho = p <= 0.5
# Ha = p > 0.5

library(LearningStats)
n= 72
exitos = 41
proportion.test(x = exitos, n = n, p0= 0.5, alternative = "greater", alpha= 0.1)

#alternative puede ser: "less", "greater", "two.sided"

#T_obs pertenece a RR
# p-value > T_obs

#T_obs no pertenece a RR -> no rechazamos Ho. 
#NO existen evidencias estadisticamente significativas de que la mayoria del alumnado prefiera la prueba escrita  



###################################
#-------Ejercicio 2------
#Con objeto de mitigar el daño ocular de las pantallas digitales, se ha realizado un
#estudio sobre la capacidad visual de los/as trabajadores/as que usan estos dispositivos de manera
#continuada durante su jornada laboral. Se ha medido la agudez visual de 10 trabajadores/as, con
#los resultados que figuran a continuación en la escala Snellen que sigue una distribución Normal:

 #                  0.71 0.66 0.64 0.49 0.80 0.67 0.52 0.81 0.55 0.65

#A la vista de los resultados anteriores, ¿se puede afirmar que la puntuación media de los/as 
#trabajadores/as que usan estos dispositivos de manera continuada es superior a 0.55 puntos?
#Redacta la conclusión del contraste suponiendo que el nivel de significación es α = 0.1.


# x = "Agudez visual de los trabajadores en la escala Snellen" XeN(mu, sigma2)
#contraste sobre la media con varianza desconocida 

# Ho: mu <= 0.55
# Ha: mu > 0.55

x = c(0.71, 0.66,0.64, 0.49, 0.80, 0.67, 0.52, 0.81, 0.55, 0.65)

Mean.test(x = x, mu0 = 0.55, alternative = "greater", alpha = 0.1)

#T_obs = 2.93049 
#RR = [1.38303, +∞) 
#p-value = 0.00837

#T_obs pertenece a RR  --> Rechazamos la hipótesis nula 
# p-valor < nivel significancia 

#Conclusion: Existen evidencias estaditicamente significativas para afirmar que la puntuación media
# de los trabajadores es mayor que 0.55


############################################
#-------Ejercicicio 3-------
#El tiempo de vida de los sistemas de ventilación empleados para los supercomputadores 
#del CESGA sigue una distribución Normal. El CESGA ha recogido datos del tiempo de vida de 9 de sus 
#sistemas de ventilación obteniendo los siguientes valores (en años):
  
  #              9.9 8.7 10.2 10.5 9.6 9.2 9.8 10.9 9.8
  
#A la vista de los resultados anteriores, ¿se puede afirmar que la varianza poblacional del tiempo
#de vida de los sistemas de ventilación difiere de 0.5 años2? Redacta la conclusión del contraste
#suponiendo que el nivel de significación es α = 0.01.


# X = "tiempo de vida de los sistemas de ventilación empleados para los supercomputares
# del CESGA" XeN(mu, sigma2)

x = c(9.9, 8.7, 10.2, 10.5, 9.6, 9.2, 9.8, 10.9, 9.8)

#contraste de hipótesis para la varianza 

#Ho: sigma2 = 0.5
#Ha: sigma2 != 0.5

variance.test(x = x, sigma02 = 0.5, alternative = "two.sided", alpha = 0.01)
#T_obs = 6.92444 
#RR = [0, 1.34441] U [21.95495, +∞) 
#p-value = 0.91039 

#T_obs no pertenece a RR --> NO RECHAZAMOS LA HIPÓTESIS NULA (Ho)
# p-value > nivel significancia 

#Conclusión: No exiten evidencias estadísticamente significaticas para afirmar que la varianza 
#poblacional del tiempo de vida de los sistemas de ventilación difiere de 0.5 años^2





###################################
#-------Ejercicio 4----
#Se ha realizado una prueba para evaluar si dos versiones de un algoritmo de búsqueda,
#A y B, tienen el mismo rendimiento en términos de tiempo de ejecución. Se han ejecutado ambas
#versiones de los algoritmos de manera independiente en dos grupos de pruebas y se obtuvieron
#los siguientes resultados en segundos para el tiempo de ejecución de cada prueba:
  
# Algoritmo A: 12.3, 15.4, 21.7, 17.2, 38.8, 42.1, 10.5, 23.3, 35.6, 28.4
# Algoritmo B: 21.2, 18.6, 25.1, 14.7, 52.3, 65.2, 40.8, 43.4, 35.6, 42.0

#Se supone que los tiempos de ejecución de ambos algoritmos siguen una distribución Normal con
#la misma varianza. ¿Podemos afirmar que existen diferencias significativas en los tiempos medios
#de ejecución entre los dos algoritmos a un nivel de significación del 5 %?


#X = "Tiempo de ejecución del algoritmo A" XeN(mu1, sigma2)

#Y = "Tiempo de ejecución del algoritmo B" XeN(mu2, sigma2)

#constraste de hipotesis de la diferencia de medias independientes con varianzas desconocidas pero iguales. 
 

algoritmoA = c(12.3, 15.4, 21.7, 17.2, 38.8, 42.1, 10.5, 23.3, 35.6, 28.4)

algoritmoB = c(21.2, 18.6, 25.1, 14.7, 52.3, 65.2, 40.8, 43.4, 35.6, 42.0)

diffmean.test(x1 = algoritmoA, x2 = algoritmoB, var.equal = T, alpha = 0.05, alternative = "two.sided", paired = F)

#si fuesen dependientes tenemos que añadir paired = T 

#T_obs = -1.82968 
#RR = (-∞, -2.10092] U [2.10092, +∞) 
#p-value = 0.08391 

#T_obs no pertenece a RR --> NO RECHAZAMOS LA HIPÓTESIS NULA 
#p-value > nivel significación

#Conclusión: No existe evidencia estadísticamente significativa para poder afirmar 
# que existen diferencias significativas en los tiempos medios
# de ejecución entre los dos algoritmos

#con el p valor calculado solo es compararlo con el nivel de significancia  

######################################
#-------Ejercicio 5-----
#Para estudiar el impacto de un programa de capacitación en el rendimiento de la red
#de una empresa, se ha realizado el siguiente experimento con 11 técnicos de soporte. Antes de la
#capacitación, se midió el tiempo promedio de respuesta (en milisegundos) para resolver problemas
#de conectividad en la red de cada técnico. Después de un programa intensivo de formación sobre
#redes y troubleshooting, se midió nuevamente el tiempo promedio de respuesta de cada técnico.
#De este modo, se dispone de dos conjuntos de observaciones del tiempo medio de respuesta de
#los técnicos en milisegundos:
  
#  Técnico      1    2    3    4     5    6      7    8    9    10    11
   
#  Previo      68    77   94   73    37   131   77    24   99   629   116
   
#  Posterior   95    90   86   58    47   121   136   65   131   630   104

#Suponiendo que los tiempos de respuesta siguen una distribución Normal, ¿hay pruebas suficientes
#para afirmar que el programa de capacitación ha reducido el tiempo de respuesta de los técnicos a
#un nivel de significación del 1 %?
 

# X = "Tiempo promedio de respuesta previo de cada técnico (miliseg)" XeN(mu1, sigma12)
# Y = "Tiempo promedio de respuesta posterior  de cada técnico (miliseg)" YeN(mu2, sigma22)

 #Contraste de hipótesis para la diferencia de medias de sucesos dependientes 

#Ho: mu1 <= mu2
#Ha: mu1 > mu2 


previo = c(68,77,94,73,37,131,77,24,99,629,116)
posterior = c(95,90,86,58,47,121,136,65,131,630,104)

diffmean.test(x1= previo, x2 = posterior, paired = T, alpha = 0.01, alternative = "greater")

# T_obs = -1.70054 
# RR = [2.76377, +∞) 
# p-value = 0.94007

# T_obs no pertenece a RR --> NO RECHAZAMOS Ho
# p-value > nivel significación

#conclusión: No hay evidencias estadisticamente significativas para afirmar que el 
#programa de capacitación haya reducido el tiempo de respuesta de los técnicos 



#################################
#-------Ejercicio 6------
#En un estudio realizado sobre la seguridad informática, se examinan dos grupos de
#usuarios en función de las condiciones de uso de sus sistemas. El primer grupo está compuesto por
#307 usuarios que realizan su trabajo principalmente en redes no seguras, mientras que el segundo
#grupo está formado por 75 usuarios que trabajan en redes protegidas por sistemas de seguridad
#avanzados. En el grupo de usuarios de redes no seguras, 230 han sido afectados por ataques de
#malware durante el último año. En el grupo de usuarios de redes seguras, 30 fueron víctimas de
#ataques. A un nivel de significación del 1 %, ¿podemos concluir que la prevalencia de ataques de
#textitmalware es significativamente menor entre los usuarios de redes seguras?


# X = "Realizar trabajos en redes no seguras y ser afectados por ataques de malware durante el último año" XeBer(px
# Y = "Realizar trabajos en redes seguras y ser afectados por ataques de malware durante el último año" YeBer(py)

# Ho: px <= py  
# Ha: px > py     

nx = 307
ny = 75

exitosx = 230 
exitosy = 30

diffproportion.test(x1 = exitosx, x2 = exitosy, n1 = nx, n2 = ny, alternative = "greater", alpha= 0.01)

#T_obs = 5.81462 
#RR = [2.32635, +∞) 
#p-value = 0 

#T_obs  pertenece a RR    -----> RECHAZAMOS Ho
#p-value < nivel de significación 

#Conclusión: Exiten evidencias estadísticamente significativas para afirmar que 
# la prevalencia de ataques de textitmalware es significativamente menor entre los usuarios de redes seguras.


##########################################
#-------Ejercicio 7------
#Una empresa de tecnología está comparando la estabilidad de dos versiones de su
#software de gestión de redes en función de si hay una diferencia significativa en la variabilidad del
#uso de la CPU. Se han realizado pruebas midiendo el porcentaje de uso de la CPU durante un
#periodo de 30 minutos obteniendo:

# Versión A (uso de CPU en %): 34.5, 36.1, 35.9, 33.4, 34.8, 36.2, 35.6, 34.7, 35.0, 34.3, 35.1, 34.9
# Versión B (uso de CPU en %): 27.8, 28.3, 29.0, 28.5, 28.2, 29.3, 28.7, 29.5, 28.8, 28.4, 29.1, 28.6

#Suponiendo que en ambos casos la variable sigue una distribución Normal, ¿es posible concluir
#que las varianzas del uso de la CPU entre las dos versiones de software son significativamente
#diferentes a un nivel de significación del 5 %?


# X = "Uso de la CPU A durante 30 mins (%)" XeN(mux, sigmax2)
# Y = "Uso de la CPU B durante 30 mins (%)" YeN (muy, sigmay2)

versionA = c(34.5, 36.1, 35.9, 33.4, 34.8, 36.2, 35.6, 34.7, 35.0, 34.3, 35.1, 34.9)
versionB = c(27.8, 28.3, 29.0, 28.5, 28.2, 29.3, 28.7, 29.5, 28.8, 28.4, 29.1, 28.6)


diffvariance.test(x1 = versionA, x2 = versionB, alternative = "two.sided", alpha = 0.05)

#T_obs = 2.77038 
#RR = [0, 0.28788] U [3.4737, +∞) 
#p-value = 0.10547 

# T_obs no pertenece a la región de rechazo
# p-value > nivel de significación 

# Conclusión: No existen evidencias estadísticamente significativas para poder afirmar que 
#las varianzas del uso de la CPU entre las dos versiones de software son significativamente
# diferentes
