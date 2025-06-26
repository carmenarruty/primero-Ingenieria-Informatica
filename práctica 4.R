#-------------PRÁCTICA 4--------------.

#____Ejercicio 1. Supongamos que lanzamos una moneda 3 veces, y anotamos el número de caras
#que salen. Es decir, la variable que estudiamos es X = “número de caras en 3 lanzamientos
#de una moneda”. Representa su masa de probabilidad y su función de distribución

x=c(0,1,2,3) #0:3
px=c(1/8, 3/8, 3/8, 1/8)
plot(x, px, type="h", lwd=4, main="Función de masa de probabilidad")

Fx=cumsum(px); px

plot(x, Fx, ylim=c(0,1), xlim=c(-1,4), main="Función de distribución.")
segments(-1,0,0,0) #para pintar linea que une los puntos 
segments(3,1,4,1)
segments(x, Fx,1:4,Fx)#puntos, probablilidades acumuladas, desde donde va hasta donde, intervalo 


#_____Ejercicio 2. Un jugador paga 5 euros por jugar a una ruleta donde los
#posibles resultados son 1, 5 y 30 euros, con probabilidades 0.5, 0.45 y 0.05. El retorno es el
#premio que recibe el jugador en euros. ¿Es un juego justo? (por juego justo, entendemos que
#el retorno medio es igual a lo que aporta el jugador). Calcula la varianza de la ganancia.
# (si el juego es justo la esperanza es q no pierdas dinero)

#xi: 1,5,30
#pi: 0.5, 0.45, 0.05

xi=c(1,5,30)
pi=c(0.5, 0.45, 0.05)

media= sum(xi*pi)
media #el juego no es justo 

#Y= "GANANCIA" Y=x-5

yi=xi-5
yi

#los pi de las yi son las mismas que las de xi 

varianza = sum(yi^2*pi)-(media-5)^2 #tamb vale sum(yi^2*pi)-(sum(yi*pi))^2
varianza

sqrt(varianza) #variabilidad de 6 euros 


#_______Ejercicio 3.  Consideremos una variable aleatoria continua X con función de densidad:

#f(x)= k(1-x^2) si -1<=x<=1. 0 en otro caso 

#a) ¿Cuál es el valor de k para que sea densidad? ¿Cuál es su función de distribución?
#para que f sea función de densidad la integral de f(x)=1 y f(x)=>0. 

#parábola cóncava.  Para que f>=0 k tiene que ser mayor o igual que cero 
#integramos entre -1 y 1 

# k = 3/4 
#R hace integración númerica, no simbólica por eso hay que hayar k a mano 


#b) Dibuja la función de densidad y la función de distribución. ¿Cuál es la moda?

par(mfrow=c(1,2)) #para pintar las dos gráficas juntas. 
fdens= function(x){3/4*(1-x^2)}
Fdist= function(x){-x^3/4+3/4*x+1/2}
x= seq(-1,1, by=0.01)
plot(x,fdens(x), type="l", main= "Función de densidad") #type l une los puntos con una línea.
segments(-1,0,-2,0)
segments(1,0, 2,0)

plot(x, Fdist(x), type= "l", xlim=c(-2,2), main="Función de distribución")
segments(-1,0,-2,0)
segments(1,1, 2,1)
par(mfrow=c(1,1)) #para quitar lo de pintar las dos gráficas juntas. 

#MODA. en una distribución normal estándar:  media = mediana = moda = 0 pq es el punto en el que la función alcanza el máximo absoluto 

# la moda es 0 pq ser el valor en el que la función de densidad alcanza su máximo 

#c) Calcula P(−0.5 ≤ X ≤ 1). Calcula la media y la varianza.

#función de distribucion en 1 menos la función de distribución de en -0.5

Fdist(1)-Fdist(-0.5)

#o tmp podemos integrar la función de densidad f(x)
integrate(fdens, lower=-0.5, upper= 1)
integrate(fdens, lower=-0.5, upper= 1) $abs.error
#integración simbólica: lo que hacemos 
#integración numérica: se usa cuando no existe una expresión implícita de la integral. Es una aproximación numérica de la integral (integración por rectángulos o por trapecios) 

#media. integral de x por f(x)

fmed=function(x){x*fdens(x)}
med = integrate(fmed, lower=-1, upper=1) $value


#varianza
fmed2= function(x){x^2*fdens(x)}
var= integrate(fmed2, lower=-1, upper= 1) $value-med^2
var
var


#________Ejercicio 4. 

##BINOMIAL (n,p)

#comandos que hay que conocer para variables alatorias discretas 
#función de masa (d) dbinom(x, size=n , prob=p)
#función de distribución (p) pbinom
#fnción cuantil (q) qbinom
# generar valores (r) rbinom


#a) Calcula la probabilidad de que P(X = 1), con X ∈ Bi(4, 0.75).

#P(Bi(4, 0.75)=1)
dbinom(1, size=4, prob=0.75)


#b) Genera 5 valores de una Bi(6, 0.3).

rbinom(5,size=5, prob=0.3)

#c) Calcula la probabilidad de que P(X ≤ 4), con X ∈ Bi(6, 0.3).

#Pi(Bi(6,0.3)<=4)
pbinom(4, size=6, prob=0.3)

#d) Calcula la probabilidad de que P(3 ≤ X ≤ 5), con X ∈ Bi(6, 0.3).

#Pi(3<= Bi(6, 0.3)<= 5)= Pi(Bi(6, 0.3)=3)+Pi(Bi(6, 0.3)=4)+Pi(Bi(6, 0.3)=5)= 
# Pi(Bi(6, 0.3)<=5)-P(Bi(6, 0.3)<=2)

pbinom(5, size=6, prob=0.3)- pbinom(2, size=6, prob=0.3)

sum(dbinom(3:5, size=6, prob=0.3))

#e) Genera 4 valores de una BN (8, 0.4).

rnbinom(4, size=8, prob=0.4)

#f) Si la probabilidad de éxito es 0.65, calcula la probabilidad de tener 3 fracasos antes del
#primer éxito. ¿Cuál es la probabilidad de tener 3, 2, 1 o ningún fracaso antes del primer éxito?

# x= "num de fracasos hasta el primer éxito " distribución geométrica 

#x Xeo(0.65)

dnbinom(3, size=1, prob= 0.65)


#g) Dibuja la función de masa de probabilidad y la función de distribución de una Pois(1).

#X= "num de sucesos en un periodo de tiempo" 
#los xi van de 0 a infinito pero no vamos a hacer infinitos. Si tenemos que de media hay 1 suceso no vamos a pintar hasta 100 ya que a partir d un valor la probabilidad a partir de ahí será practicamente 0 
x=0:8
fmasa= dpois(x,lambda=1) 
fmasa

plot(x,fmasa, type="h", lwd=4, main="Función de masa de probabilidad")

Fx= ppois(x, lambda=1)
plot(x, Fx, type="s", main="Función de distribución")

#h) Compara la distribución anterior con la de una Bi(100, 0.01)
#son practicamente iguales 
xrej= 0:10
pbin= dbinom(xrej, size=100, prob=0.01)
par(mfrow=c(1,2))
plot(xrej, pbin, type="h", main="Binomial")
plot(xrej, dpois(xrej, lambda=1), main="Poisson", type="h")
points(xrej, pbin, col=2, pch=16)
par(mfrow=c(1,1))


#________Ejercicio 5. 
# VARIABLES ALEATORIAS CONTINUAS 
#funcion de masa: dnorm(x, mean=, sd = )
#funcion de distribución: pnomr(x, mean, sd)
#función cuanti: qnorm(x, mean, sd) 
#generación: rnorm(x,mean, sd)

#a) Generamos 15 valores de X ∈ U (3, 4).

runif(15, min=3, max=4)

#b) Dibuja la función de distribución y la función de densidad de una U (3, 4).

x = seq(3,4, by = 0.01)
par(mfrow = c(2,1))
plot(x, dunif(x, min = 3, max = 4), type = "l", main = "Función de Densidad", ylab = "prob")
plot(x, punif(x, min =3, max =4), type = "l", main = "Función de Distribución", ylab = "Y")
par(mfrow = c(1,1))

#c) Calcula la probabilidad de que una N (160, 100) tome valores menores de 150. ¿Y valores menores que su media?

pnorm(150, mean=160, sd= 10)

pnorm(160, mean=160, sd=10)

#d) Genera 100 valores de una N (1, 4) y dibuja la densidad de dicha variable.

rnorm(100, mean=160, sd=10)

#e) Dibuja las densidades de N (0, 1), N (5, 1) y N (0, 4).

#la camapana de gauss está definida en todo R con asíntotas horizontales hacia los dos lados pero no vamos a dibujar todo R 

#una distribución normal toma valores ditintos de 0 entre los valores: (media-4*desv.tip, media+4*desv.tip)

#N(0,1) debemos pintar su f de densidad  en el intervalo (-4,4) pq en el resto será practicamente 0 

xx= seq(-7,9, by=0.01)
yy= dnorm(xx, mean=1, sd=2)
plot(xx,yy, type="l") #f densidad de la normal N(1,4)

#N(0,1) -> (-4,4 )

#N(5,1) -> (1,9)

#N(0,4) -> (-8, 8)

#INTERVALO PAR REPRESENTARLOS TODOS A LA VEZ (-8,9)

x1= seq(-8,9, by= 0.01)
y1= dnorm(x1)#N(0,1) estandar 
y2= dnorm(x1, mean=5, sd=1)
y3= dnorm(x1, mean=0, sd=4)

plot(x1, y1, type="l", lwd=2, col=2)
lines(x1, y2,col=3, lwd=2)
lines(x1, y3, col=4, lwd=2)
legend("topleft", legend=c("N(0,1)", "N(5,1)", "N(0,4"), col=2:4, lwd=2, bty="n")




#f) ¿En qué punto la distribución de una N (0, 1) acumula el 50 % de la probabilidad? ¿En qué punto acumula una N (0, 1) el 5 % de la probabilidad? 
#¿Por debajo de qué punto en una N (160, 64) están el 80 % de los valores?

qnorm(0.5) #acumula el 50% equivalente a qnorm(0.5, mean=0, sd=1)

qnorm(0.05) #acumula el 5%

qnorm(0.8, mean=160, sd=8) # punto que deja a la izq una probabilidad del 80%


#g) Dibuja la masa de probabilidad de una Bi(40, 0.25). Sobre ésta, dibuja la curva de densidad de N (40 · 0.25, 40 · 0.25 · (1 − 0.25)).

x = 0:40
y = dbinom(x, size = 40, prob = 0.25)
plot(x,y, type = "h", lwd = 2, xlab = "x", ylab= "Probabilidad/Densidad", xlim = c(0, 21),main = "Masa de probabilidad de Bi(40,0.25)")
points(x,y,pch=20)

yn = seq(0,40, by = 0.01)

lines(dnorm(yn, mean = 40*0.25, sd = sqrt(40*0.25*0.75))~yn, col = 2, lwd = 2)

legend("topright", legend = c("Binomial", "Normal"), col = c(1,2), lwd = 2)



#_______Ejercicio 6. Carga el archivo de datos del iris de Fisher, data(iris), y analiza la distribución de la anchura del sépalo. 
# ¿Podríamos suponer que estos datos proceden de una distribución normal?

data(iris)
head(iris)
class(iris)

dato = iris$Sepal.Width

#HISTOGRAMA DE LOS DATOS. 
hist(dato, main = "histograma", xlab = "Anchura del Sépalo", freq = F, xlim = c(2,4.5))
# representamos la densidad de los datos
lines(density(dato), col = 3, lwd = 2)

#media y varianza 

media = mean(dato)
media

varianza = var(dato)*((length(dato)-1)/length(dato))
varianza

#REPRESENTAMOS LA  DENSIDAD NORMAL PARA LOS DATOS 
X = seq(0,5, by = 0.01)
lines(dnorm(X, mean = media, sd = sqrt(varianza))~X, col = 4, lwd = 2)



#______Ejercicio 7. 
#a) La probabilidad de que una Bi(5, 0.6) valga 3 es A.
#b) La probabilidad de que una BN (10, 2A) sea menor o igual que 5 es B.  (nbinom)
#c) Fijamos la semilla set.seed(5). Generamos 15 valores de una Pois(2B) y nos quedamos con el del noveno lugar (que será C).
#d) Si tenemos una N (C, 2C), calculamos la probabilidad de que valga menos o igual que C − 0.5, y obtenemos D.
#e) La probabilidad de que una Ge(2D) valga entre 1 y 3 (ambos incluidos) será E.
#f ) La probabilidad de que una Exp(E) sea menor que 10 será F .
#g) La media de una Γ(5, F ) será G.
#h) La parte entera (floor(·)) de G más 1 son las unidades del premio.
 
#A = P(Bi(5, 0.6) = 3) 
A = dbinom(3, size = 5, prob = 0.6)
# B = P(BN (10, 2A) <= 5)
B = pnbinom(5, size = 10, prob=2*A)
B

set.seed(5)
#Generamos 15 valores de una Pois(2B)
valores = rpois(15, lambda = 2*B)
valoomres
C = valores[9]
C


# D = P(N(C, 2C)<= C-0.5)
# D = P(N(4, 8)<= 3.5)
D = pnorm(3.5, mean = 4, sd = sqrt(8))
D

# E = P( 1 <= Ge(2D) <= 3 ) = P(Ge(2D) <= 3) - P(1 <= Ge(2D))

# En R, para la versión de geom que empieza en 0 (fallos antes del primer éxito):

# P(1 ≤ X≤ 3)= P(X ≤ 3) − P(X < 1) = P(X ≤ 3)−P(X ≤ 0)

E = pgeom(3, prob = 2*D) - pgeom(0, prob = 2*D)
E

# F = P(Exp(E) < 10)
F = pexp(10, rate = E)
F


# La media de una Γ(5, F) será G.
shape = 5
rate = F 

media = shape/rate
G = media 

# La parte entera (floor(·)) de G más 1 son las unidades del premio.
premio = floor(G)+1 
premio
