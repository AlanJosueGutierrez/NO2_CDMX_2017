#EL problema que nos interesa es sobre la contaminaci?n del aire, para ello 
#obtenemos nuestros datos del siguiente enlace
#http://www.aire.cdmx.gob.mx/default.php?opc=%27aKBhnmI=%27&opcion=aw==
#tomamos el del a?o 2017 
#http://www.aire.cdmx.gob.mx/default.php?opc=%27aKBhnmE=&r=aHR0cDovLzE0OC4yNDMuMjMyLjExMjo4MDgwL29wZW5kYXRhL0luZGljZUNhbGlkYWRBaXJlL2luZGljZV8yMDE3LmNzdg==

#Dichos datos contienen un encabezado que es eliminado de forma manual para que no nos genere
#problemas con el an?lisis, guardamos dicho archivo con el nombre "indice_2017-2.csv"

#La poblaci?n en la que nos enfocaremos ser? en la del di?xido de Nitr?geno
#siendo uno de los compuestos que identificamos que tiene como variables a 
#las distintas zonas en las que se producen dicho compuesto. 
#Comenzamos....
#################################################################################
############################  ANALISIS EXPLORATORIO  ############################
#################################################################################
#Tenemos a la variable cualitativa zona con sus categorias (Noroeste, 
#Noreste, Suroeste, Sureste y Centro). Mientras que tenemos a la 
#cuantitativa ppb (unidades en las que estan nuestros datos).
#Por lo tanto agrupamos nuestras poblacion en 5 grupos:
#1 ppb del Di?xido de Nitr?geno en la zona Noroeste
#2 ppb del Di?xido de Nitr?geno en la zona Noreste
#3 ppb del Di?xido de Nitr?geno en la zona Suroeste
#4 ppb del Di?xido de Nitr?geno en la zona Sureste 
#5 ppb del Di?xido de Nitr?geno en la zona Centro

#Que representan nuestras variables del problema a estudiar. Entonces
#Cargamos nuestros datos, importante ubicarse en el mismo directorio de trabajo
#donde estan los datos
setwd("~/Documentos/10mo/Astro/Proyecto/") # Nos ubucamos en la carpeta te trabajo.

### Comenzamos por hacer una exploración superficial de nuestros datos.

indice <- read.csv("Datos/indice_2017.csv") # Leemos los primeros datos de interés.
head(indice)    # Damos un vistezo a los primeros datos.
dim(indice)     # Dimención de los datos. Renglones | Columnas.
attach(indice)  # GUARDAMOS en memoria los daotos.
names(indice)   # Imprimimos los títulos de las columnas.
str(indice)     # Mostramos el tipo de dato que tenemos por columna (se muestran los primeros 10).
mode(indice)    # Formato de los datos.

#Primero tomamos los valores de los data frames que nos interesan
## Definimos nuestras variables. En funci?n de estas describiremos el problema.
## Nos concentraremos en el dioxido de nitrogeno en las diferentes zonas.
NO2.NO <- indice$Noroeste.dioxido.de.nitrogeno
NO2.NE <- indice$Noreste.dioxido.de.nitrogeno
NO2.C <- indice$Centro.dioxido.de.nitrogeno
NO2.SO <- indice$Suroeste.dioxido.de.nitrogeno
NO2.SE <- indice$Sureste.dioxido.de.nitrogeno


#Ahora creamos el data frame para nuestra poblacion global
NO2 <- data.frame("NO" = NO2.NO, "NE" = NO2.NE, "SO" = NO2.SO, "SE" = NO2.SE, "C" = NO2.C)
dim(NO2)

#Quitamos datos faltantes, visualizamos la cantidad de datos y guardamos.
NO2 <- na.omit(NO2); dim(NO2); attach(NO2)  #(con esto se sumple que las variables tienen la misma cantidad de datos)
#Tenemos:
#--- Datos cuantitativos discretos ---

#Finalmente definimos 

#Tenemos:
###### Datos cuantitativos discretos #####



# Ya con las variables y los grupos correspondientes, procedemos al análisis exploratorio.
# Revisamos la tabla de frecuencias de cada grupo.
print(table(NO2)); print(table(NO2.NO)); print(table(NO2.NE)); print(table(NO2.C)); print(table(NO2.SO)); print(table(NO2.SE))

# Diagramas de barras respecto a las tablas de frecuencias.
par(mfrow=c(2,3));
barplot(prop.table(table(NO2$C))); title("NO2 en el centro", xlab = "ppb", ylab = "Frecuencia") # Concentración de NO2 en toda la cuidad.
barplot(prop.table(table(NO2$NO))); title("NO2 en el noroeste", xlab = "ppb", ylab = "Frecuencia")
barplot(prop.table(table(NO2$NE))); title("NO2 en el noreste", xlab = "ppb", ylab = "Frecuencia")
barplot(prop.table(table(NO2$C))); title("NO2 en el centro", xlab = "ppb", ylab = "Frecuencia")
barplot(prop.table(table(NO2$SO))); title("NO2 en el suroeste", xlab = "ppb", ylab = "Frecuencia")
barplot(prop.table(table(NO2$SE))); title("NO2 en el sureste", xlab = "ppb", ylab = "Frecuencia")
par(mfrow=c(1,1))

# Diagramas de caja y brazos en posición horizontal.
par(mfrow=c(2,3))
boxplot(NO2,horizontal=T,xlab="Partes por billón de NO2")   # Concentración en toda la ciudad.
boxplot(NO2$NO,horizontal=T,xlab="ppb de NO2 en el noroeste")
boxplot(NO2$NE,horizontal=T,xlab="ppb de NO2 en el noreste")
boxplot(NO2$C,horizontal=T,xlab="ppb de NO2 en el centro")
boxplot(NO2$SO,horizontal=T,xlab="ppb de NO2 en el suroeste")
boxplot(NO2$SE,horizontal=T,xlab="ppb de NO2 en el sureste")
par(mfrow=c(1,1))
# NOTA: Dado el número grande de puntos en la tabla de frecuencias, no es recomendable usar Diagramas Circulares.

# Un resumen de nuestras poblaciones.
summary(NO2) # Nos muestra los valores mínimos, máximos, quantiles, mediana y media; para cada valor.
var(NO2$NO); var(NO2$NE); var(NO2$C); var(NO2$SO); var(NO2$SE) # Varianzas de cada grupo de la población.
sd(NO2$NO); sd(NO2$NE); sd(NO2$C); sd(NO2$SO); sd(NO2$SE) # Desviación estandar de cada grupo de la población.



#################################################################################
######################  1.1 Estimación intervalos Confinza ######################
######################  1.2 Pruba de hipotesis paramétrica ######################
#################################################################################

#Ahora veremos si se satisface la normalidad para cada variable

#Revisando Normalidad en la zona Noroeste (NO)
qqnorm(NO)       # grafica QQ (cuantil-cuantil)
qqline(NO,lwd=2,col=2)

#Vemos tenemos problemas en varios datos
#install.packages("fBasics")
library(fBasics)#tambien se requieren los paquetes timeDate y timeSeries
lillieTest(NO) # prueba de hipotesis de bondad de ajuste
#H0:hay normalidad vs Ha:no hay normalidad
#Vemos que el valor p es de 2.2e-16, por lo tanto rechaazamos H0 y nos quedamos con Ha
#Por lo tanto no cumple normalidad

############################################################################
#Revisando Normalidad en la zona Noreste (NE)

qqnorm(NE)       # grafica QQ (cuantil-cuantil)
qqline(NE,lwd=2,col=2)

#Vemos tenemos problemas en varios datos
lillieTest(NE) # prueba de hipotesis de bondad de ajuste
#H0:hay normalidad vs Ha:no hay normalidad
#Vemos que el valor p es de 2.2e-16, por lo tanto rechaazamos H0 y nos quedamos con Ha
#Por lo tanto no cumple normalidad


############################################################################
#Revisando Normalidad en la zona Suroeste (SO)

qqnorm(SO)       # grafica QQ (cuantil-cuantil)
qqline(SO,lwd=2,col=2)

#Vemos tenemos problemas en varios datos
lillieTest(SO) # prueba de hipotesis de bondad de ajuste
#H0:hay normalidad vs Ha:no hay normalidad
#Vemos que el valor p es de 2.2e-16, por lo tanto rechaazamos H0 y nos quedamos con Ha
#Por lo tanto no cumple normalidad

############################################################################
#Revisando Normalidad en la zona Sureste (SE)

qqnorm(SE)       # grafica QQ (cuantil-cuantil)
qqline(SE,lwd=2,col=2)

#Vemos que tenemos problemas en varios datos
lillieTest(SE) # prueba de hipotesis de bondad de ajuste
#H0:hay normalidad vs Ha:no hay normalidad
#Vemos que el valor p es de 2.2e-16, por lo tanto rechaazamos H0 y nos quedamos con Ha
#Por lo tanto no cumple normalidad


############################################################################
#Revisando Normalidad en la zona Centro (C)

qqnorm(C)       # grafica QQ (cuantil-cuantil)
qqline(C,lwd=2,col=2)

#Vemos que tenemos problemas en varios datos
lillieTest(C) # prueba de hipotesis de bondad de ajuste
#H0:hay normalidad vs Ha:no hay normalidad
#Vemos que el valor p es de 2.2e-16, por lo tanto rechazamos H0 y nos quedamos con Ha
#Por lo tanto no cumple normalidad

#Ahora procederemos a transformar nuestras variables
#Para el Noroeste
library(forecast)
library(MASS)
lambdaNO <- BoxCox.lambda(NO,method="loglik",lower=-5,upper=5)
lambdaNO # l=0.2
lambda2NO = BoxCox.lambda(NO,method="guerrero",lower=-5,upper=5)
lambda2NO # l2=0.134, metodo guerrero
transNO = BoxCox(NO, lambdaNO)
trans2NO = BoxCox(NO, lambda2NO)


#Ahora veremos si se satisface la normalidad para la variable transformada
#Revisando Normalidad en la zona Noroeste (NO)
qqnorm(transNO) 
qqline(transNO,lwd=2,col=2)

qqnorm(trans2NO)
qqline(trans2NO,lwd=2,col=2)

#Vemos que nuestros datos son lineales para ambos casos
#Boxplot
boxplot(transNO, horizontal = T)
boxplot(trans2NO, horizontal = T)
#Esos datos atipicos podrian causarnos problemas
library(fBasics)
library(nortest)
ad.test(transNO) ; cvm.test(transNO) ; lillie.test(transNO) ; pearson.test(transNO) # prueba de hipotesis de bondad de ajuste
#H0:hay normalidad vs Ha:no hay normalidad
#Vemos que en todas las pruebas rechaazamos H0 y nos quedamos con Ha
#Por lo tanto no cumple normalidad para l=0.2, es posible que se deban a los atipicos
ad.test(trans2NO) ; cvm.test(trans2NO) ; lillie.test(trans2NO) ; pearson.test(trans2NO)
#De igual manera Vemos que en todas las pruebas rechazamos H0 y nos quedamos con Ha
#Por lo tanto no cumple normalidad para l=0.134


#Para el Noreste
lambdaNE <- BoxCox.lambda(NE,method="loglik",lower=-5,upper=5)
lambdaNE # l=0.2, metodo 
lambda2NE = BoxCox.lambda(NE,method="guerrero",lower=-5,upper=5)
lambda2NE # l2=0.096, metodo guerrero
transNE = BoxCox(NE, lambdaNE)
trans2NE = BoxCox(NE, lambda2NE)


#Ahora veremos si se satisface la normalidad para la variable transformada
#Revisando Normalidad en la zona Noroeste (NO)
qqnorm(transNE) 
qqline(transNE,lwd=2,col=2)

qqnorm(trans2NE)
qqline(trans2NE,lwd=2,col=2)

#Vemos que nuestros datos son lineales para ambos casos, sin embargo en los extremos hay problemas
#Boxplot
boxplot(transNE, horizontal = T)
boxplot(trans2NE, horizontal = T)
#Esos datos atipicos podrian causarnos problemas
ad.test(transNE) ; cvm.test(transNE) ; lillie.test(transNE) ; pearson.test(transNE) # prueba de hipotesis de bondad de ajuste
#H0:hay normalidad vs Ha:no hay normalidad
#Vemos que en todas las pruebas rechazamos H0 y nos quedamos con Ha
#Por lo tanto no cumple normalidad para l=0.2, es posible que se deban a los atipicos
ad.test(trans2NE) ; cvm.test(trans2NE) ; lillie.test(trans2NE) ; pearson.test(trans2NE)
#De igual manera Vemos que en todas las pruebas rechazamos H0 y nos quedamos con Ha
#Por lo tanto no cumple normalidad para l=0.096

#Para el Suroeste
lambdaSO <- BoxCox.lambda(SO,method="loglik",lower=-5,upper=5)
lambdaSO # l=0.25, metodo 
lambda2SO = BoxCox.lambda(SO,method="guerrero",lower=-5,upper=5)
lambda2SO # l2=0.175, metodo guerrero
transSO = BoxCox(SO, lambdaSO)
trans2SO = BoxCox(SO, lambda2SO)


#Ahora veremos si se satisface la normalidad para la variable transformada
#Revisando Normalidad en la zona Noroeste (NO)
qqnorm(transSO) 
qqline(transSO,lwd=2,col=2)

qqnorm(trans2SO)
qqline(trans2SO,lwd=2,col=2)

#Vemos que nuestros datos son lineales para ambos casos, con problemas en los extremos
#Boxplot
boxplot(transSO, horizontal = T)
boxplot(trans2SO, horizontal = T)
#Esos datos atipicos podrian causarnos problemas
ad.test(transSO) ; cvm.test(transSO) ; lillie.test(transSO) ; pearson.test(transSO) # prueba de hipotesis de bondad de ajuste
#H0:hay normalidad vs Ha:no hay normalidad
#Vemos que en todas las pruebas rechazamos H0 y nos quedamos con Ha
#Por lo tanto no cumple normalidad para l=0.25, es posible que se deban a los atipicos
ad.test(trans2SO) ; cvm.test(trans2SO) ; lillie.test(trans2SO) ; pearson.test(trans2SO)
#De igual manera Vemos que en todas las pruebas rechazamos H0 y nos quedamos con Ha
#Por lo tanto no cumple normalidad para l2=0.175

#Para el Sureste
lambdaSE <- BoxCox.lambda(SE,method="loglik",lower=-5,upper=5)
lambdaSE # l=0.3, metodo 
lambda2SE = BoxCox.lambda(SE,method="guerrero",lower=-5,upper=5)
lambda2SE # l2=0.205, metodo guerrero
transSE = BoxCox(SE, lambdaSE)
trans2SE = BoxCox(SE, lambda2SE)


#Ahora veremos si se satisface la normalidad para la variable transformada
#Revisando Normalidad en la zona Noroeste (NO)
qqnorm(transSE) 
qqline(transSE,lwd=2,col=2)

qqnorm(trans2SE)
qqline(trans2SE,lwd=2,col=2)

#Vemos que nuestros datos son lineales para ambos casos
#Boxplot
boxplot(transSE, horizontal = T)
boxplot(trans2SE, horizontal = T)
#Esos datos atipicos podrian causarnos problemas
ad.test(transSE) ; cvm.test(transSE) ; lillie.test(transSE) ; pearson.test(transSE) # prueba de hipotesis de bondad de ajuste
#H0:hay normalidad vs Ha:no hay normalidad
#Vemos que en todas las pruebas rechazamos H0 y nos quedamos con Ha
#Por lo tanto no cumple normalidad para l=0.3, es posible que se deban a los atipicos
ad.test(trans2SE) ; cvm.test(trans2SE) ; lillie.test(trans2SE) ; pearson.test(trans2SE)
#De igual manera Vemos que en todas las pruebas rechazamos H0 y nos quedamos con Ha
#Por lo tanto no cumple normalidad para l=0.205


#Para el Centro
lambdaC <- BoxCox.lambda(C,method="loglik",lower=-5,upper=5)
lambdaC # l=0.05, metodo 
lambda2C = BoxCox.lambda(C,method="guerrero",lower=-5,upper=5)
lambda2C # l2=0.029, metodo guerrero
transC = BoxCox(C, lambdaC)
trans2C = BoxCox(C, lambda2C)


#Ahora veremos si se satisface la normalidad para la variable transformada
#Revisando Normalidad en la zona Noroeste (NO)
qqnorm(transC) 
qqline(transC,lwd=2,col=2)

qqnorm(trans2C)
qqline(trans2C,lwd=2,col=2)

#Vemos que nuestros datos son lineales para ambos casos, con problemas en los extremos
#Boxplot
boxplot(transC, horizontal = T)
boxplot(trans2C, horizontal = T)
#Esos datos atipicos podrian causarnos problemas
ad.test(transC) ; cvm.test(transC) ; lillie.test(transC) ; pearson.test(transC) # prueba de hipotesis de bondad de ajuste
#H0:hay normalidad vs Ha:no hay normalidad
#Vemos que en todas las pruebas rechazamos H0 y nos quedamos con Ha
#Por lo tanto no cumple normalidad para l=0.05, es posible que se deban a los atipicos
ad.test(trans2C) ; cvm.test(trans2C) ; lillie.test(trans2C) ; pearson.test(trans2C)
#De igual manera Vemos que en todas las pruebas rechazamos H0 y nos quedamos con Ha
#Por lo tanto no cumple normalidad para l=0.029


#Vemos que ninguna variable cumple normalidad, aun asi estimaremos intervalos de confianza para
#las variables transformadas sabiendo que no es del todo correcto
#Nos tomamos una muestra aleatoria de 3000 elementos de nuestra poblacion transformada
#Para el muestreo aleatorio simple sin repetici?n, basta con hacer:

indices <- sample( 1:nrow( NO2 ), 3000 )#generamos indices aleatorios
transNO.muestreado <- transNO[ indices ]#tomamos los elementos de lo numeros generados

indices <- sample( 1:nrow( NO2 ), 3000 )
transNE.muestreado <- transNE[ indices ]

indices <- sample( 1:nrow( NO2 ), 3000 )
transSO.muestreado <- transSO[ indices ]

indices <- sample( 1:nrow( NO2 ), 3000 )
transSE.muestreado <- transSE[ indices ]

indices <- sample( 1:nrow( NO2 ), 3000 )
transC.muestreado <- transC[ indices ]

#Ahora creamos el data frame para nuestra muestra de la poblaci?n
NO2.trans <- data.frame("NO.trans" = transNO.muestreado, "NE.trans" = transNE.muestreado, 
                        "SO.trans" = transSO.muestreado, "SE.trans" = transSE.muestreado,
                        "C.trans" = transC.muestreado)
dim(NO2.trans)
attach(NO2.trans)


# 1.1.1
# a)Estimaremos el intervalo para una variable
#En este caso sera la zona centro

#Conocemos la media poblacional y sd poblacionales
#Juntamos todas las columnas de los primeros datos transformados para sacar la media de toda la poblacion
NO2TRANS <- c(transNO,transNE,transSO,transSE,transC)
meanpoblacional<-mean(NO2TRANS)#3.529
sdpoblacional<-sd(NO2TRANS)#0.8934

# El i.c de mu es: xbarra+-z(alpha/2)*(sigma/sqrt(n))
tbarra <- mean(C.trans)#2.98 para la muestra aleatoria tomada
error_estim<- qnorm(0.08/2,mean=0,sd=1,lower.tail=F)*(0.8/sqrt(length(C.trans)))#el error de estimacion q de cuantil le ponemos la alpha entre dos, hablamos de una z y la cola media baja le ponemos falso para que nos de el signo positivo, la sigma vale 0.6 entre la raiz de la longitud de las datos

c(tbarra-error_estim,tbarra+error_estim)
#El intervalo que nos da resulta de (2.956,3.008) con una confianza del 92% 
#en donde deber?a de estar la media poblacional sin embargo vemos que no es as?

#Hagamos la prueba de hipotesis
#?existe razon para creer, con un nivel de significancia de 0.08, que los niveles
#de concentracion transformados de la poblacion promedio no es de 3.529?
# H0: mu=3.529   vs Ha: mu=?3.529 (distinto de 3.529)
#install.packages("PASWR")
library(PASWR)

zcal<-(mean(C.trans)-3.529)/(0.8/sqrt(length(C.trans)))
zcrit1<- qnorm(0.08/2,mean=0,sd=1,lower.tail=T)#el valor critico
zcrit2<- qnorm(0.08/2,mean=0,sd=1,lower.tail=F)


prueba1<- z.test(C.trans,alternative="two.sided",mu=3.529,sigma.x=0.8,conf.level=0.92)
prueba1$statistic
prueba1$p.value
prueba1$conf.int

#Tenemos zcal=-37.42 < zcrit1=-1.75 es decir caemos en la zona de rechazo por lo tanto
#rechazamos hip nula es decir mu distinto de 3.529
#Ademas tenemos un valor p de 1.81e-306 donde tenemos que rechazar H0

#Concluimos que el enfoque parametrico no es una buen modelo


#1.1.2.
# a) Ahora lo haremos para dos poblaciones, la zona centro y la zona Noroeste
# muestras independientes
#H0:miu1-miu2=0 vs Ha:miu1-miu2=?0
t.test(C.trans,NO.trans,alternative="less",mu0 = 0,paired = F,var.equal = T,conf.level = 0.92)
qt(0.08,df=length(C.trans)+length(NO.trans)-2,lower.tail = T)#df son los grados de libertad
#El intervalo que nos da es de (-inf,-0.63)
#Vemos que no pasa por el cero por lo tanto las consideramos diferentes
#Es decir nos tomamos Ha miu1<miu2 lo que es equivalente  decir que la
#media en el centro es menor que la media en el Noroeste para las
#Variables transformadas

#En la prueba de hiptesis tenemos
#El t calculado es de -37.795
#tcritico = -1.4
#Por lo tanto tenemos que tcalculado=-37. < tcritico=-1.4 Por lo tanto estamos en una zona de rechazo
#Por lo tanto nos quedamos con la Ha que las medias son diferentes

# b) No son Independientes.




#################################################################################
################# 2. HIP NO PARAMÉTRICAS Y ANÁLISIS CATEGÓRICOS #################
#################################################################################

# 2.1 Pruebas de hipótesis no paramétricas.
# H0: Med_1 = Med_2   vs Ha: Med_1 != Med_2.
# NOTESE: Ahora se comparan las medianas en lugar de las medias. Siempre se compara en parejas.
wilcox.test(NO2$NO,NO2$NE,paired=F,alternative="two.sided",exact=F,conf.level=0.92) # Valor-p -> 0. Rechazamos H0.
wilcox.test(NO2$NO,NO2$C,paired=F,alternative="two.sided",exact=F,conf.level=0.92) # Valor-p -> 0. Rechazamos H0.
wilcox.test(NO2$NO,NO2$SO,paired=F,alternative="two.sided",exact=F,conf.level=0.92) # Valor-p -> 0. Rechazamos H0.
wilcox.test(NO2$NO,NO2$SE,paired=F,alternative="two.sided",exact=F,conf.level=0.92) # Valor-p -> 0. Rechazamos H0.

wilcox.test(NO2$NE,NO2$C,paired=F,alternative="two.sided",exact=F,conf.level=0.92) # Valor-p -> 0. Rechazamos H0.
wilcox.test(NO2$NE,NO2$SO,paired=F,alternative="two.sided",exact=F,conf.level=0.92) # Valor-p -> 0. Rechazamos H0.
wilcox.test(NO2$NE,NO2$SE,paired=F,alternative="two.sided",exact=F,conf.level=0.92) # Valor-p -> 0. Rechazamos H0.

wilcox.test(NO2$C,NO2$SO,paired=F,alternative="two.sided",exact=F,conf.level=0.92) # Valor-p -> 0. Rechazamos H0.
wilcox.test(NO2$C,NO2$SE,paired=F,alternative="two.sided",exact=F,conf.level=0.92) # Valor-p -> 0. Rechazamos H0.

wilcox.test(NO2$SE,NO2$SO,paired=F,alternative="two.sided",exact=F,conf.level=0.92) # Valor-p -> 0. Rechazamos H0.

# En todos los casos obtenemos que las concentraciones de NO2 son diferentes a las demás zonas.
# Considerando los calores emparejados, el resultado es análogo.
# El resultado es similar al encontrado en el enfoque paramétrico.

#Para la muestra

#Nos tomamos una muestra aleatoria de 3000 elementos de nuestra pobacion
#Para el muestreo aleatorio simple sin repetición, basta con hacer:

indices <- sample( 1:nrow( NO2 ), 3000 )
NO2.muestreado <- NO2[ indices, ]
dim(NO2.muestreado)


# 2.2 Análisis de datos categóricos.
# a) construir una tabla de contingencia.

library(car)

# Usaremos las siguientes tablas de contingencia para co,nstruir la tabla categórica de dos variables.
# Corresponden al NO2 en el noreste y noroeste.
cont<-table(NO2.muestreado$NO); cont1<-table(NO2.muestreado$NE); cont; cont1
#Esta sería nuestra tabla categórica completa.
            #2,3, 4, 5, 6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,22, 23,24,25,26,27,28,29,10,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,54,58,61,
A <- matrix(c(1,8,27,40,53, 90,106,126,225,161,147,179,141,155,177,128,126,129,177,117,90,102,67,60,57,38,30,33,39,26,14,22, 6,17,16, 7,10,10,10, 5, 6, 2, 2, 5, 4, 3, 2, 2, 0, 0, 1, 0, 1,
              3,4,19,64,97,122,170,131,216,129,183,157,151,139,155,132,143,115,146, 88,92, 89,61,57,49,42,32,33,43,20,17,14, 8,18, 7, 4, 7, 7,16, 4, 3, 3, 1, 2, 0, 0, 2, 0, 3, 1, 0, 0, 0),
           nrow = 2, byrow = T)
# A sería nuestra tabla de contingencia, sin embargo es demaiado "larga" para los siguientes tests.

# b) 
# i) Nuestra hipotesis nula (H0) es: independencia. La hipótesis alternativa (Ha) es: no independencia.
chisq.test(A); fisher.test(A) # No funcionan.
# Ahora podemos proceder a "cortar" nuestra tabla hasta un punto en el cual R sea capaz de calcularlo.
# Utilizamos 3 valores centrales y 3 de cada lado de la tabla de contingencia.
    Aa <- matrix(c(1,8,27,161,147,179,0, 0, 1,
                   3,4,19,129,183,157,3, 1, 0),
                 nrow = 2, byrow = T)           # ii) Elegimosun valor de significancia de 0.02.
    fisher.test(Aa,conf.level = 0.98)
    chisq.test(Aa) # iii) Al hacer estas pruebas nos da que sólo esta funciona con un p-value de 0.02. RECHAZAMOS H0.
                   #      Por lo tanto podemos concluir que NO HAY INDEPENDENCIA.



################################################################################
########################### 3. ANALISIS DE REGRESION ###########################
################################################################################

pairs(NO2)
cor(NO2)

model.NO2.1<- lm(C~NO+NE+SO+SE)#primer modelo con todas
summary(model.NO2.1)

#Tenemos la ecuacion 
#y=b+a1x1+a2x2+a3x3+a4x4
#y=0.313 + 0.327NO + 0.259NE + 0.322SO + 0.213SE

#Haremos una prueba general de los supuestos de la regresion
#install.packages("gvlma")
library(gvlma)
valida_model.NO2.1 <- gvlma(model.NO2.1)
summary(valida_model.NO2.1)

#Vemos que no satisface ningun supuesto, aunque la r cuadrada tiene un valor de 0.854
#no satisface:
#Global Stat        3171.93 0.000e+00 Assumptions NOT satisfied!
#Skewness            459.73 0.000e+00 Assumptions NOT satisfied!
#Kurtosis           2604.40 0.000e+00 Assumptions NOT satisfied!
#Link Function        89.62 0.000e+00 Assumptions NOT satisfied!
#Heteroscedasticity   18.18 2.012e-05 Assumptions NOT satisfied!

# Checamos 2. Supuesto de Independencia de residuos (Prueba Durbin-Watson) es el mas importante
#H0: autocorrelacion es cero
#install.packages("lmtest");
library(lmtest)#para ver indp. de residuos
C.dwtest<- dwtest(model.NO2.1,data=C); Y.dwtest#metemos el modelo con los datos que queremos ajustar son C
result.autocor<- C.dwtest$statistic; result.autocor

# valor-p indica que NO hay autocorrelacion, es decir, que se cumple la independencia
# de residuos. 

#Para estimar un mejor modelo y dado que transformando las variables veiamos que no se satisfacia
#normalidad para mejorar el modelo lo que haremos sera eliminar regresores


#Tomamos el de norte con centro ya que parece haber una mejor relacion
plot(NO,C,pch=16,xlab="ppb en NO",ylab="ppb en C",
     main="Relacion del NO2")

fit_NO2<- lm(C~NO); fit_NO2; summary(fit_NO2)

#LA ecuacion y = b + ax es 
#C = 3.11 (0.106) + 0.862 (0.005) NO, lo que esta entre parentesis es el error
# en ambos casos tenemos un valor de t de 2e<-16, por lo tanto nos dice que ambos coefientes son
#distintos de cero, la rcuadrada vale 0.727

#Veamos que pasa si quitamos el intercepto
fit_NO2.2<- lm(C~NO-1); fit_NO2.2; summary(fit_NO2.2)
#LA ecuacion y = ax es 
#C = 1.013 (0.002)NO, vemos que el error en la pendiente disminuye
# tenemos valor de t de 2e<-16, por lo tanto nos dice que el coefiente
# es distinto de cero, el valor de rcuadrada aumenta a 0.949, este 
#parece ser un mejor modelo

abline(fit_NO2,col="red")#hacer una linea recta
abline(fit_NO2.2,col="blue")

#Haremos una prueba general de los supuestos de la regresion
#install.packages("gvlma")
#library(gvlma)
#Modelo con intercepto
valida_fit_NO2 <- gvlma(fit_NO2)
summary(valida_fit_NO2)

#Entonces tenemos


#                      Value p-value                  Decision
#Global Stat        2435.335 0.00000 Assumptions NOT satisfied!
#Skewness            154.041 0.00000 Assumptions NOT satisfied!
#Kurtosis           2275.384 0.00000 Assumptions NOT satisfied!
#Link Function         2.883 0.08952    Assumptions acceptable.
#Heteroscedasticity    3.027 0.08188    Assumptions acceptable.
#Ahora satisface 2 condiciones
#Veremos ind. en residuos para el modelo con intercepto
# H0: autocorrelacion es cero

#library(lmtest)#para ver indp. de residuos
C.dwtest<- dwtest(fit_NO2,data=C); C.dwtest#metemos el modelo con los datos que queremos ajustar son C
result.autocor<- C.dwtest$statistic; result.autocor
#p=2.26e-16 por lo tanto rechazamos hip nula es decir si hay dependencia en los residuos 


#Modelo sin intercepto verificaremos manualmente

# 1. Supuesto de Linealidad 
par(mfrow = c(1,1))
plot(NO,C,pch=16,xlab="ppb en el NO",ylab="ppb en el C",
     main="Relaci?n de NO2")
abline(fit_NO2.2,col=4,lwd=2)
legend("topleft",legend = paste("R-Cuadrado Ajustado=",
                                round(summary(fit_NO2.2)$adj.r.squared,2)),cex=.7)
#La r cuadrada nos dice que si hay linelaidad

#2. Veremos ind. en residuos
# H0: autocorrelacion es cero

library(lmtest)#para ver indp. de residuos
C.dwtest<- dwtest(fit_NO2.2,data=C); C.dwtest#metemos el modelo con los datos que queremos ajustar son C
result.autocor<- C.dwtest$statistic; result.autocor
#p=2.26e-16 por lo tanto rechazamos hip nula es decir si hay dependencia en los residuos 

#Diagnostico del modelo (Verificacion de supuestos): GRAFICAS EN R

plot(fit_NO2)#modelo de regresion con ordenada

# a) Residuos vs valores ajustados (predichos). IDEAL: puntos aleatorios, vemos cierta tendencia, lo
#que no dice claramente que no hay normalidad

# b) Grafica QQ. IDEAL: La distribucion de los ERRORES deberia caer sobre la recta 
# para decir que se distribuyen normalmente.Pero no lo hacen

# c) Escala-Localizacion. IDEAL: puntos aleatorios. No patrones.
#Se aprecia un patron como si de interferencia se tratase

# d) Distancia de Cook: indica cuales puntos tienen una fuerte influencia sobre la regresion.
# ("leverage points"). 




plot(fit_NO2.2)#modelo de regresion sin ordenada

# a) Residuos vs valores ajustados (predichos). IDEAL: puntos aleatorios, Se nota cierta
#tendencia

# b) Grafica QQ. IDEAL: Vemos que no caen sobre la recta por lo tanto no hay normalidad

# c) Escala-Localizacion. Pareciera que hay cierto patron

# d) Vemos que los puntos mas bajos tienen mayor influencia

#Por lo tanto elegimos el modelo que tiene mayor Rcuadrada
plot(NO,C,pch=16,xlab="ppb en el NO",ylab="ppb en el C",
     main="Relaci?n de NO2",xlim=c(0,150),ylim=c(0,100))
abline(fit_NO2.2,col=4,lwd=2) # sin ordenada al origen
legend("topleft",legend = paste("R-Cuadrado Ajustado=",
                                round(summary(fit_NO2.2)$adj.r.squared,2)),cex=.7)

max(NO)

### Intervalo de confianza para los regresores, en este caso es NO
NO1<- data.frame(NO=seq(0,61))  # debemos cambiar el formato de los datos, para que funcione la funcion predict
ic.NO2 <- predict(fit_NO2.2,NO1,interval="confidence")
head(ic.NO2)
#nos da su limite inferior y superior

#### Banda de confianza para el modelo, nos interesa dar el modelo y sus incertidumbres.
lines(NO1$NO, ic.NO2[,2],lty=2,lwd=2,col="red")#hacemos una grafica de lineas, ponindole el intervalo de confianza, aca es el inferior
lines(NO1$NO, ic.NO2[,3],lty=2,lwd=2,col="red")#aca el superior

### Intervalo de prediccion para la respuesta C (Centro), bandas de prediccion, queremos predecir nuevos valores.
nuevas.NO<- data.frame(NO=seq(61,150))  
ip.NO2 <- predict(fit_NO2.2,nuevas.NO,interval="prediction")#nos da el intervalo de prediccion
head(ip.NO2)

#### Banda de prediccion para las nuevas observaciones de velocidad
lines(nuevas.NO$NO, ip.NO2[,2],lty=2,lwd=2,col="green")
lines(nuevas.NO$NO, ip.NO2[,3],lty=2,lwd=2,col="green")

################################### ANOVA ################################## 
# El problema de analizar la calidad de la recta de regresion estimada se hace 
# a traves del metodo del ANALISIS DE VARIANZA (ANOVA).
# En este proceso la variacion total de la variable dependiente (respuesta) se 
# descompone en varias partes:
# SCR= suma de los cuadrados de la regresion
# SCE= suma de los cuadrados de los errores

anova(fit_NO2.2)   # analisis de la varianza
# F=2.2e-16, rechazamos hipotesis nula, osea
# Se concluye que el modelo es adecuado.
