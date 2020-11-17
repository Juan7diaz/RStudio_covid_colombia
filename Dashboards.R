# -------------------------- TALLER 2 ----------------------------------------------------------
# ----------------------------------------------------------------------------------------------

# Curso de Estadística I
# Facultad de Ingeniería
# Universidad del Magdalena
# Grupo 03


# ---------------------------------------------------------------------------------------------

# *Información del grupo*

# Grupo asignado: 1  (Digite aquí el número de su grupo)

# Integrantes (Apellidos y Nombres - Código)

# Integrante 1: Gian Marco Astori Payares - 2020114003
# Integrante 2: Juan Diaz Guerrero - 2020114035
# Integrante 3: Johan Sebastian Hawkins Barrera - 2020114034


# ---------------------------------------------------------------------------------------------
# *Desarrollo de los ejercicios*
# ---------------------------------------------------------------------------------------------


######################################################################

# **Primer ejercicio: 9  (Digite aquí el número del ejercicio)

# ENUNCIADO
# Una compañía farmacéutica desea saber si un medicamento experimental tiene
# efecto sobre la presión sistólica de la sangre. A 15 pacientes seleccionados
# al azar se les aplicó el medicamento y, después de un tiempo suficiente para
# que el medicamento tuviera efecto, se registraron sus presiones sistólicas.
# Los datos aparecen a continuación:

# Variable (Defina la variable y asigne los datos correspondientes)
# Nombre de la variable "X": 

# Asignación de datos a la variable
x <- c(172,140,123,130,115,
       148,108,129,137,161,
       123,152,133,128,142)

length(x)#impar
sort(table(x))

# inciso a) Medidas de tendencia central

Media <-mean(x);Media
Mediana <-median(x);Mediana
library(modeest)
Moda<-mfv(x);Moda

# inciso b) Medidas de dispersión 

Varianza <- var(x);Varianza
D_Estandar <- sd(x);D_Estandar
C_Variacion <- sd(x) / mean(x);C_Variacion

library(moments)
Simetria <- skewness(x);Simetria #coeficientes de simetría
Curtosis <- kurtosis(x);Curtosis #coeficientes de curtosis

# Responda y justifique:

# ¿Los datos son homogéneos?
# R// Los datos si son homogéneos ya que son menores a 0.5

# ¿Los datos tienen un sesgo positivo?
# R// Si ya que es Asimetrica es decir de sesgo positiva (derecha)

# ¿La distribución de los datos es leptocúrtica, platicúrtica o mesocúrtica?
# Es leptocúrtica ya que la Kurtosis es mayor que 0, en la libreria de Momonts

# inciso c) Medidas de localización 

Q1 <-quantile(x,0.25);Q1
Q2 <-quantile(x,0.5);Q2
Q3 <-quantile(x,0.75);Q3

# inciso d) Histograma

hist(x,
     main = "Histograma Sobre Efectos de Presión Sistólica",
     xlab = "Presiones Sistólicas", ylab = "Frecuencia",
     xlim=(c(100, 180)), ylim=(c(0, 6)),
     col = 1:60,
     labels = T,
     breaks = 8)

abline(v=Media, col=1) 
abline(v=Mediana, col=2) 
abline(v=Moda, col=20) 
abline(v=Varianza, col=4)
abline(v=D_Estandar, col=5) 
abline(v=C_Variacion, col=6) 
abline(v=Simetria, col=7) 
abline(v=Curtosis, col=8) 
abline(v=Q1, col=9) 
abline(v=Q2, col=9) 
abline(v=Q3, col=9)

# inciso e) Diagrama de caja y bigotes

boxplot(x,
        col="#00b894", boxcol="darkblue", medcol="#e84393",
        outcol="red", staplecol="purple",
        main="Diagramas de caja y bigotes",
        ylab="Efectos de Presión Sistólicas")

# Responda y justifica

# ¿Hay observaciones atípicas?
# No los hay, ya que no hay datos por encima del limite superir o
# por debajo del limite inferior

# inciso f) Conclusiones

# Observación 1
# No hay datos atipicos.

# Observación 2
# Al momento de calcular Curtosis, probamos con 2 librerias diferentes:
# Moments (Kurtosis) & Psych (Kurtosi), pero cuando sacamos los 2 Resultados 
# estos eran diferentes ya que los parametros fijados en cada una de las
# librerias es diferente.

######################################################################

# **Segundo ejercicio: 14  (Digite aquí el número del ejercicio)

# ENUNCIADO
# Dos máquinas, cada una operada por una persona, son utilizadas para cortar
# tiras de hule, cuya longitud ideal es de 200 mm, con una tolerancia
# de ± 3 mm. Al final del turno un inspector toma una muestra e inspecciona que
# la longitud cumpla especificaciones. A continuación, se muestran
# las últimas 110 mediciones para ambas máquinas.

# Variable (Defina la variable y asigne los datos correspondientes)
# Nombre de la variable "y": 

# Asignación de datos a la variable

# Ya que en este ejercicio debemos trabajar con 2 conjuntos de datos distintos
# usaremos 2 variables: Operador 1 será "Y" y Operador 2 será "W".

y <- c(199.2,199.7,201.8,202.0,201.0,201.5,
       200.7,201.4,200.4,201.7,201.4,201.4,
       200.7,200.9,201.0,201.5,201.2,201.3,
       200.5,201.2,201.7,201.2,201.2,200.5,
       200.2,201.0,201.4,201.4,201.1,201.2,
       202.0,201.0,201.5,201.6,200.6,200.1,
       200.7,201.8,200.5,200.5,200.8,200.3,
       198.6,200.3,198.5,198.2,199.6,198.2,
       199.7,199.7,199.0,198.4,199.1,198.8,
       199.6)

sort(table(y))
length(y)#impar

w <- c(198.9,198.8,198.7,199.2,199.3,199.7,
       199.0,199.0,198.7,199.1,200.3,200.5,
       199.6,199.0,199.7,198.9,199.2,197.9,
       199.4,198.7,198.5,198.7,198.6,198.5,
       200.0,200.8,200.9,200.1,201.0,201.3,
       199.8,202.1,200.7,201.4,200.6,200.6,
       200.7,198.4,198.3,198.8,197.8,198.1,
       199.5,199.0,198.9,198.5,199.9,198.3,
       200.3,199.6,199.0,198.7,200.5,198.4,
       199.2)

sort(table(w))
length(w)#impar

# ----------------------------------------------------------------------------

# inciso a) Medidas de tendencia central


# Medidas de tendencia central para "y"
Media <-mean(y);Media
Mediana <-median(y);Mediana
library(modeest)
Moda<-mfv(y);Moda

##############################################################################

# Medidas de tendencia central para "w"
Media <-mean(w);Media
Mediana <-median(w);Mediana
library(modeest)
Moda<-mfv(w);Moda

# ----------------------------------------------------------------------------

# inciso b) Medidas de dispersión 

# Medidas de dispersión para "y"
Varianza <- var(y);Varianza
D_Estandar <- sd(y);D_Estandar
C_Variacion <- sd(y) / mean(y);C_Variacion
library(moments)
Simetria <- skewness(y);Simetria #coeficientes de simetría
Curtosis <- kurtosis(y);Curtosis #coeficientes de curtosis

# Responda y justifique:

# ¿Los datos son homogéneos?
# R// Los datos si son homogéneos ya que son menores a 0.5

# ¿Los datos tienen un sesgo positivo?
# R// No ya que es Asimetrica es decir de sesgo negativo (izquierda)

# ¿La distribución de los datos es leptocúrtica, platicúrtica o mesocúrtica?
# Es leptocúrtica ya que la Kurtosis es mayor que 0, en la libreria de Momonts

##############################################################################


# Medidas de dispersión para "w"
Varianza <- var(w);Varianza
D_Estandar <- sd(w);D_Estandar
C_Variacion <- sd(w) / mean(w);C_Variacion
library(moments)
Simetria <- skewness(w);Simetria #coeficientes de simetría
Curtosis <- kurtosis(w);Curtosis #coeficientes de curtosis

# Responda y justifique:

# ¿Los datos son homogéneos?
# R// Los datos si son homogéneos ya que son menores a 0.5

# ¿Los datos tienen un sesgo positivo?
# R// Si ya que es Asimetrica es decir de sesgo positiva (derecha)

# ¿La distribución de los datos es leptocúrtica, platicúrtica o mesocúrtica?
# Es leptocúrtica ya que la Kurtosis es mayor que 0, en la libreria de Momonts

# ----------------------------------------------------------------------------

# inciso c) Medidas de localización

# Medidas de localización para "y"
Q1 <-quantile(y,0.25);Q1
Q2 <-quantile(y,0.5);Q2
Q3 <-quantile(y,0.75);Q3

##############################################################################

# Medidas de localización para "w"
Q1 <-quantile(w,0.25);Q1
Q2 <-quantile(w,0.5);Q2
Q3 <-quantile(w,0.75);Q3



# ----------------------------------------------------------------------------

# inciso d) Histograma

#Histograma
hist(y,
     main = "Histograma Sobre Efectos de Presión Sistólica",
     xlab = "Presiones Sistólicas", ylab = "Frecuencia",
     xlim=(c(195, 205)), ylim=(c(0, 20)),
     col = 1:60,
     labels = T,
     breaks = 8)

abline(v=Media, col=1) 
abline(v=Mediana, col=2) 
abline(v=Moda, col=20) 
abline(v=Varianza, col=4)
abline(v=D_Estandar, col=5) 
abline(v=C_Variacion, col=6) 
abline(v=Simetria, col=7) 
abline(v=Curtosis, col=8) 
abline(v=Q1, col=9) 
abline(v=Q2, col=9) 
abline(v=Q3, col=9)

##############################################################################

#Histograma
hist(w,
     main = "Histograma Sobre Efectos de Presión Sistólica",
     xlab = "Presiones Sistólicas", ylab = "Frecuencia",
     xlim=(c(195, 205)), ylim=(c(0, 20)),
     col = 1:60,
     labels = T,
     breaks = 8)

abline(v=Media, col=1) 
abline(v=Mediana, col=2) 
abline(v=Moda, col=20) 
abline(v=Varianza, col=4)
abline(v=D_Estandar, col=5) 
abline(v=C_Variacion, col=6) 
abline(v=Simetria, col=7) 
abline(v=Curtosis, col=8) 
abline(v=Q1, col=9) 
abline(v=Q2, col=9) 
abline(v=Q3, col=9)

# ----------------------------------------------------------------------------

# inciso e) Diagrama de caja y bigotes

# Diagrama de caja y bigote para "y" & "w"
boxplot(w,y,
        col=4:5,
        main="Diagramas de caja y bigotes para y & w",
        outcol=5:8, staplecol=5:6,
        ylab="Efectos de Presión Sistólicas")

# Responda y justifica

# ¿Hay observaciones atípicas?
# No los hay, ya que no hay datos por encima del limite superir o
# por debajo del limite inferior

# inciso f) Conclusiones

# Observación 1
# No hay puntos atipicos

# Observación 2
# los graficos presentar simetrias opuestas.

# Observación 3
# Ambos conjuntos de datos son homogeneas y leptocurticas.

######################################################################

# **Tercer ejercicio: 1  (Digite aquí el número del ejercicio)

# ENUNCIADO
# Las siguientes puntuaciones representan la calificación en el examen final
# para un curso de estadística elemental.

# Variable (Defina la variable y asigne los datos correspondientes)
# Nombre de la variable "z": 

# Asignación de datos a la variable
z <- c(23,60,79,32,57,74,52,70,82,
       36,80,77,81,95,41,65,92,85,
       55,76,52,10,64,75,78,25,80,
       98,81,67,41,71,83,54,64,72,
       88,62,74,43,60,78,89,76,84,
       48,84,90,15,79,34,67,17,82,
       69,74,63,80,85,61)

sort(table(z))
length(z)#par

# inciso a) Medidas de tendencia central

Media <-mean(z);Media
Mediana <-median(z);Mediana
library(modeest)
Moda<-mfv(z);Moda

# inciso b) Medidas de dispersión

Varianza <- var(z);Varianza
D_Estandar <- sd(z);D_Estandar
C_Variacion <- sd(z) / mean(z);C_Variacion

library(moments)
Simetria <- skewness(z);Simetria 
Curtosis <- kurtosis(z);Curtosis 

# Responda y justifique:

# ¿Los datos son homogéneos?
# R// Los datos si son homogéneos ya que son menores a 0.5

# ¿Los datos tienen un sesgo positivo?
# R// No ya que es Asimetrica es decir de sesgo negativo (izquierda)

# ¿La distribución de los datos es leptocúrtica, platicúrtica o mesocúrtica?
# Es leptocúrtica ya que la Kurtosis es mayor que 0, en la libreria de Momonts

# inciso c) Medidas de localización 

Q1 <-quantile(z,0.25);Q1
Q2 <-quantile(z,0.5);Q2
Q3 <-quantile(z,0.75);Q3

# inciso d) Histograma

hist(z,
     main = "Histograma de Calificaciones",
     xlab = "Calificaciones", ylab = "Frecuencia",
     xlim=(c(0, 100)), ylim=(c(0, 20)),
     col = 1:60,
     labels = T,
     breaks = 10)

abline(v=Media, col=1) 
abline(v=Mediana, col=2) 
abline(v=Moda, col=3) 
abline(v=Varianza, col=4)
abline(v=D_Estandar, col=5) 
abline(v=C_Variacion, col=6) 
abline(v=Simetria, col=7) 
abline(v=Curtosis, col=8) 
abline(v=Q1, col=9) 
abline(v=Q2, col=9) 
abline(v=Q3, col=9)

# inciso e) Diagrama de caja y bigotes

boxplot(z,
        col="#00b894", boxcol="darkblue", medcol="#e84393",
        outcol="red", staplecol="purple",
        main="Diagramas de caja y bigotes",
        ylab="Calificaciones")

# Responda y justifica

# ¿Hay observaciones atípicas?
# Los datos atipicos son los valores de 10 y 15, ya que estan por debajo
# del limite inferior, para concluir esto usamos "ylim=(c(0,20))", donde de
# boxplot en la linea 155.

# inciso f) Conclusiones

# Observacion 1
# En la linea 135, el codigo "abline(v=Varianza, col=4)" no muestra la marca,
# a menos que el limite maximo de x (xlim) sea mayor o igual a 450.

# Observacion 2
# Al momento de calcular Curtosis, probamos con 2 librerias diferentes
# Moments (Kurtosis) & Psych (Kurtosi), pero cuando sacamos los 2 Resultados
# estos eran diferentes ya que los parametros fijados en cada una de
# las libreria es diferente.

# Observacion 3
# Para ampliar los limetes de X o Y en el histograma o en diagrama de
# caja y bigote usamos xlim=(c(n,m)) & ylim=(c(n,m)).

######################################################################
