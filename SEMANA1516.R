#QUIZ SEMANA 15 Y 16#
#Lisseth soler 2180007, Laura Ortiz 2181319, Fernando Villamizar 2180005#

##MANOVA##
#Se tiene una población total de 60 gallinas, a la mitad de estas sólo se les alimenta con maíz y a la otra mitad sólo con purina. Se quiere saber si existen diferencias significativas en el peso y tamaño de los huevos según los diferentes tipos de alimentación#
set.seed(1)
#simulando datos de maiz#
Peso <- rnorm(30,60,4)   
Tamaño <- runif(30,min=6, max=9)   
tratamiento <- rep("maiz",30)
Maiz <- cbind(Peso,Tamaño,tratamiento)

#simulando datos de purina#
Peso <- rnorm(30,63,5)   
Tamaño <- runif(30,min = 4.8, max = 6.2)  
tratamiento <- rep("Purina",30)
Purina <- cbind(Peso,Tamaño,tratamiento)

gallinas <- rbind(Purina,Maiz)
gallinas <- as.data.frame(gallinas)
gallinas_manova<- manova(cbind(Peso, Tamaño) ~ tratamiento, data = gallinas)
summary(gallinas_manova)
#Del resultado anterior, se observa que las dos variables de los huevos (peso y tamaño) son significativamente diferentes entre los tipos de alimentación#


##REGRESIÓN LOGÍSTICA##
#Una población de 60 gallinas está enferma con gripe aviar, se quiere saber si el peso y la edad de la gallina se encuentran relacionadas con su supervivencia#
#Simulando datos de peso y edad#
Peso <-runif(60,min = 1.7, max = 2.2)  #(peso en Kg)
edad <- runif(60,min = 1.2, max = 5)  #(edad en años)
supervivencia <-rbinom(60,1,prob = 0.5)
#uniendo las variables#
gallinas <- cbind(Peso,edad,supervivencia)

gallinas <- as.data.frame(gallinas)

gallinas_logit <- glm(supervivencia ~ Peso + edad, 
                      data = gallinas, family = "binomial")

summary(gallinas_logit)
#Viendo estos resultados no se encuentra una relación entre el peso y edad de las gallinas con la supervivencia de las mismas#


##REGRESIÓN MÚLTIPLE##
#En la misma población de gallinas se quiere saber si el gasto de purina (g x día) está relacionado con el peso (Kg) y edad (años) de la gallina; además del peso de su huevo#
#Simulando valores de peso, edad y peso del huevo#
Peso_huevo <-rnorm(60,63,5) 
Peso <-runif(60,min = 1.7, max = 2.2)  
edad <- runif(60,min = 1.2, max = 5) 
#Simulando datos del gasto de purina#
gasto.purina <- runif(60,min = 90, max =130)
gallinas <- cbind(Peso_huevo,Peso,edad,gasto.purina)
gallinas <- as.data.frame(gallinas)
#Regresión múltiple#
gallinas_reg_mul <- lm(gasto.purina~ Peso + edad+Peso_huevo, data = gallinas)
summary(gallinas_reg_mul)
#Lamentablemente ninguna de las variables empleadas (peso del huevo, peso y edad de la gallina) logró resolver la duda de la cantidad de purina gastada por día#

##ANÁLISIS FACTORIAL##
#Se tienen 13 gallinas y la cantidad de huevos que cada una de ellas pone a lo largo de los meses de enero, febrero,marzo y abril. Se quiere ver la relación entre la cantidad de huevos y los meses en los cuales fueron puestos
install.packages("readxl")
library(psych)
library(corrplot)
library(openxlsx)
datos <- read.xlsx(xlsxFile = "huevos por mes.xlsx")
datos
#verificar si es viable para realizar el analisi factorial 
mcorre<- cor(datos)
mcorre
cov(datos)
corrplot(cor(datos))
bartlett.test(datos)
KMO(datos)
#es viable realizarlo ya que existe una correlación entre algunos de sus datos
#Número de factores que se ursarán 
eigen(mcorre)
#Dos valores se encuentran por encima de la unidad
#Determinación de los factores 
AnalisisF<-factanal(datos, factors = 2, rotation = "none", scores = "Bartlett")
AnalisisF
#A partir de los dos factores tomados ( los que estaban por encima de la unidad )
#se muestra que en el mes de enero y febrero hubo una mayor relación entre la puesta de huevos 
#y los meses, cosa que no sucede con marzo, ya que en este mes aumenta de manera notable 
#el número de huevos puestos


##ANÁLISIS DISCRIMINANTE##
#Usando como ejemplo los datos de Iris, se conoce 3 especies diferentes a las cauels se han registrado distintos datos sobre sus flores. Se desea conocer un modelo para clasificar las flores de acuerdo a las variables regitradas (Longitud sepalo,Ancho sepalo,Longitud petalo y Ancho del petalo)#
library(MASS) 
#Cargando base de datos iris#
data(iris) 
Longitud_sepalo <- iris$Sepal.Length
Ancho_sepalo <-iris$Sepal.Width
Longitud_petalo <- iris$Petal.Length
Ancho_petalo <- iris$Petal.Width
Especies <- iris$Species
datos_flor <- cbind(Longitud_sepalo,Ancho_sepalo,Longitud_petalo,Ancho_petalo,Especies)

datos_flor <- as.data.frame(datos_flor)
iris.lda <- lda(Especies~ Longitud_sepalo+Ancho_sepalo+Longitud_petalo+Ancho_petalo,data =datos_flor )
iris.lda


##ANÁLISIS DE CLUSTER##
##Se quiere clasificar una población de 60 gallinas según el número de puestas de huevos a la semana#
set.seed(1)
peso.gall <- runif(60, 3.5, 5)
edad.gall <- rnorm(60, 2, 1.5)
puestasxsem <- rnorm(60, 5, 2)
library(cluster)
library(purrr)
data.gall <- c(peso.gall, edad.gall, puestasxsem)
#escalar las variables para que tengan media 0 o sd 1)
data.gall <- scale(data.gall)
#Aglomeración con agnes#
aglgall <- agnes(data.gall, method = "complete")
aglgall$ac


##MATRIZ DE CORRELACIÓN##
#Se quiere mirar si existe una relación entre las mediadas del pico,las patas y el peso de 60 gallinas#
set.seed(1)
peso<-rnorm(n = 60,m=60,sd=4)
#tamaño pico
tampi<- runif(n = 60,min = 2,max = 5)
#tamaño pata
tampa <- runif(n=60, min = 5, max = 7)
#hacer matriz
medidas<- matrix(c(peso,tampi,tampa),nrow = 60,ncol = 3)
medidas
#nombrar las columnas
colnames(medidas)<-c("peso","tampi","tampa")
medidas
cor(medidas)
#No existe relación entre las varibles peso, tamaño de la pata y tamaño del pico, al ser valores cercanos a 0 en los tres casos

##ANÁLISIS DE MANTEL##
ozone<-read.table("http://www.ats.ucla.edu/stat/r/faq/ozone.csv", sep=",", header=T)
head(ozone, n=10)
station.dists <- dist(cbind(ozone$Lon, ozone$Lat))
ozone.dists <- dist(ozone$Av8top)
as.matrix(station.dists)[1:5, 1:5]
as.matrix(ozone.dists)[1:5, 1:5]
mantel.rtest(station.dists, ozone.dists, nrepet = 9999)
gerd <- read.table("germen.dist.txt",header=T)
geod <- read.table("geo.dit.txt",header=T)
#cargar librerias
install.packages ("vegan")
library (vegan)
mantel.germenes<-mantel(geod, gerd)
mantel.germenes


##CORRELACIÓN##
#Se tiene una población de 60 gallinas y se quiere mirar si existe una relación entre el tamaño del pico y el peso#
set.seed(1)
#simular peso y tamaño del pico
peso<-rnorm(n = 60,m=60,sd=4)
tamaño<- runif(n = 60,min = 2,max = 5)
base<-cbind(peso,tamaño)
#redondear los resultados a 2 cifras decimales
round(cor(base),2)
#Del resultado obtenido se evidencia que no existe una relación lineal entre el tamaño del pico y el peso de la gallina, ya que este valor es muy cercano al 0#


