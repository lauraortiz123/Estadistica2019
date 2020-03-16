###### Semana 12-13 Normalidad

if(!require(ape)){install.packages("ape")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(ggridges)){install.packages("ggridges")}
if(!require(plyr)){install.packages("plyr")}
if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(biotools)){install.packages("biotools")}

if(!require(moments)){install.packages("moments")}
if(!require(nortest)){install.packages("nortest")}
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(car)){install.packages("car")}
if(!require(HardyWeinberg)){install.packages("HardyWeinberg")}

setwd("C:/Users/USER/OneDrive/Documentos/NUEVO/libros biologia/ESTADÍSTICA/Laboratorio/github/TRABAJOS EN CLASE/semana 12 y 13")
install.packages("readr")
library("readr")
getwd()
Data <- read.csv("Morphological_data.csv")
View(Data)

attach(Data)


###### Exploración manual

glimpse(Data)
#Observations: 693
#Variables: 27

summary(Data)

ddply(Data, 'COUNTRY', summarise, grp.mean=mean(BL),
      grp.median=median(BL),
      grp.SD=sd(BL))

ddply(Data, 'COUNTRY', summarise, grp.mean=mean(WW),
      grp.median=median(WW),
      grp.SD=sd(WW))

ddply(Data, 'COUNTRY', summarise, grp.mean=mean(DB),
      grp.median=median(DB),
      grp.SD=sd(DB))

table(Data$COUNTRY)

##### Exploración visual

ggplot(Data)+
  geom_point(aes(x=BL, y=WB,color= COUNTRY,shape=COUNTRY))

ggplot(Data)+
  geom_boxplot(aes(x=COUNTRY, y=BL, fill=COUNTRY),outlier.colour = "blue", outlier.shape = 1)

ggplot(Data)+
  geom_boxplot(aes(x=GENDER, y=BL, fill=GENDER),outlier.colour = "blue", outlier.shape = 1)


ggplot(Data ) +
  geom_violin(aes(x=COUNTRY, y=BL, fill=COUNTRY, color=COUNTRY),alpha=0.4,width=1.4) +
  geom_boxplot(aes(x=COUNTRY, y=BL),outlier.colour = "red", outlier.shape = 16,width=0.1)+
  geom_jitter(aes(x=COUNTRY, y=BL),alpha=0.4)



ggplot(Data)+
  geom_bar(aes(x=BL, fill=COUNTRY))+
  facet_wrap(~COUNTRY)


ggplot(Data) + 
  geom_density_ridges(aes(x = BL, y = COUNTRY))

#COUNTRY-BL
ggplot(Data, aes(sample = BL, colour = COUNTRY)) +
  stat_qq() +
  stat_qq_line()



#### Mexico

mexico <- Data[which(Data$COUNTRY == 'mx'),]

#Mexico-BL

ggplot(data = mexico, aes(x=BL)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(mexico$BL),
                            sd = sd(mexico$BL))) +
  ggtitle("Histograma mexico + curva normal teórica") +
  theme_bw()

#Mexico-WB

ggplot(data = mexico, aes(x=WB)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(mexico$WB),
                            sd = sd(mexico$WB))) +
  ggtitle("Histograma mexico + curva normal teórica") +
  theme_bw()

#Mexico-DB

ggplot(data = mexico, aes(x=DB)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(mexico$DB),
                            sd = sd(mexico$DB))) +
  ggtitle("Histograma mexico + curva normal teórica") +
  theme_bw()



########## TEST DE NORMALIDAD


#Contrastes de hipótesis
#Muestra mayor a 50
lillie.test(x = mexico$BL ) #Distribución normal

lillie.test(x = mexico$WB ) #No tiene distribución normal

lillie.test(x = mexico$DB ) #No tiene distribución normal

lillie.test(x = mexico$WLLP ) #Distribución normal


ggplot(mexico, aes(sample = BL)) +
  stat_qq() +
  stat_qq_line()

ggplot(mexico, aes(sample = WB)) +
  stat_qq() +
  stat_qq_line()

ggplot(mexico, aes(sample = DB)) +
  stat_qq() +
  stat_qq_line()

ggplot(mexico, aes(sample = WLLP)) +
  stat_qq() +
  stat_qq_line()



ggplot(data = mexico, aes(x=WB)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(mexico$WB),
                            sd = sd(mexico$WB))) +
  ggtitle("Histograma mexico + curva normal teórica") +
  theme_bw()

ggplot(data = mexico, aes(x=DB)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(mexico$DB),
                            sd = sd(mexico$DB))) +
  ggtitle("Histograma mexico + curva normal teórica") +
  theme_bw()

ggplot(data = mexico, aes(x=WLLP)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(mexico$WLLP),
                            sd = sd(mexico$WLLP))) +
  ggtitle("Histograma mexico + curva normal teórica") +
  theme_bw()


skewness(mexico$BL)
kurtosis(mexico$BL) #Leptocurtica

skewness(mexico$WB)
kurtosis(mexico$WB) #Leptocurtica

skewness(mexico$DB)
kurtosis(mexico$DB) #Leptocurtica

skewness(mexico$WLLP)
kurtosis(mexico$WLLP) #Leptocurtica




####### Homogeneidad de varianzas - HOMOCEDASTICIDAD

#Contraste de la razón de varianzas
ggplot(data = Data, aes(x = COUNTRY, y = BL, colour = COUNTRY)) +
  geom_boxplot() +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none")


aggregate(BL~COUNTRY, data = Data, FUN = var)

class(COUNTRY)
as.factor(COUNTRY) #COUNTRY convertido de caracter a factor

leveneTest(Data$BL ~ Data$COUNTRY) #No hay homogeneidad de varianza

leveneTest(Data$WB ~ Data$COUNTRY) #Hay homogeneidad de varianza

leveneTest(Data$DB ~ Data$COUNTRY) #Hay homogeneidad de varianza

leveneTest(Data$WLLP ~ Data$COUNTRY) #Hay homogeneidad de varianza


######## TRANSFORMACIONES

#México-BL ---> Distribución normal
ggplot(mexico, aes(sample = BL)) +
  stat_qq() +
  stat_qq_line()

#México-WB ---> Distribución NO normal

ggplot(mexico, aes(sample = WB)) +
  stat_qq() +
  stat_qq_line()

#México-DB ---> Distribución NO normal

ggplot(mexico, aes(sample = DB)) +
  stat_qq() +
  stat_qq_line()

#México-WLLP ---> Distribución normal

ggplot(mexico, aes(sample = WLLP)) +
  stat_qq() +
  stat_qq_line()


#Transformación de Tukey
#MEXICO-WB
mxWB <- data.frame(wb =mexico$WB)

lillie.test(mxWB$wb) # No sigue una distribución normal

T_tuk =  transformTukey(mxWB$wb) # Sigue una distribución normal después de la transformación

#MEXICO_DB
mxDB <- data.frame(db =mexico$DB)

lillie.test(mxDB$db) # No sigue una distribución normal

T_tuk =  transformTukey(mxDB$db) # No sigue una distribuión normal despues de la transformación



#Test paramétricos
usa <- Data[which(Data$COUNTRY == 'us'),]

lillie.test(x = usa$BL) #Distribución normal

t.test(mexico$BL, usa$BL, alternative = "two.sided") # Las medias son diferentes
t.test(mexico$BL, usa$BL, alternative = "greater") # La media de x es menor
t.test(mexico$BL, usa$BL, alternative = "less") # La media de y es mayor

var(mexico$BL)
var(usa$BL)
var(mexico$BL)/var(usa$BL) # Las varianzas son diferentes



#Test no paramétricos
lillie.test(x = usa$WB)

wilcox.test(mexico$WB, usa$WB, alternative = "two.sided") # No hay diferencias
wilcox.test(mexico$WB, usa$WB, alternative = "greater") # La media de x es mayor
wilcox.test(mexico$WB, usa$WB, alternative = "less") # La media de y es menor