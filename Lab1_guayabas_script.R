##Laboratorio Marcadores Moleculares- Pr?ctica 3##
##Diversidad Gen?tica de Guayaba en San Cristobal y el Continente##
##B Gutierrez##
##Septiembre 2017##

setwd("/Users/AndreaMontero/Desktop/Datos")
getwd()
library("hierfstat")
library("ade4")
library("adegenet")

##PARTE I- Diversidad Gen?tica

#Leer matriz de datos para estimar ?ndices de diversidad gen?tica
guayabas2<-read.csv("Datos_Lab1_Guayaba2.csv",header = TRUE,sep = ";")
guayabas2_diversity<-df2genind(guayabas2[,-1:-2], ind.names = guayabas2$X, pop = guayabas2$Pop,sep = "-", ploidy = 2)

#Estimar la heterocigocidad de cada poblaci?n
Diversidad_por_locus<-summary(guayabas2_diversity)
Diversidad_por_locus
Hs_por_poblacion<-Hs(guayabas2_diversity)
Hs_por_poblacion

#Comparar si los valores de Hs son diferentes entre poblaciones (estad?stica inferencial)
Hs_ContvsSC<-Hs.test(guayabas2_diversity[guayabas2_diversity$pop==1],
                     guayabas2_diversity[guayabas2_diversity$pop==2],
                     n.sim = 999,alter = "two-sided")
Hs_ContvsSC

##Adem?s de las diferencias entre dos ambas islas, hay evidencia que sugiere que podr?an existir dos grupos
##separados dentro de San Cristobal. Las tres primeras muestras corresponden a las localidades del norte
##(Cerro Verde y La SOledad), mientras que las siguientes dos muestras pertenecen al sur de la isla (localidad
##El Socavon). Puedes repetir este an?lisis con las tres poblaciones? (Modifica el c?digo de arriba y los archivos
##de datos necesarios para lograr esto).

##PARTE II- Distancias Gen?ticas

#Leer matriz de datos y convertirla a formato para estimar distancias gen?ticas
guayabas<-read.csv("Datos_Lab1_Guayaba_3pops.csv")
guayabas_gen<-as.data.frame(guayabas[,-1])
head(guayabas_gen)

#C?lculo de distancias gen?ticas
#Distancias de Nei
guayabas_nei<-pairwise.neifst(guayabas_gen,diploid=TRUE)
guayabas_nei
#DIstancias de Weir-Cockram (Fst)
guayabas_FST<-pairwise.WCfst(guayabas_gen, diploid=TRUE)
guayabas_FST



#Cynthia 
a<-(3*n)
n=1
a+3

5+5
a=(5+5)
b=c(1,2,3)
a=c(1,2,3)
b=c("A","B","C")
data.frame(a,b)
#Guayaba - Laboratorio marcadoresmoleculares
#diversidad genetica de la guayba en San Cristobal y continente

library(hierfstat)
library(ade4)
library(adegenet)




