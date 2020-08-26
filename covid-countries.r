#PASO 0. CARGAR LA BASE DE DATOS DESDE MENU:
#IMPORTANTE, SEPARADOR ; FECHA TIPO DATA Y RESTO NUMERICOS
#se puede hacer con estas instrucciones pero mejor desde menu
VariosPaises_Covid19 <- read.csv("VariosPaises_Covid19Actualizado.csv", sep=";")
VariosPaises_Covid19$FECHA<-as.Date(VariosPaises_Covid19$FECHA, "%d/%m/%Y")
VariosPaises_Covid19$CONTAGIADOS<-as.integer(VariosPaises_Covid19$CONTAGIADOS)
VariosPaises_Covid19$FALLECIDOS<-as.integer(VariosPaises_Covid19$FALLECIDOS)
VariosPaises_Covid19$RECUPERADOS<-as.integer(VariosPaises_Covid19$RECUPERADOS)
VariosPaises_Covid19$HOSPITALIZADOS<-as.integer(VariosPaises_Covid19$HOSPITALIZADOS)
VariosPaises_Covid19$UCIs<-as.integer(VariosPaises_Covid19$UCIs)
# comprobar si son numericos las variables restantes
library(dplyr)
library(ggplot2)
#1. Elimino filas y columnas sobrantes y compruebo que no hay errores en paises
VariosPaises_Covid19<-VariosPaises_Covid19Actualizado[1:1737,1:7]
(paises<-unique(VariosPaises_Covid19$COUNTRY))
#2. Completo con ceros toda la base de datos
#para evitar errores de calculos, con la funcion
# limpiar.r que debo cargar primero en el workspace
d<-dim(VariosPaises_Covid19)
VariosPaises_Covid19<-limpiar(VariosPaises_Covid19, 1, d[1], 3, d[2],0)
#3. CALCULO DEL INDICADOR DE PELIGROSIDAD:
attach(VariosPaises_Covid19)#asocio la bbdd para no usar $
IP<-(CONTAGIADOS+FALLECIDOS)-RECUPERADOS
detach(VariosPaises_Covid19)
# 4. Implementacion de la funcion R0 para reutilizar 
R0<-function(x,inc){
  valor<-0
  for (i in inc:length(x)){
    valor[i]<-x[i]/x[i-inc+1]
  }
  return(valor)
}
#4. Calculo los R0 respecto a 3dias, 7dias y 14dias
R0_3<-R0(VariosPaises_Covid19$CONTAGIADOS,3)
R0_7<-R0(VariosPaises_Covid19$CONTAGIADOS,7)
R0_14<-R0(VariosPaises_Covid19$CONTAGIADOS,14)

#5. GUARDO LOS DATOS CONCATENADOS
VariosPaises_Covid19<-data.frame(VariosPaises_Covid19, IP, R0_3, R0_7,R0_14)


#6. GRAFICOS PARA TODOS LOS PAISES
#limpiamos nas, nans e infs de las columnas calculadas
#usando la funcion limpiar.r parametrizada adecuadamente

VariosPaises_Covid19<-limpiar(VariosPaises_Covid19,1,1737,8,11,0)
#ploteamos los casos por fecha usando facetas por pais
ggplot(data=VariosPaises_Covid19)+
  geom_point(mapping=aes (x=FECHA, y=CONTAGIADOS, color=COUNTRY))+
  facet_wrap(~COUNTRY, nrow = 3)
ggplot(data=VariosPaises_Covid19)+
  geom_point(mapping=aes (x=FECHA, y=FALLECIDOS, color=COUNTRY))+
  facet_wrap(~COUNTRY, nrow = 3)
ggplot(data=VariosPaises_Covid19)+
  geom_point(mapping=aes (x=FECHA, y=HOSPITALIZADOS, color=COUNTRY))+
  facet_wrap(~COUNTRY, nrow = 3)
ggplot(data=VariosPaises_Covid19)+
  geom_point(mapping=aes (x=FECHA, y=UCIs, color=COUNTRY))+
  facet_wrap(~COUNTRY, nrow = 3)
ggplot(data=VariosPaises_Covid19)+
  geom_point(mapping=aes (x=FECHA, y=IP, color=COUNTRY))+
  facet_wrap(~COUNTRY, nrow = 3)
ggplot(data=VariosPaises_Covid19)+
  geom_point(mapping=aes (x=FECHA, y=R0_14, color=COUNTRY))+
  facet_wrap(~COUNTRY, nrow = 4)
#7. Los cargo todos en una archivo para guardarlos:
write.csv(VariosPaises_Covid19, "VariosPaises_Covid19_Corregido.csv")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#8.Selecciono un pais, por ejemplo Spain y compruebo los datos, 
# limpiamos si es necesario
Spain<-filter(VariosPaises_Covid19, COUNTRY=="SPAIN")
Netherland<-filter(VariosPaises_Covid19, COUNTRY=="NETHERLAND")
Peru<-filter(VariosPaises_Covid19, COUNTRY=="PERU")
Colombia<-filter(VariosPaises_Covid19, COUNTRY=="COLOMBIA")
Brazil<-filter(VariosPaises_Covid19, COUNTRY=="BRAZIL")
Mexico<-filter(VariosPaises_Covid19, COUNTRY=="MEXICO")
ggplot(data=Spain)+
  geom_point(mapping=aes (x=FECHA, y=R0_14, color=COUNTRY))+
  geom_smooth(mapping=aes (x=FECHA, y=R0_14))
ggplot(data=Netherland)+
  geom_point(mapping=aes (x=FECHA, y=R0_14, color=COUNTRY))+
  geom_smooth(mapping=aes (x=FECHA, y=R0_14))
ggplot(data=Spain)+
  geom_point(mapping=aes (x=FECHA, y=IP, color=COUNTRY))+
  geom_smooth(mapping=aes (x=FECHA, y=IP))
ggplot(data=Netherland)+
  geom_point(mapping=aes (x=FECHA, y=IP, color=COUNTRY))+
  geom_smooth(mapping=aes (x=FECHA, y=IP))
#9. Comprobamos que no hay datos erroneos

attach(Spain)
boxplot(CONTAGIADOS, FALLECIDOS, UCIs, HOSPITALIZADOS, RECUPERADOS)
plot(CONTAGIADOS, main="Contagios", type="l", col="blue")
plot(FALLECIDOS, main="Fallecidos", type="l", col="black")
plot(UCIs, main="Cuidados Intensivos", type="l", col="orange")
plot(HOSPITALIZADOS, main="Hospitalizados", type="l", col="red")
plot(RECUPERADOS, main="Recuperados", type="l", col="green")
plot(IP, main="Indicador de peligro", type="l", col="red")
plot(R0_14, main="Indice R0 a 14 dias", type="l", col="orange")
plot(R0_7, main="Indice R0 a 7 dias", type="l", col="orange")
plot(R0_3, main="Indice R0 a 3 dias", type="l", col="orange")

detach(Spain)
#la correlacion entre IP y R0 a 14 dias en el periodo
Spain$FECHA[15:40] #fin del estado de confinamiento total
# es de mas de un 80% lo que demuestra la efectividad del confinamiento
#porque a partir de ese momento el R0 y el IP se hacen muy bajos
cor(Spain$IP[15:40],Spain$R0_14[15:40])




#CALCULO DEL INDICADOR DE PELIGROSIDAD:

#asocio la bbdd para no usar $

attach(VariosPaises_Covid19)

IP<-(CONTAGIADOS+FALLECIDOS)-RECUPERADOS
VariosPaises_Covid19$RECUPERADOS<-as.double(VariosPaises_Covid19$RECUPERADOS)
VariosPaises_Covid19<-mutate(VariosPaises_Covid19, IP=CONTAGIADOS+FALLECIDOS-RECUPERADOS)



#Indice de peligrosidad de un solo pais: ejemplo Peru
Peru<-filter(VariosPaises_Covid19, COUNTRY=="PERU", months(FECHA)%in%c("abril", "mayo", "junio"))
Spain<-filter(VariosPaises_Covid19, COUNTRY=="SPAIN", months(FECHA)%in%c("abril", "mayo", "marzo"))
Colombia<-filter(VariosPaises_Covid19, COUNTRY=="COLOMBIA",months(FECHA)%in%c("julio", "junio"))
# RESUMEN DE LOS DATOS PARA SPAIN:
Spain<-filter(VariosPaises_Covid19, COUNTRY=="SPAIN")
#voy a limpiar unos datos erroneos en Spain
for (i in 6:8) {Spain[i,7]<-144}
#tengo que recalcular IP en spain
detach(VariosPaises_Covid19)
attach(Spain)
IP<-(CONTAGIADOS+FALLECIDOS)-RECUPERADOS

Spain_R0_1<-R0(Spain$CONTAGIADOS,2)
Spain_R0_7<-R0(Spain$CONTAGIADOS,8)
Spain_R0_14<-R0(Spain$CONTAGIADOS,15)
Spain<-data.frame(Spain, Spain_R0_1, Spain_R0_7, Spain_R0_14)
Spain<-limpiar(Spain, 1, 132, 9, 11,0)
write.csv(Spain, "spain.csv")
Spain$FECHA<-as.Date(Spain$FECHA, "%d/%m/%y")
Spain_mam<-filter(Spain, months(FECHA)%in%c("abril", "mayo", "marzo"))
Spain_ma<-filter(Spain, months(FECHA)%in%c("abril", "marzo"))
days<-1:length(Spain_ma $FECHA)
ggplot(data=Spain_ma )+
  geom_point(mapping=aes (x=days, y=IP), color="black")+
  geom_smooth(mapping=aes (x=days, y=IP, col="red"))
ggplot(data=Spain_ma )+
  geom_point(mapping=aes (x=days, y=Spain_R0_14), color="black")+
  geom_smooth(mapping=aes (x=days, y=Spain_R0_14))

#relacion entre R0 e IP
ggplot(data=Spain_ma )+
  geom_point(mapping=aes (y=IP, x=Spain_R0_7), color="black")+
  geom_smooth(mapping=aes (y=IP, x=Spain_R0_7))
cor(Spain_ma$IP, Spain_ma$Spain_R0_14)
cor(Spain_mam$IP, Spain_mam$Spain_R0_14)
cor(Spain$IP, Spain$Spain_R0_14)
plot(Spain$IP)
plot(Spain$FALLECIDOS)

ggplot(data=dfspain)+
  geom_point(mapping=aes (x=Spain.FECHA, y=Spain_R0_1), color="green")+
  geom_smooth(mapping=aes (x=Spain.FECHA, y=Spain_R0_1), color="green")+
  geom_point(mapping=aes (x=FECHA, y=Spain_R0_14 ),color="red")+
  geom_smooth(mapping=aes (x=FECHA, y=Spain_R0_14), color="red")


summary(CONTAGIADOS)
summary(FALLECIDOS)
summary(RECUPERADOS)
summary(HOSPITALIZADOS)
summary(UCIs)

#REPRESENTACIÓN DEL INDICE DE PELIGROSIDAD POR PAISES:
#para Spain

ggplot(data=Spain)+
  geom_point(mapping=aes (x=FECHA, y=IP, color=COUNTRY))+
  geom_smooth(mapping=aes (x=FECHA, y=IP))
#eliminamos los datos atipicos, negativos
d<-dim(Spain)
for (i in 1:d[1]) 
  for (j in 3:d[2]) {
    if (Spain[i,j]<(-10000)){Spain[i,j]<-0}
  }
ggplot(data=Spain)+
  geom_point(mapping=aes (x=FECHA, y=IP, color=COUNTRY))+
  geom_smooth(mapping=aes (x=FECHA, y=IP))

#REPRESENTACIÓN DEL INDICE DE PELIGROSIDAD POR PAISES:
#para Peru
ggplot(data=Peru)+
  geom_point(mapping=aes (x=FECHA, y=IP, color=COUNTRY))+
  geom_smooth(mapping=aes (x=FECHA, y=IP))
#REPRESENTACIÓN DEL INDICE DE PELIGROSIDAD POR PAISES:
#para Colombia
ggplot(data=Colombia)+
  geom_point(mapping=aes (x=FECHA, y=IP, color=COUNTRY))+
  geom_smooth(mapping=aes (x=FECHA, y=IP))



ggplot(data=VariosPaises_Covid19Amplia)+
  geom_point(mapping=aes (x=FECHA, y=IP, color=COUNTRY))+
  facet_wrap(~COUNTRY, nrow = 3)


ggplot(data=VariosPaises_Covid19)+
  geom_point(mapping=aes (x=FECHA, y=CONTAGIADOS, color=COUNTRY))+
  geom_smooth(mapping=aes (x=FECHA, y=CONTAGIADOS))


SN<-filter(VariosPaises_Covid19, (COUNTRY=="SPAIN")|(COUNTRY=="NETHERLAND"))
ggplot(data=SN)+
  geom_point(mapping=aes (x=FECHA, y=CONTAGIADOS, color=COUNTRY))+
  geom_smooth(mapping=aes (x=FECHA, y=CONTAGIADOS, linetype=COUNTRY))


attach(VariosPaises_Covid19)
FECHA<-as.Date(FECHA, "%d/%m/%y")
MARZO2020<-filter(VariosPaises_Covid19,months(FECHA)=="marzo")

MARZO2020<-filter(MARZO2020,(COUNTRY!="SPAIN"),(COUNTRY!="NETHERLAND"))
MARZO2020<-filter(MARZO2020,(COUNTRY!="BRAZIL"),(COUNTRY!="GUATEMALA"))
ggplot(data=MARZO2020)+
  geom_point(mapping=aes (x=FECHA, y=CONTAGIADOS, color=COUNTRY))
  

ggplot(data=MARZO2020)+
  geom_point(mapping=aes (x=FECHA, y=CONTAGIADOS, color=COUNTRY))+
  geom_smooth(mapping=aes (x=FECHA, y=CONTAGIADOS, linetype=COUNTRY)) 
