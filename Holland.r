library(ggplot2)
library(dplyr)

datos_covid<-VariosPaises_Covid19_Corregido
datos_covid<-rename(datos_covid, Day=X1)
datos_covid.Holland<-filter(datos_covid, COUNTRY=="NETHERLAND")

Days<-datos_covid.Holland$Day-1596
ggplot(data=datos_covid.Holland)+
  geom_point(mapping=aes (x=Days, y=HOSPITALIZADOS))+
  geom_smooth(mapping=aes (x=Days, y=HOSPITALIZADOS))

ggplot(data=datos_covid.Holland)+
  geom_point(mapping=aes (x=Day, y=RECUPERADOS))+
  geom_smooth(mapping=aes (x=Day, y=RECUPERADOS), color="green")

ggplot(data=datos_covid.Holland)+
  geom_point(mapping=aes (x=Day, y=CONTAGIADOS))+
  geom_smooth(mapping=aes (x=Day, y=CONTAGIADOS), color="orange")

ggplot(data=datos_covid.Holland)+
  geom_point(mapping=aes (x=Day, y=FALLECIDOS))+
  geom_smooth(mapping=aes (x=Day, y=FALLECIDOS), color="black")
ggplot(data=datos_covid.Holland)+
  geom_point(mapping=aes (x=Day, y=UCIs))+
  geom_smooth(mapping=aes (x=Day, y=UCIs), color="red")
ggplot(data=datos_covid.Holland)+
  geom_point(mapping=aes (x=Days, y=IP))+
  geom_smooth(mapping=aes (x=Days, y=IP), color="red")+
 xlab("9 March-x x")+  ylab( "Infected+Lost-Recovered") +
  ggtitle("Danger Index:")
# en Holanda el primer registro parece procedente de datos acumulados
#por ello lo elimino para que se vea mejor la curva de R0, 
datosh<-datos_covid.Holland[-1,]
ggplot(data=datosh)+
  geom_point(mapping=aes (x=Days[-1], y=R0_14))+
  geom_smooth(mapping=aes (x=Days[-1], y=R0_14), color="red")
# como se siguen viendo un par de outliers problematicos, voy a 
# eliminarlos mediante un suavizado, sustituyendo los dos mayores
# outliers por la media de los valores cercanos
suaviza<-function(datos, valormaximo){
  for (i in (2:(length(datos)-1))){
    if (datos[i]>valormaximo){datos[i]<-(datos[i-1]+datos[i+1])/2}
  }
  return(datos)
}
datosh$R0_14<-suaviza(datosh$R0_14,40)
# si solo quiero imprimir los primeros 46 días de datos, puedo 
# reducir los datos de la siguiente forma

datosh<-datos_covid.Holland[2:46,]
ggplot(data=datosh)+
  geom_point(mapping=aes (x=Days[2:46], y=R0_14))+
  geom_smooth(mapping=aes (x=Days[2:46], y=R0_14), color="red")

#datosh<-datos_covid.Holland[2:46,]
datosh$R0_7<-suaviza(datosh$R0_7,40)
ggplot(data=datosh)+
  geom_point(mapping=aes (x=Days[2:46], y=R0_7))+
  geom_smooth(mapping=aes (x=Days[2:46], y=R0_7), color="green")

#lo que realmente interesa es que los nodos sumidero reciban m?s llegadas que el nodo fuente
#MINIMIZATION TO INF AND LOST AND MAXIMIZATION TO RECOVERED, 
#DANGER INDEX PROPOSAL IS (INFECTED+LOST)-RECOVERED, IN MY DATABASE:


res<-CONTAGIADOS+FALLECIDOS-RECUPERADOS
gr_res<-ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=Day, y=res))+
  geom_smooth(mapping=aes (x=Day, y=res), color="red")
gr_res + xlab("9 March-9 May")+  ylab( "Infected+Lost-Recovered") +
  ggtitle("Danger Index:")
#Another alternative as ratio
res<-(CONTAGIADOS+FALLECIDOS)/RECUPERADOS
gr_res<-ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=Day, y=res))+
  geom_smooth(mapping=aes (x=Day, y=res), color="red")
gr_res + xlab("9 March-9 May")+  ylab( "(Infected+Lost)/Recovered") +
  ggtitle("Danger Ration Indicator:")
