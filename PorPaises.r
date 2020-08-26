library(ggplot2)
library(dplyr)
datos_covid<-VariosPaises_Covid19_Corregido
datos_covid.Spain<-filter(datos_covid, COUNTRY=="SPAIN")
ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=Day, y=HOSPITALIZADOS))+
  geom_smooth(mapping=aes (x=Day, y=HOSPITALIZADOS))

ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=Day, y=RECUPERADOS))+
  geom_smooth(mapping=aes (x=Day, y=RECUPERADOS), color="green")

ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=Day, y=CONTAGIADOS))+
  geom_smooth(mapping=aes (x=Day, y=CONTAGIADOS), color="orange")

ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=Day, y=FALLECIDOS))+
  geom_smooth(mapping=aes (x=Day, y=FALLECIDOS), color="black")
ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=Day, y=UCI))+
  geom_smooth(mapping=aes (x=Day, y=UCI), color="red")

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
