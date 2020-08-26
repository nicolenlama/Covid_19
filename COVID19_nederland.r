
datos_covid<-Covid_19_Milena[1:47,1:11]
library(ggplot2)
library(dplyr)
datos_covid<-rename(datos_covid, Day=INDEX)
ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=Day, y=H))+
  geom_smooth(mapping=aes (x=Day, y=H))

ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=Day, y=R ))+
  geom_smooth(mapping=aes (x=Day, y=R ), color="green")

ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=Day, y=INF))+
  geom_smooth(mapping=aes (x=Day, y=INF), color="orange")

ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=Day, y=Lost))+
  geom_smooth(mapping=aes (x=Day, y=Lost), color="black")
ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=Day, y=U))+
  geom_smooth(mapping=aes (x=Day, y=U), color="red")
attach(datos_covid)
res<-INF-(Lost+R)
gr_res<-ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=Day, y=res))+
  geom_smooth(mapping=aes (x=Day, y=res), color="red")
gr_res + xlab("9 March-24 April")+  ylab( "arrivals-departures") +
  ggtitle("Danger indicator:")        
         
         
         
         
         
         
         
         
         
         
         
         
)