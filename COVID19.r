#COVID 19 MODEL TRANSMISSION BASED ON FLOW NETWORK AND SIR FAMILY MODEL

N<-47100396
library(readr)
Covid_19_Victoria <- read_delim("C:/Users/Victoria/Desktop/COVID19/Covid-19-Victoria.csv", 
                                ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                trim_ws = TRUE)
View(Covid_19_Victoria)

#Reducing the dataset to another smaller according to the data collected and variables in use
datos_covid<-Covid_19_Victoria[1:62,1:11] # EL N. DE FILAS SE VA INCREMENTANTO UNO DIARIAMENTE
#creo la funci?n armonica para usar la media armonica al estimar los betas por ser ratios
armonica<-function(x){ #usamos media arm?nica porque los betas1 son ratios
  n<-length(x)
  return(n/sum(1/x))
}
attach(datos_covid)





#calculo de los par?metros BETAS: 
betas1<-INF/Q#frecuencias relativas diarias (o regla de Laplace, favorables aprox.)
plot(betas1)
BETA1<-median(betas1)

#???PARA CALCULAR EL ESTIMADOR DE LAPROBABILIDAD DE INFECCI?N SIENDO PERSONAL SANITARIO, 
# CALCULO EL PERSONAL SANITARIO INFECTADO DIARIO, PORQUE EN EL DATASET VIENE ACUMULADO
K<-length(PSINFAC)
PSINF.DIARIO<-0
for (i in 1:K) {
  PSINF.DIARIO[i]<-PSINFAC[i+1]-PSINFAC[i]
}
PSINF.DIARIO[K]<-PSINFAC[K]
betas2<-PSINF.DIARIO/PS#idem aprox
plot(betas2[1:K-1]) # investigar por qu? sale decreciente
BETA2<-median(betas2[1:K-1]) # un sanitario tiene mucha m?s probabilidad de infecci?n.
#PARA CALCULAR LA PROBABILIDAD DE SER INFECTADO EN EL SECTOR TRABAJADORES, QUE HACEN "VIDA
# NORMAL", LO QUE HAGO ES APLICAR UN NIVEL DE TRANSMISI?N DE BETA 0=2 QUE ES EL MINIMO (ESTA 
#ENTRE DOS Y TRES, Y PARA LOS INFECTADOS HOY CALCULO LOS INFECTADOS MA?ANA SEG?N ESE NIVEL DE TRANSMISI?N)
#TRABAJADORES INFECTADOS DE ACUERDO CON EL NIVEL DE TRANSMISI?N 2
BETA0<-2
#betas0<- lo dejo pendiente de momento porque lo tengo que pensar mejor
#aproximacion seg?n el ratio 25% de infectados (corresponde al 25% de la poblacion)
#dividido entre la poblaci?n total de trabajadores esenciales o en activo
betas3<-INF/(4*TE)
plot(betas3)
BETA3<-median(betas3) #esto lo he actualizado a fecha 11 de abril con datos del gobierno en tv 

# de momento no voy a calcular nada con el beta0
# por que a la vista de los datos creo que es m?s acertado usar los betas1-3



#calculo de los alfas:
#------------------------LO DEJO PARA MAS TARDE------------------------

alfas1<-0 #no se puede calcular sin tener los datos personales de los fallecidos
alfas2<-0
for (i in 2:length(f)) {
  alfas2[i]<-H[i]/I[i-1]
}
ALFA2<-armonica(alfas2)
mean(alfas2)
#-------------------------SIN TERMINAR --------------------------------


#-------------------REPRESENTACION DE LOS DATOS-------------------------

#---------------COMPARATIVAS ENTRE LAS VARIABLES-----------------------
ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=I, y=H))+
  geom_smooth(mapping=aes (x=I, y=H))
ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=I, y=f))+
  geom_smooth(mapping=aes (x=I, y=f), color="red")
ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=I, y=R))+
  geom_smooth(mapping=aes (x=I, y=R), color="GREEN")
ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=I, y=f+R))+
  geom_smooth(mapping=aes (x=I, y=f+R), color="PINK")

#--------------DATOS DE EVOLUCION---------------------------------

ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=INDEX, y=HOSPITALIZADOS))+
  geom_smooth(mapping=aes (x=INDEX, y=HOSPITALIZADOS))

ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=INDEX, y=RECUPERADOS))+
  geom_smooth(mapping=aes (x=INDEX, y=RECUPERADOS), color="green")

ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=INDEX, y=CONTAGIADOS))+
  geom_smooth(mapping=aes (x=INDEX, y=CONTAGIADOS), color="orange")

ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=INDEX, y=FALLECIDOS))+
  geom_smooth(mapping=aes (x=INDEX, y=FALLECIDOS), color="black")
ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=INDEX, y=UCI))+
  geom_smooth(mapping=aes (x=INDEX, y=UCI), color="red")

#lo que realmente interesa es que los nodos sumidero reciban m?s llegadas que el nodo fuente
attach(datos_covid)
res<-CONTAGIADOS-(FALLECIDOS+RECUPERADOS)
gr_res<-ggplot(data=datos_covid)+
  geom_point(mapping=aes (x=INDEX, y=res))+
  geom_smooth(mapping=aes (x=INDEX, y=res), color="red")
gr_res + xlab("dias (9 marzo-18 abril)")+  ylab( "entradas-salidas") +
  ggtitle("Indicador de peligrosidad:")
length(CONTAGIADOS)
#An?lisis operacional, seria mejor hacerlo por ventanas de 3 d?as por ejemplo
tasallegadas<-mean(I) #el grafo se puede simplificar a una ?nica fuente: Infectados
tasasalidas<-mean(R+f) #dos nodos sumidero, recuperados y fallecidos
tiempoServicioHospital

