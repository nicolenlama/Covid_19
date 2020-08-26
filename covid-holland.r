#transformacion de los datos de Holanda
#primero rescato la base de datos en csv
#y cargo las librerias
library(dplyr)
library(ggplot2)
datos<-COVID_19_Holland
fecha<-as.Date(COVID_19_Holland$Date_of_report)
dato<-data.frame(fecha,datos)
dato<-group_by(dato, fecha)
datos_holanda<-summarize(dato, 
                         count=n(),
                         fallecidos=sum(Deceased, na.rm=T),
                         hospitalizados=sum(Hospital_admission, na.rm = T),
                         contagiados=sum(Total_reported, na.rm = T))
#grabo los datos acumulados de Paises Bajos
write.csv(datos_holanda, "datos_holanda_acum.csv")
#ahora hago la base de datos sin acumular, restando las columnas
rm(list=c("contagiados", "fallecidos", "hospitalizados"))
attach(datos_holanda)
c<-contagiados[1]
h<-hospitalizados[1]
f<-fallecidos[1]
for (i in 2:137){
  f[i]<-fallecidos[i]-fallecidos[i-1]
  c[i]<-contagiados[i]-contagiados[i-1]
  h[i]<-hospitalizados[i]-hospitalizados[i-1]
}

detach(datos_holanda)
diarios_holanda<-data.frame(datos_holanda$fecha, f, c, h)
#guardo los datos diarios de Paises Bajos
library(readr)
write_excel_csv(diarios_holanda,"datos_holanda_diarios2.csv")
