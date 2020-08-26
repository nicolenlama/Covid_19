suaviza<-function(datos, valormaximo){
  for (i in (2:(length(datos)-1))){
    if (datos[i]>valormaximo){datos[i]<-(datos[i-1]+datos[i+1])/2}
  }
  return(datos)
}