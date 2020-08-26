R0<-function(x,inc){
  #esta funcion recibe los datos y el numero de dias
  # que se usan de incremento para hacer el ratio de la diferencia
  #entre los contagiados
  valor<-0
  for (i in inc:length(x)){
    valor[i]<-x[i]/x[i-inc+1]
  }
  return(valor)
}