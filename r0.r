R0<-function(x,inc){
  valor<-0
  for (i in inc:length(x)){
    valor[i]<-x[i]/x[i-inc+1]
  }
  return(valor)
}