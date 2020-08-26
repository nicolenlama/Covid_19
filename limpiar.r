
#esta funcion SUSTITUYE los NA, NAN e INf POR X 
limpiar<-function(matriz, i1, i2, j1, j2, x) {
  for (i in i1:i2){
    for (j in j1:j2){
      if (is.na(matriz[i,j])|is.nan(matriz[i,j])|is.infinite(matriz[i,j])){matriz[i,j]<-x}
    }
  }
  return(matriz)
}
