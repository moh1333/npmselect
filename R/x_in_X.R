x_in_X = function(x,X){
  x = as.matrix(x)
  X = as.matrix(X)
  svec = apply(x,1,function(w) all(((w-X[1,])>=0))&((X[2,]-w)>=0))
  return(svec)
}
