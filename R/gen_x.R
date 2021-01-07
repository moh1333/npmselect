gen_x = function(n){
  idx = seq(1,n)/n
  a = (idx - 0.5)^2
  x = rnorm(n, 0, sqrt(1/(1-a)))
  return(x)
}
