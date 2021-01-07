model_3 = function(x, y, band, kern = 'e'){
  # Kernel choice setup
  if(kern == 'e'){
    kern_fun = epk
  }

  n = length(y)
  y_hat = rep(0,n)
  idx = seq(1,n)/n
  
  for(i in 1:n){
    w = kern_fun(idx - i/n, band)
    time_mod = lm(y~x, weights = w)
    y_hat[i] = time_mod$fitted.values[i]
  }
  return(y_hat)
}
