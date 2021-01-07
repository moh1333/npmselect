model_1 = function(x, y, sband, tband, skern = 'e', tkern = 'e'){
  # Kernel choice setup
  if(skern=='e'){
    skern_fun = epk
  }
  
  # Create estimated and index vectors
  n = length(y)
  y_hat = rep(0,n)
  index = seq(1,n)/n
  x = as.matrix(x)
  
  # Fit values
  for(i in 1:n){
    # Generate modified response vectors for time-LLRs
    z = skern_fun(apply(x,1,function(w) norm(as.matrix(w-x[i,]),'F')), sband)
    z_prime = y*z

    # Generate time-LLRs
    f_hat_xi = npregress(index, z, bandwidth = tband, kernel = tkern, control.par = list(degree = 1))
    t_hat_xi = npregress(index, z_prime, bandwidth = tband, kernel = tkern, control.par = list(degree = 1))
    
    # Peel off fitted value at x_i
    y_hat[i] = t_hat_xi$fitted[i]/f_hat_xi$fitted[i]
  }
  return(y_hat)
}
