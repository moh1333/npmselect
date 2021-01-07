select_model_default = function(x, y, X, I, verb = FALSE){
  # Setup and default values
  x = as.matrix(x)
  n = dim(x)[1]
  d = dim(x)[2]
  cs = prod(apply(x,2,IQR))
  ct = 0.5
  idx = seq(1/n,1,length.out = n)
  tau = log(n)*n^(-(d+3)/(d+4))
  gic = rep(Inf,4)
  if(verb){
    rss = rep(0,4)
    df = rep(0,4)
  }
  
  # Get GIC val for Model 1
  sband_1 = cs*n^(-1/(d+5))
  tband_1 = ct*n^(-1/(d+5))
  y_hat = model_1(x, y, sband_1, tband_1)
  if(verb){
    vals = GIC(x, idx, y, y_hat, X, I, 1, tau, sband_1, tband_1, TRUE)
    gic[1] = vals$gic
    rss[1] = vals$rss
    df[1] = vals$df
  } else {
    gic[1] = GIC(x, idx, y, y_hat, X, I, 1, tau, sband_1, tband_1)
  }
  
  # Get GIC val for Model 2
  sband_2 = cs*n^(-1/(d+4))
  y_hat = model_2(x, y, sband_2)
  if(verb){
    vals = GIC(x, idx, y, y_hat, X, I, 2, tau, sband_2, 0, TRUE)
    gic[2] = vals$gic
    rss[2] = vals$rss
    df[2] = vals$df
  } else {
    gic[2] = GIC(x, idx, y, y_hat, X, I, 2, tau, sband_2, 0)
  }
  
  # Get GIC val for Model 3
  tband_3 = ct*n^(-0.2)
  y_hat = model_3(x, y, tband_3)
  if(verb){
    vals = GIC(x, idx, y, y_hat, X, I, 3, tau, 0, tband_3, TRUE)
    gic[3] = vals$gic
    rss[3] = vals$rss
    df[3] = vals$df
  } else {
    gic[3] = GIC(x, idx, y, y_hat, X, I, 3, tau, 0, tband_3)
  }
  
  # Get GIC val for Model 4
  y_hat = model_4(x, y)
  if(verb){
    vals = GIC(x, idx, y, y_hat, X, I, 4, tau, 0, 0, TRUE)
    gic[4] = vals$gic
    rss[4] = vals$rss
    df[4] = vals$df
  } else {
    gic[4] = GIC(x, idx, y, y_hat, X, I, 4, tau, 0, 0)
  }
  
  selected = which.min(gic)
  if(verb){
    return(list(selected = selected, gic = gic, rss = rss, df = df))
  } else {
    return(selected)
  }
}
