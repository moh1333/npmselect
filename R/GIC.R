GIC = function(x, index, y, y_hat, X, I, model_num, tau, sband, tband, verb = FALSE){
  rss = rss_n(x, index, y, y_hat, X, I)
  df = 0
  x = as.matrix(x)
  n = dim(x)[1]
  d = dim(x)[2]
  
  if(model_num == 1){
    df = prod(2*apply(x,2,IQR)) / (tband*sband^d)
  } else if(model_num == 2){
    df = prod(2*apply(x,2,IQR)) / (sband^d)
  } else if(model_num == 3){
    df = (d+1) / tband
  } else if(model_num == 4){
    df = d + 1
  }
  gic = log(rss/n)+tau*df
  if(verb){
    return(list(gic = gic, rss = rss, df = df))
  } else {
    return(gic)
  }
}
