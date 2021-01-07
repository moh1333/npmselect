rss_n = function(x, index, y, y_hat, X, I){
  valid = !is.nan(y_hat)
  rss_vec = ((y-y_hat)^2)[valid]
  svec = x_in_X(x,X)[valid]
  tvec = i_in_I(index,I)[valid]
  rss = sum(rss_vec*svec*tvec)
  return(rss)
}
