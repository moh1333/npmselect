model_2 = function(x, y, band, kern = 'e'){
  np_model = npregress(x, y, bandwidth = band, kernel = kern, control.par = list(degree = 1))
  y_hat = np_model$fitted
  return(y_hat)
}
