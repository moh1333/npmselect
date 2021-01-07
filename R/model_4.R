model_4 = function(x, y){
  lin_model = lm(y~x)
  y_hat = lin_model$fitted.values
  return(y_hat)
}
