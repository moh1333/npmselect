epk = function(x, band){
  y = x/band
  return((0.75/band)*(1-y^2)*(abs(y)<=1))
}
