sim_study = function(n, phi){
   snr = matrix(nrow = 1000, ncol = 4)
   con_mat = matrix(rep(0,16), nrow = 4, ncol = 4)
   
   X = c(-2,2)
   I = c(0.2,0.8)
   
   # Run 1000 simulations
   for(i in 1:1000){
     x = gen_x(n)
     t = seq(1,n)/n
     
     # Model 1
     m = 2*cospi(t*x)*sinpi(2*t)
     e = rnorm(n, 0, phi*abs(t*x)/2)
     y = m + e

     snr[i,1] = sqrt(sum(m^2)/sum(e^2))
     selected = select_model_default(x,y,X,I)
     con_mat[1,selected] = con_mat[1,selected] + 1
     
     # Model 2
     m = x + exp(x)*sinpi(x)
     e = rnorm(n, 0, phi*t*exp(x/3))
     y = m + e

     snr[i,2] = sqrt(sum(m^2)/sum(e^2))
     selected = select_model_default(x,y,X,I)
     #print(select_model_default(x,y,X,I,TRUE))
     con_mat[2,selected] = con_mat[2,selected] + 1

     # Model 3
     m = 3*t + 5*t*cospi(2*t^2)*x
     e = rnorm(n, 0, phi*abs(t*x))
     y = m + e

     snr[i,3] = sqrt(sum(m^2)/sum(e^2))
     selected = select_model_default(x,y,X,I)
     con_mat[3,selected] = con_mat[3,selected] + 1

     # Model 4
     m = 5 - 2*x
     e = rnorm(n, 0, phi*abs(x + t))
     y = m + e

     snr[i,4] = sqrt(sum(m^2)/sum(e^2))
     selected = select_model_default(x,y,X,I)
     con_mat[4,selected] = con_mat[4,selected] + 1
     
     if(i%%10 == 0){
       print(i)
     }
   }
   
   snr = apply(snr,2,median)
   return(list(snr = snr, con_mat = con_mat))
}