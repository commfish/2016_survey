#K.Palof
#Scallop survey 2016 
#bootstrap function for d_bar (resampling data), gives CI's for dbar and N estimates
#     refer to 0-abundance file for original code
#   want to get dbar and N from each boostrap
db.fun2 <- function(data,i){
  d = data[i,]
  d_bar <- (1/mean(d$n)*sum(d$di))
  N=mean(d$area_nm2)*d_bar
  parms <- c(d_bar, N)
  return(parms)
}