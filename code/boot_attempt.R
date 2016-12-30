# K.Palof 
# work on bootstrap for Scallop Survey 2016
library(boot)
#library(broom)
#library(purrr)

# trial1: indiv bed ----
catch.area %>% filter(Bed == "KSH1") %>%
  filter(size_class==1|di==0) -> catch.area.KSH1.L 

#want to get dbar and N from each boostrap
db <- function(data) {
  d <- data
  d_bar <- (1/mean(d$n)*sum(d$di))
  N=mean(d$area_nm2)*d_bar
  parms <- c(d_bar, N)
  return(parms)
}
db.fun <- function(d,i){
  db(d[i,])
}
results.1 <- boot(data = catch.area.KSH1.L, db.fun, 
                R=1000)
plot(results.1, index=1)
boot.ci(results.1, conf = 0.95)


#   want to get dbar and N from each boostrap
source("./code/boot_fnc.R")
results.2 <- boot(data = catch.area.KSH1.L, db.fun2, 
                 R=1000)
plot(results.2, index=1)
plot(results.2, index =2)
boot.ci(results.2, conf = 0.95, index =1)
boot.ci(results.2, conf = 0.95, index =2)

# bootstrap by BED - this is for ALL scallops - large and small
boot_all <- lapply(c.a.bedlist, boot, statistic = db.fun2, R=1000)

lapply(boot_all, boot.ci, index =1)
plot(boot_all$KSH1, index =1)

#boot By Bed and Size class ...1 and 2 for each bed
boot_all2 <- lapply(c.a.bedlist2, boot, statistic = db.fun2, R=1000)

lapply(boot_all2, boot.ci, index =1)
plot(boot_all2$KSH1.1, index =1)

# other methods that weren't working.
# Doesn't work
#boot2 <- catch.area.KSH1.L %>% modelr::bootstrap(10)%>%
 # mutate(dbar = map_dbl(strap, ~ ((1/.$n*sum(.$di)))))

# why are there 5200 rows??? not right
#boot.KSH1 <- catch.area.KSH1.L %>% bootstrap(100)%>%
#  do(data.frame(dbar=(1/.$n*sum(.$di))))
  
