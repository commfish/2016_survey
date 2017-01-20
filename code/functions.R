####### 2016 Scallop Statewide Survey
####### Ben Williams / Katie Palof 
####### ben.williams@alaska.gov / katie.palof@alaska.gov

# Functions to bootstrap scallop survey by numbers, weight, and meat weight ratio


# Function to summarize - does NOT include bootstrap
f.sum <- function(x){
  # first turn the list to a dataframe
  # use dplyr to summarise each list
  # output is one row all stats.
  
  x = as.data.frame(x)
  x %>%
    group_by(year, District, Bed, variable)%>%
    summarise(n=mean(n),
              area = mean(area_nm2) ,
              dbar = (1/n*sum(di)),
              var_dbar=1/((n)-1)*sum((di-dbar)^2) ,
              cv=sqrt(var_dbar)/dbar*100,
              ss=sum((di-dbar)^2),
              N=area*dbar,
              varN=(area^2)*1/n*1/(n-1)*ss,
              cvN=sqrt(varN)/N*100) -> out
  out
}

# bootstrap ----
# used for numbers and weights-------------
f.it <- function(x){
  # first turn the list to a dataframe
  # extract the identifiers to append to the results
  # function to be run each time for calculating dbar & N
  # function to sample by rows
  # replicate the data 1000 times
  
  x = as.data.frame(x)
  y = x[1,c(2:4,8)]
  boot.it <- function(x){
    d_bar = sum(x$di)/mean(x$n)
    N=mean(x$area_nm2)*d_bar
    c(d_bar, N)
  }
  
  f.do <- function(x){
    x %>% sample_n(nrow(.), replace=TRUE) -> x 
    boot.it(x)
  }
  
  as.data.frame(t(replicate(1000,f.do(x)))) -> out
  names(out) <- c('dbar','N')
  cbind(out,y)
}
