library(ggplot2)
library(YieldCurve)
library()
install.packages("RQuantLib")

nelson_siegel_calculate<-function(theta,tau,beta0,beta1,beta2){
  beta0 + beta1*(1-exp(-theta/tau))/(theta/tau) + beta2*((1-exp(-theta/tau))/(theta/tau) - exp(-theta/tau))
}

###Let's plot the yield curve of NS with the following parameters

ns_data <-
  data.frame(maturity=1:30) %>%
  mutate(ns_yield=nelson_siegel_calculate(theta=maturity,tau=3.3,beta0=0.07,beta1=-0.02,beta2=0.01))

head(ns_data)

ggplot(data=ns_data, aes(x=maturity,y=ns_yield)) + geom_point() + geom_line()


Nelson.Siegel(as.matrix(UST[1:100,c(2:ncol(UST))]),maturity=c(.083333,
                                                              .166666,
                                                              .25,
                                                              .5,
                                                              1,
                                                              2,
                                                              3,
                                                              5,
                                                              7,
                                                              10,
                                                              20,
                                                              30))

names(UST)


install.packages("installr"); library(installr) # install+load installr

updateR() # updating R.