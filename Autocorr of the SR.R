install.packages('Ecdat')
install.packages('fGarch')

#clean the workspace
rm(list=ls())

#step a
library('fGarch')
library('Ecdat')
data(SP500,package = 'Ecdat')
ret1<-SP500$r500[1306:1805]

#step b
fit1<- garchFit( formula = ~garch(1,1), data = ret1, trace=FALSE)
sigma1<-sqrt(fit1@h.t)

#step c
plot(sigma1, type='l')

#step d
con1<-which(SP500[1:1805,1]<=-0.228)
prob1<-length(con1)/length(SP500[1:1805,1])


#step e
retstand<-ret1/sigma1
acf(ret1^2,20)
acf(retstand^2,20)

#step f
qqnorm(retstand)
qqline(retstand, col='red')

#step g
fit=garchFit(formula = ~garch(1,1), data=SP500$r500, cond.dist='std')



