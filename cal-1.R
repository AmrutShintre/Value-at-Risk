## program to illustrate calculating various concepts of VaR
##  using a long positive on $10 million IBM
##  based on daily log returns of IBM from July 3, 62 to Dec 31, 98
## Based on RiskMetrics

## get in the data, which is already in log returns
ibm = scan("d-ibm-log-6298.txt")
T = length(ibm)
head(ibm)
plot(ibm,type='l')

source('IGarch.R')
m1=Igarch(ibm,include.mean=F,volcnt=F)
names(m1)
b_beta = as.numeric(m1$par)
ibm[T]   # The log returns are in percentages.
v1=m1$volatility
v1[T]
vt=(1-b_beta)*ibm[T]^2 + b_beta*v1[T]^2
vt
sqrt(vt)
qnorm(0.95)*sqrt(vt)
qnorm(0.99)*sqrt(vt)
qnorm(0.999)*sqrt(vt)

## Expected shortfall calculation
ES = dnorm(qnorm(.99))*sqrt(vt)/0.01

