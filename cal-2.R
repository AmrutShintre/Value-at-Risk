## program to illustrate calculating various concepts of VaR
##  using a long positive on $10 million IBM
##  based on daily log returns of IBM from July 3, 62 to Dec 31, 98
## Based on Econometric modeling

## get in the data, which is already in log returns
ibm = scan("d-ibm-log-6298.txt")
T = length(ibm)
head(ibm)

## econometric modeling
library(fGarch)
acf(ibm)
pacf(ibm)

## try Gaussian GARCH(1,1)
m2 = garchFit(~arma(2,0)+garch(1,1),data=ibm,trace=F)
summary(m2)
predict(m2,1)
pre = as.numeric(predict(m2,1))
pre[1] - qnorm(0.95)*pre[3]  # VaR for p = 0.05
pre[1] + dnorm(qnorm(.95))*pre[3]/0.05 # ES for p = 0.05
pre[1] - qnorm(0.99)*pre[3]  # VaR for p = 0.01
pre[1] + dnorm(qnorm(.99))*pre[3]/0.01 # ES for p = 0.01
pre[1] - qnorm(0.999)*pre[3]  # VaR for p = 0.001

## try GARCH(1,1) with Student-t
m3 = garchFit(~arma(2,0)+garch(1,1),data=ibm,cond.dist='std',trace=F)
summary(m3)
predict(m3,1)
pre = as.numeric(predict(m3,1))
t_df = 6.41736
pre[1] - qt(0.95,df=t_df)*pre[3]/sqrt(t_df/(t_df-2))  # VaR for p = 0.05
pre[1] - qt(0.99,df=t_df)*pre[3]/sqrt(t_df/(t_df-2))  # VaR for p = 0.01
pre[1] - qt(0.999,df=t_df)*pre[3]/sqrt(t_df/(t_df-2))  # VaR for p = 0.001

## Alernatively we can use qstd function in RMetrics without adjusting for variance
pre[1] - qstd(0.95,nu=t_df)*pre[3]  # VaR for p = 0.05
pre[1] - qstd(0.99,nu=t_df)*pre[3]  # VaR for p = 0.01
pre[1] - qstd(0.999,nu=t_df)*pre[3]  # VaR for p = 0.001

## And ES for p = 0.01 can be easily calculated as
q1 = qstd(0.99,nu=t_df); d1 = dstd(q1,nu=t_df)
ES3 = pre[1] - d1*((t_df-2)+q1^2)*pre[3]/(0.01*(t_df-1))
