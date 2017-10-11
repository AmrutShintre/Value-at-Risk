## program to illustrate calculating various concepts of VaR
##  using a long positive on $10 million IBM
##  based on daily log returns of IBM from July 3, 62 to Dec 31, 98
## Empirical quantile

## get in the data, which is already in log returns
ibm = scan("d-ibm-log-6298.txt")
T = length(ibm)
head(ibm)

### Empirical quantile
nibm=-ibm
quantile(nibm,c(.95,.99))
q1 = as.numeric(quantile(nibm,c(.95,.99,.999)))

## ES
idx=c(1:T)[nibm > q1[1]]
mean(nibm[idx])
idx=c(1:T)[nibm > q1[2]]
mean(nibm[idx])
