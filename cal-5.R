## program to illustrate calculating various concepts of VaR
##  using a long positive on $10 million IBM
##  based on daily log returns of IBM from July 3, 62 to Dec 31, 98
## POT, based on GPD

## get in the data, which is already in log returns
ibm = scan("d-ibm-log-6298.txt")
T = length(ibm)
head(ibm)

## Peaks over the threshold approach 
nibm = -ibm

m2=gpd(nibm,threshold=2.5)  # Fit a POT model with threshold of 2.5%
names(m2)
m2
m2$par.ests
m2$par.ests/m2$par.ses

## checking model adequacy
plot(m2)  

## VaR calculation
riskmeasures(m2,c(0.95,0.99,0.999))

m3=gpd(nibm,threshold=5)  # Fit a POT model with threshold of 5%
m3
m3$par.ests
m3$par.ests/m3$par.ses

m3=gpd(nibm,threshold=1)  # Fit a POT model with threshold of 1%
m3
m3$par.ests
m3$par.ests/m3$par.ses
plot(m3)
riskmeasures(m3,c(0.95,0.99,0.999))

m3=gpd(nibm,threshold=3)  # Fit a POT model with threshold of 3%
m3$par.ests
m3$par.ests/m3$par.ses
riskmeasures(m3,c(0.95,0.99,0.999))


