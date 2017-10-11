## program to illustrate calculating various concepts of VaR
##  using a long positive on $10 million IBM
##  based on daily log returns of IBM from July 3, 62 to Dec 31, 98
## Traditional EVT

## get in the data, which is already in log returns
ibm = scan("d-ibm-log-6298.txt")
T = length(ibm)
head(ibm)

### Extreme value approach (traditional)
library(evir)
nibm = -ibm
m2=gev(nibm,block=21)
m1 = gev(ibm,block=21)
m2.par = m2$par.ests
m2.par/m2$par.ses

## model checking
plot(m2)

ymax = max(m2$data)
cat('Prob. that the drop will exceed the maximum',ymax,'is',
 1-pgev(ymax,m2.par[1],m2.par[3],m2.par[2]),'\n')


## function calculating VaR using GEV
evtVaR <- function(xi,sigma,mu,n=21,prob=0.01){
# Comput VaR using the block maximum.
# sigma: scale parameter (It is the alpha_n in the textbook)
# mu: location parameter (It is the beat_n in the textbook)
#      For long position: mu = -beta_n
#      For short position: mu = beta_n
# xi: shape parameter (It is the -k_n of the textbook)
# n: block size
# p: tail probability
if (abs(xi) < 0.00000001)
VaR = mu + sigma*log((-n)*log(1-prob))
if (abs(xi) >= 0.00000001){
v1=1.0-(-n*log(1.0-prob))^{-xi}
VaR = mu - (sigma/xi)*v1
}
print(VaR)
evtVaR<-list(VaR = VaR)
} 

evtVaR(m2.par[1],m2.par[2],m2.par[3],21,0.05)
evtVaR(m2.par[1],m2.par[2],m2.par[3],21,0.01)
evtVaR(m2.par[1],m2.par[2],m2.par[3],21,0.001)

## choose a different block size
c_b = 63
m3=gev(nibm,block=c_b)
m3.par = m3$par.ests
m3.par/m3$par.ses

evtVaR(m3.par[1],m3.par[2],m3.par[3],c_b,0.05)
evtVaR(m3.par[1],m3.par[2],m3.par[3],c_b,0.01)
evtVaR(m3.par[1],m3.par[2],m3.par[3],c_b,0.001)



