# load dependencies
library(evir)
library(quantmod)
library(ineq)

# data exploration
getSymbols("BTC-USD", src="yahoo", from="2016-01-01")

chartSeries(`BTC-USD`, type = 'line', theme = 'white')

bitcoin = Ad(`BTC-USD`)

lossreturns = diff(log(bitcoin))
losses = -lossreturns[lossreturns < 0]
returns = lossreturns[lossreturns > 0]
summary(lossreturns)
sd(returns)*100
sd(losses)*100
summary(`BTC-USD`)
# for BTC-USD Adjusted Price, mean and median are close


# plots for returns/losses
hist(lossreturns, breaks = 100, col = 2) # normally dist
hist(returns)
summary(returns)
qplot(returns) # linearity of plot shows returns to be normally distributed
qplot(losses) # one outlier, can conclude losses are also normally distributed
emplot(returns, 'xy') # zipf plot
emplot(losses, 'xy')
meplot(returns) # rough linearity until 5.2%, so this could be our threshold
meplot(losses)

# plots for adj. price
hist(bitcoin)
emplot(bitcoin)
meplot(bitcoin)

MSplot <- function(data,p=4) {
  par(mfrow = c(2, 2)) 
  x=abs(data)
  for (i in 1:p) {
    y=x^i
    S=cumsum(y)
    M=cummax(y)
    R=M/S
    plot(1:length(x),R,type='l', col=2, lwd=3, ylim=c(0,1),xlab='n', ylab='Rn', 
         main=paste("MSplot for p=",i))
  }
  par(mfrow = c(1, 1)) 
  # return(R)
}
MSplot(bitcoin) # all moments converge implying that this is not fat-tailed
MSplot(returns) # third and fourth moments converge but slower
MSplot(losses) # the third and fourth moments do not converge and are undefined
