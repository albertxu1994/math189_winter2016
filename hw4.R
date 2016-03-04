gauge <- read.csv("gauge.txt", sep="")
gain <- gauge$gain
densities <- gauge$density

gainAvg <- NULL
#densityAvg <- NULL
for(i in 0:8){
  gainAvg<-c(gainAvg, mean(gauge$gain[seq(from=(i*10)+1, to=(i+1)*10)]))
  # densityAvg<-c(densityAvg, mean(gauge$density[seq(from=(i*10)+1, to=(i+1)*10)]))
}
densityDedup <- gauge$density[seq(1,length(gauge$density),by=10)]
linearModel <- lm(I(log(gainAvg)) ~ (densityDedup))

# plots of qq, std residuals, and the fit
plot(densityDedup, log(gainAvg))
abline(coef(linearModel), col="red")

plot(resid(linearModel))
abline(0,0, col="red")

#not sure what's up here:
plot(linearModel)