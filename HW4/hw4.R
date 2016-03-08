gauge <- read.table("gauge.txt", header=T)

avgGain <- sapply(unique(gauge$density), FUN=function(x) { mean(gauge[gauge$density == x, "gain"]) })
library(ggplot2)
avgGauge <- data.frame(density=unique(gauge$density), gain=avgGain)
avgGauge$logGain <- log(avgGauge$gain)
linearModel <- lm(logGain ~ density, data=avgGauge)
linModPredict <- predict(linearModel, interval="prediction", se.fit=T, level=0.95)
avgGauge <- data.frame(avgGauge, linModPredict$fit)

# extract values from the linear model
yIntercept <- summary(linearModel)$coefficients[1,1]
yInterceptSE <- summary(linearModel)$coefficients[1,2]
slope <- summary(linearModel)$coefficients[2,1]
slopeSE <- summary(linearModel)$coefficients[2,2]

logGainDensityPlot <- ggplot(avgGauge, aes(x=density, y=logGain)) + 
  geom_point() +
  geom_smooth(aes(fill="Confidence"), method="lm", fullrange=T, level=0.95, alpha=0.3) +
  geom_ribbon(aes(y=fit, ymin=lwr, ymax=upr, fill="Prediction"), alpha=0.15) +
  scale_fill_manual("Interval", values=c("red", "blue")) +
  theme_bw() +
  labs(x="Density", y="Log Transformed Average Gain", title="Log Transformed Average Gain vs Density") +
  geom_abline(intercept=yIntercept - qt(.975, df=7)*yInterceptSE, slope=slope + qt(.975, df=7)*slopeSE, colour="red") +
  geom_abline(intercept=yIntercept + qt(.975, df=7)*yInterceptSE, slope=slope - qt(.975, df=7)*slopeSE, colour="red")
#ggsave("logGainDensity.png", plot=logGainDensityPlot)

residualsPlot <- ggplot(linearModel, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_smooth(se=F) +
  geom_hline(yintercept=0, col="red") +
  scale_y_continuous(limits=c(-2,2)) +
  theme_bw() +
  labs(title="Residual vs Fitted Values", x="Fitted Values", y="Residuals")
#ggsave("residualsPlot.png", plot=residualsPlot)

# TODO qq plot
# TODO cross validation

## plots of qq, std residuals, and the fit
#plot(avgGauge$density, avgGauge$logGain)
#abline(coef(linearModel), col="red")
#
#plot(resid(linearModel))
#abline(0,0, col="red")
#
##not sure what's up here: doesn't work
#plot(linearModel)
