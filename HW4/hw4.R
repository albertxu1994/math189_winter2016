library(ggplot2)
gauge <- read.table("gauge.txt", header=T)

avgGain <- sapply(unique(gauge$density), FUN=function(x) { mean(gauge[gauge$density == x, "gain"]) })
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
df <- summary(linearModel)$df[2]

# function to make a point estimate of density and interval around it, given gain
# log(gain) = slope * density + yIntercept
predictDensity <- function(linMod, gain) {
  yIntercept <- summary(linMod)$coefficients[1,1]
  yInterceptSE <- summary(linMod)$coefficients[1,2]
  slope <- summary(linMod)$coefficients[2,1]
  slopeSE <- summary(linMod)$coefficients[2,2]
  df <- summary(linMod)$df[2]
  pointEst <- (log(gain) - yIntercept) / slope
  lowerBound <- (log(gain) - (yIntercept - qt(.975, df=df)*yInterceptSE)) / (slope)
  upperBound <- (log(gain) - (yIntercept + qt(.975, df=df)*yInterceptSE)) / (slope)
  list(logInput=log(gain), estimate=pointEst, lowerBound=lowerBound, upperBound=upperBound)
}

# plot the prediction and confidence intervals
logGainDensityPlot <- ggplot(avgGauge, aes(x=density, y=logGain)) + 
  geom_point() +
  geom_smooth(method="lm", fullrange=T, level=0.95, se=F) +
  geom_ribbon(aes(y=fit, ymin=lwr, ymax=upr), alpha=0.15) +
  scale_fill_manual("Interval", values=c("red", "blue")) +
  theme_bw() +
  labs(x="Density", y="Log Transformed Average Gain", title="Log Transformed Average Gain vs Density")
ggsave("logGainDensity.png", plot=logGainDensityPlot)

# plot the residuals vs the fitted values
residualsPlot <- ggplot(linearModel, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_smooth(se=F) +
  geom_hline(yintercept=0, col="red") +
  scale_y_continuous(limits=c(-2,2)) +
  theme_bw() +
  labs(title="Residual vs Fitted Values", x="Fitted Values", y="Residuals")
ggsave("residualsPlot.png", plot=residualsPlot)

# make QQ plot of residuals
qqPlot <- ggplot(linearModel, aes(qqnorm(.stdresid)[[1]], .stdresid)) +
  geom_point() +
  theme_bw() +
  geom_abline(slope=1, intercept=0, colour="red") +
  labs(title="Normal QQ Plot of the Residuals", x="Theoretical Quantiles", y="Standardized Residuals")
ggsave("qqPlotResiduals.png", plot=qqPlot)

# predict density for gain reading of 38.6 and 426.7
print("actual density for avg reading of 38.55:")
print(avgGauge[avgGauge$gain == 38.55, "density"])
print("predicted density for avg reading of 38.6:")
print(predictDensity(linearModel, 38.6))

print("actual density for avg reading of 426.7:")
print(avgGauge[avgGauge$gain == 426.7, "density"])
print("predicted density for avg reading of 426.7:")
print(predictDensity(linearModel, 426.7))

# cross validation
# remove densities 0.508 and 0.001, and predict the density for an average reading of 38.6
not508 <- gauge[gauge$density != 0.508,]
not508_avgGain <- sapply(unique(not508$density), FUN=function(x) { mean(not508[not508$density == x, "gain"]) })
not508_avgGauge <- data.frame(density=unique(not508$density), gain=not508_avgGain)
not508_avgGauge$logGain <- log(not508_avgGauge$gain)
not508_linearModel <- lm(logGain ~ density, data=not508_avgGauge)
print("predicted density, removing 0.508, for avg reading of 38.6")
print(predictDensity(not508_linearModel, 38.6))

not001 <- gauge[gauge$density != 0.001,]
not001_avgGain <- sapply(unique(not001$density), FUN=function(x) { mean(not001[not001$density == x, "gain"]) })
not001_avgGauge <- data.frame(density=unique(not001$density), gain=not001_avgGain)
not001_avgGauge$logGain <- log(not001_avgGauge$gain)
not001_linearModel <- lm(logGain ~ density, data=not001_avgGauge)
print("predicted density, removing 0.001, for avg reading of 38.6")
print(predictDensity(not001_linearModel, 38.6))

