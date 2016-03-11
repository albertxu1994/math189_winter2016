# Math 189, Winter 16, Hw 4

# Sherry Diep, sadiep@ucsd.edu, A10598577
# Sandra Hui, s3hui@ucsd.edu, A10551236
# David Lee, dal002@ucsd.edu, A11304749
# Irving Valles, ivalles@ucsd.edu, A09669593
# Albert Xu, a8xu@uscd.edu, A10670971
# Mark Yee, mjyee@ucsd.edu, A11289551

library(ggplot2)
gauge <- read.table("gauge.txt", header=T)

avgGain <- sapply(unique(gauge$density), FUN=function(x) { mean(gauge[gauge$density == x, "gain"]) })
avgGauge <- data.frame(density=unique(gauge$density), gain=avgGain)
avgGauge$logGain <- log(avgGauge$gain)
linearModel <- lm(logGain ~ density, data=avgGauge)
linModPredict <- predict(linearModel, interval="prediction", se.fit=T, level=0.95)
avgGauge <- data.frame(avgGauge, linModPredict$fit)

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

# function to make a point estimate of density and interval around it, given gain
predictDensity <- function(linMod, gain) {
  predict(linMod, newdata=gain, level=0.95, interval="prediction")
}

# predict density for gain reading of 38.6 and 426.7
pred_linearModel <- lm(density ~ logGain, data=avgGauge)
print("actual density for avg reading of 38.55:")
print(avgGauge[avgGauge$gain == 38.55, "density"])
print("predicted density for avg reading of 38.55:")
print(predictDensity(pred_linearModel, data.frame(logGain=log(38.55))))

print("actual density for avg reading of 426.7:")
print(avgGauge[avgGauge$gain == 426.7, "density"])
print("predicted density for avg reading of 426.7:")
print(predictDensity(pred_linearModel, data.frame(logGain=log(426.7))))

# cross validation
# remove densities 0.508 and 0.001, and predict the density for an average reading of 38.6
not508 <- gauge[gauge$density != 0.508,]
not508_avgGain <- sapply(unique(not508$density), FUN=function(x) { mean(not508[not508$density == x, "gain"]) })
not508_avgGauge <- data.frame(density=unique(not508$density), gain=not508_avgGain)
not508_avgGauge$logGain <- log(not508_avgGauge$gain)
not508_linearModel <- lm(density ~ logGain, data=not508_avgGauge)
print("predicted density, removing 0.508, for avg reading of 38.55")
print(predictDensity(not508_linearModel, data.frame(logGain=log(38.55))))

not001 <- gauge[gauge$density != 0.001,]
not001_avgGain <- sapply(unique(not001$density), FUN=function(x) { mean(not001[not001$density == x, "gain"]) })
not001_avgGauge <- data.frame(density=unique(not001$density), gain=not001_avgGain)
not001_avgGauge$logGain <- log(not001_avgGauge$gain)
not001_linearModel <- lm(density ~ logGain, data=not001_avgGauge)
print("predicted density, removing 0.001, for avg reading of 38.55")
print(predictDensity(not001_linearModel, data.frame(logGain=log(38.55))))

