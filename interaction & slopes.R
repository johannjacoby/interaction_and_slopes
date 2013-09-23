#get example data with predictors X1 and X2 and a dependent variable Y
ds <- as.data.frame(
	read.table(
		"https://raw.github.com/johannjacoby/interaction_and_slopes/master/no_interaction_differing_slopes.dat", 
		header=T, sep="\t"))

#center both predictors
ds$centered.X1 <- scale(ds$X1, center=T, scale=F)
ds$centered.X2 <- scale(ds$X2, center=T, scale=F)

#shift X2 to obtain the simple slope of X1 @ 1SD below the mean of X2
ds$centered.X2.lo <- ds$centered.X2 + sd(ds$X2)
#shift X2 to obtain the simple slope of X1 @ 1SD above the mean of X2
ds$centered.X2.hi <- ds$centered.X2 - sd(ds$X2)

#### yes, it is correct, to get ds$centered.X2.hi you have to <b>subtract</b> 1 SD, 
#### and in order to get ds$centered.X2.lo you have to <b>add</b> 1 SD.



# now estimate the basic regression model to see whether X1 and X2 interact:
model0 <- lm(Y~centered.X1*centered.X2, ds)
# and the two regression models in order to obtain the simple slopes of X1 at X2=1SD below the mean and at X2=1SD above the mean:
model.lo <- lm(Y~centered.X1*centered.X2.lo, ds)
model.hi <- lm(Y~centered.X1*centered.X2.hi, ds)

#show the models estimates
summary(model0); summary(model.lo); summary(model.hi)

# print the interaction and the simple slopes of X1 @ X2=-1SD and X2=+1SD
results0 <- summary(model0)[[4]]; results.lo <- summary(model.lo)[[4]]; results.hi <- summary(model0)[[4]]
cat("\n",
	"Interaction X1 * X2: b=",results0[4],", t=",results0[12],", p=",sprintf("%5.4f",results0[16]),ifelse(results0[16] < .05," *",""),"\n",
	"Slope of X1 @ X2 = -1SD: b=",results.lo[2],", t=",results.lo[10],", p=",sprintf("%5.4f",results.lo[14]),ifelse(results.lo[14] < .05," *",""),"\n",
	"Slope of X1 @ X2 = +1SD: b=",results.hi[2],", t=",results.hi[10],", p=",sprintf("%5.4f",results.hi[14]),ifelse(results.hi[14] < .05," *",""),"\n",
	sep="")

# simple slope of X1 @ X2 = mean
cat("Slope of X1 @ X2 = mean of X2: b=",results0[2],", t=",results0[10],", p=",sprintf("%5.4f",results0[14]),ifelse(results0[14] < .05," *",""),"\n")



